(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config
        :caten/codegen/polyhedral :caten/codegen/transform)
  (:export
    #:auto-schedule
    #:Opt #:opt-id #:opt-amount #:apply-opt #:opt-applicable-p
    #:AutoScheduler #:autoscheduler-best-schedule #:autoscheduler-config
    #:autoscheduler-n-generation #:autoscheduler-gen2act
    #:NoOpt #:Parallel #:Global #:Local #:Interchange #:TileBand #:Unroll #:Packing
    #:get-possible-opts #:optimize-band #:minimize-cost #:compute-costs))

(in-package :caten/codegen/auto-scheduler)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Purpose: get a list of optimal scheduling commands
;; [TODO] JIT_DEBUG >= 2 to see optimized schedule sequence by BEAM (TODO: Searching method like tiramisu)
;; ~~~ Schedule Templates ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Parallelize == apply-tile 100あるループを4分割とかする
;; CPU=local_size(1, 1, 1)
;; GPU=local_size(32, 1, 1) global_size=(32, ...) -> gidx gidy gidz lidx lidy lid3
;; ^ i.e.: parallelize = Tiling with size
;; Implementation:
;; - [ ] Everything is TILE, find the best tile from polyhedron!
;; - [ ] ISL Schedule Nodeに対してBEAM Searchを実施する
;; - [ ] OPTIMIZE=2 will cache the optimized sequence of Opt
;; - [ ] GPU: gid.x * local_size + lid.xこれをFirst Class Supportにする
;; - [ ] (caten x :variables (list (variable 'b 0 10 2))) -> 全部に対してSampleする
;; - [ ] Unroll/Tiling/Parallelizeを全部Tileで実装する
;; - [ ] Loop Tilingした後にInterchangeができるようにScheduleする
;; - [ ]  TODO: Cache the result from OPTIMIZE=2
;; - [ ] ParallelLevel: ignore the count from visible-p=NIL
;; - [ ] Unroll_BODY without Unroll_PARENT -> Unroll away BODY
;; ~~~ Optimizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Opt () ((id :initarg :id :initform nil :accessor opt-id) (amount :initarg :amount :initform nil :accessor opt-amount)))
(defgeneric apply-opt (opt schedule-node item config) (:documentation "Returns a new isl:schedule-node with current optimization was applied."))
(defgeneric opt-applicable-p (opt schedule-node item config) (:documentation "Returns T if the current optimization is applicable to the given schedule-node"))
(defmethod print-object ((opt Opt) stream)
  (print-unreadable-object (opt stream :type t)
    (format stream ":id ~a :amount ~a" (opt-id opt) (opt-amount opt))))

(defclass NoOpt (Opt) nil (:documentation "Nothing applied."))
(defmethod apply-opt ((opt Noopt) schedule-node item config) (isl:copy schedule-node))
(defmethod opt-applicable-p ((opt Noopt) schedule-node item config) t)
;; Note: Parallel vs Global/Local is orthogonal
(defclass Parallel (Opt) nil (:documentation "Parallelizes the given schedule-node"))
(defmethod apply-opt ((opt Parallel) schedule-node item config) (apply-parallel schedule-node))
(defmethod opt-applicable-p ((opt Parallel) schedule-node item config)
  (and
   (< (isl:schedule-node-get-schedule-depth schedule-node) (auto-scheduler-n-global-loops config)) ;; Located in the parallel level?
   (check-legality-parallel schedule-node (poly-dependencies (getattr item :polyhedral)))))
;; TODO (Add Global/Local Support in Caten)
(defclass Global (Opt) nil) ;; blockIdx
(defclass Local (Opt) nil)  ;; threadIdx

(defclass Interchange (Opt) nil (:documentation "Swaps the loop with `amount` th band node in the current schedule."))
(defmethod apply-opt ((opt Interchange) schedule-node item config)
  (opt-applicable-p opt schedule-node item config))
(defmethod opt-applicable-p ((Opt Interchange) schedule-node item config)
  (apply-interchange (getattr item :polyhedral) schedule-node (opt-amount opt)))

(defclass TileBand (Opt) nil (:documentation "Tiles the given schedule-node-band with the size"))
(defmethod apply-opt ((opt TileBand) schedule-node item config) (apply-tile schedule-node (opt-amount opt)))
(defmethod opt-applicable-p ((opt TileBand) schedule-node item config)
  ;; [TODO] Only applicable after the band has a data reuse. but how do we know that?
  t)

(defclass Unroll (Opt) nil (:documentation "Unroll the loop with `amount`
```
for (int i=0; i<10; i++) {
   T1(i)
}
```
=>
```
for (int i=0; i<10; i+=amount) {
  T1(i)
  T2(i+1)
  ...
  T_amount(i+amount)
}
```"))
;; More chances to find vectorize by gcc, but takes longer compilation time and no improvements on the speed.
(defmethod apply-opt ((opt Unroll) schedule-node item config) (apply-unroll schedule-node (opt-amount opt)))
(defmethod opt-applicable-p ((opt Unroll) schedule-node item config) t)

(defclass Packing (Opt) nil (:documentation "Packs the task with `amount`.
```
for (int i=0; i<10; i++) {
   T1(i)
}
```
=>
```
for (int i=0; i<10; i+=amount) {
  T1_PACKED(T1(i+0), T1(i+1), ..., T1(i+amount))
}
```
"))
(defmethod apply-opt ((opt Packing) schedule-node item config) (apply-pack schedule-node (opt-amount opt)))
(defmethod opt-applicable-p ((opt Packing) schedule-node item config)
  ;; [TODO] Usually vectorize-level parallelism is located in the innnermost loop. how to judge this?
  t)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass AutoScheduler ()
  ((best-schedule :initarg :schedule :type isl:schedule-node-domain :accessor autoscheduler-best-schedule)
   (n-generation :initform 0 :accessor autoscheduler-n-generation)
   (gen2act :initform (make-hash-table) :accessor autoscheduler-gen2act)
   (config :initarg :config :type Auto-Scheduler-Config :accessor autoscheduler-config)))

(defgeneric compute-costs (auto-scheduler schedule-nodes item) (:documentation "This method receives a list of candidate schedules, returning a same-lengthed list whose elements are type of float. (the higher the better)"))

(defmethod get-possible-opts ((auto-scheduler AutoScheduler) schedule-node-band config &aux (actions))
  "Returns a list of possible optimization candidates."
  (push (make-instance 'NoOpt) actions)
  ;; Interchange
  (let ((undernearth-band-count
          (1- (length (schedule-node-get-undernearth-bands schedule-node-band)))))
    (dotimes (amount undernearth-band-count)
      (push (make-instance 'Interchange :amount amount) actions)))
  ;; Parallel Directive (Parallel or GLOBAL/LOCAL)
  (case (auto-scheduler-n-global-loops config)
    (0 nil)
    (1 (push (make-instance 'Parallel) actions))
    (3 ;; Block/Thread Parallelism
     (warn "TODO: Block/Thread Parallelism"))
    (otherwise
     (warn "Currently Caten does not support n_global_loops=~a and thus the code is not parallelized." (auto-scheduler-n-global-loops config))))
  ;; Tile
  (dolist (tile-size (auto-scheduler-tile-sizes config))
    (push (make-instance 'TileBand :amount tile-size) actions))
  ;; Pack(Vectorize)
  ;; [TODO] Feed the simd width by auto scheduler config
  (dolist (width `(4 8)) (push (make-instance 'Packing :amount width) actions))
  ;; Unroll
  ;; [TODO] Unroll_Limit=16?
  (push (make-instance 'Unroll :amount 16) actions)
  actions)
;; TODO: Stop Early Scalarify? NUMO cores would select the interchange of LOAD in gemm kernel
(defmethod optimize-band ((auto-scheduler AutoScheduler) schedule-node-band item)
  "Applies all possible optimizations to the given band, returning the best one."
  ;; Only interested in the schedule-node-band
  (unless (eql (isl:schedule-node-get-type schedule-node-band) :schedule-node-band)
    (return-from optimize-band schedule-node-band))
  (symbol-macrolet ((JIT_DEBUG (ctx:getenv :JIT_DEBUG)))
    (let* ((config (autoscheduler-config auto-scheduler))
           (next-actions
             (loop for opt in (get-possible-opts auto-scheduler schedule-node-band config)
                   if (opt-applicable-p opt schedule-node-band item config)
                     collect opt))
           (next-kernels (map 'list #'(lambda (x) (apply-opt x schedule-node-band item config)) next-actions))
           (sorted (sort (map 'list #'list (compute-costs auto-scheduler next-kernels item) next-kernels next-actions) #'> :key #'car)))
      (format t "~%DEBUG: Generation=~a:~%" (autoscheduler-n-generation auto-scheduler))
      (print next-actions)
      ;; Interchange: Scalar Loadの依存を壊さないか見る必要がある (壊したらapplicable-p=NIL)
      (dolist (k next-kernels) (print (render-schedule-node (isl:schedule-node-get-schedule k))))
      (format t "Selected:~%~a" (render-schedule-node (isl:schedule-node-get-schedule (second (car sorted)))))
      (setf (gethash (autoscheduler-n-generation auto-scheduler) (autoscheduler-gen2act auto-scheduler)) (third (car sorted)))
      (incf (autoscheduler-n-generation auto-scheduler))
      (second (car sorted)))))
;; [TODO] Cache or Trainingするために，Outputはsequence of Opt, Cacheできるようなデータ構造にする
(defmethod minimize-cost ((auto-scheduler AutoScheduler) item)
  (declare (optimize (speed 3)))
  (labels ((optimize-children (node &key (return-ancestor-p t))
             (setf node (optimize-band auto-scheduler node item))
             (let ((n-children (the fixnum (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))))
               (dotimes (nth n-children) (setf node (optimize-children (isl:schedule-node-get-child node nth))))
               (if return-ancestor-p
                   (isl:schedule-node-get-ancestor node 1)
                   node))))
    (optimize-children (autoscheduler-best-schedule auto-scheduler) :return-ancestor-p nil)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Reconfigurable AutoScheduler
;; - [ ] Minimize the actual execution time
;; - [ ] Minimize Proximity Coincidence
;; - [ ] RandomForest
;; - [ ] Rule Based
(defclass BogoScheduler (AutoScheduler) nil)
(defmethod compute-costs ((s BogoScheduler) schedule-nodes item) (loop repeat (length schedule-nodes) collect (random 1.0)))

(defclass ProfileScheduler (AutoScheduler) nil) ;; TODO

(defclass ILPScheduler (AutoScheduler) nil) ;; TODO

(defclass RandomForestScheduler (AutoScheduler) nil) ;; TODO
;; [TODO] Remember Mark
(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the (integer 0 2) (ctx:getenv :OPTIMIZE))))
    (when (= 0 OPTIMIZE) (return-from auto-schedule)) ;; No optimization
    ;; OPTIMIZE=1 : Parallel, Unroll, Vectorizeなど，ルールと優先度から
    ;; OPTIMIZE=2 : PROFILINGする，その代わりthreadbarrierなどを使える
    (when (>= OPTIMIZE 1)
      (let* ((strategy 'BogoScheduler) ;; TODO
             (auto-scheduler (make-instance strategy :schedule (isl:schedule-get-root (poly-schedule (getattr node :polyhedral))) :config auto-scheduler)))
        (minimize-cost auto-scheduler node)))
    ;; [TODO] BEAM report
    ;; n-trial
    ;; n-generation
    ;; found-sequence
    ;; total-time-consumed
    ;; Load blueprint from optimized polyhedral IR
    (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node))))
