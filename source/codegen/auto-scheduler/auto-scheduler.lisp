(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config
        :caten/codegen/polyhedral :caten/codegen/transform)
  (:export
    #:auto-schedule
    #:Opt #:opt-id #:opt-amount #:apply-opt #:opt-applicable-p
    #:AutoScheduler #:autoscheduler-best-schedule #:autoscheduler-config
    #:get-possible-opts))

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
;; TODO: Cache the result from OPTIMIZE=2
;; BEAM Search: ISL Schedule Treeで実施する
;; Tiling+Interchangeは必須 (Tile Dimsを一番下に移動したい。。。)
;; remove tiling, unroll, coincidence -> transform.lisp
;; ~~~ Optimizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Opt () ((id :initarg :id :initform nil :accessor opt-id) (amount :initarg :amount :initform nil :accessor opt-amount)))
(defgeneric apply-opt (opt schedule-node item config) (:documentation "Returns a new isl:schedule-node with current optimization was applied."))
(defgeneric opt-applicable-p (opt schedule-node item config) (:documentation "Returns T if the current optimization is applicable to the given schedule-node"))
(defmethod print-object ((opt Opt) stream)
  (print-unreadable-object (opt stream :type t)
    (format stream ":id ~a :amount ~a" (opt-id opt) (opt-amount opt))))

(defclass NoOpt (Opt) nil (:documentation "Nothing applied"))
(defmethod apply-opt ((opt Noopt) schedule-node item config) (isl:copy schedule-node))
(defmethod opt-applicable-p ((opt Noopt) schedule-node item config) t)

(defclass Parallel (Opt) nil (:documentation "Tiles the current schedule-band by amount"))
(defmethod apply-opt ((opt Parallel) schedule-node item config)
  
  )
(defmethod opt-applicable-p ((opt Parallel) schedule-node item config)
  (check-legality-parallel schedule-node (poly-dependencies (getattr item :polyhedral))))

(defclass Interchange (Opt) nil (:documentation "Swaps the loop with `amount` th band node in the current schedule."))
(defmethod apply-opt ((opt Interchange) schedule-node item config)
  (opt-applicable-p opt schedule-node item config))
(defmethod opt-applicable-p ((Opt Interchange) schedule-node item config)
  (apply-interchange (getattr item :polyhedral) schedule-node (opt-amount opt)))

(defclass TileBand (Opt) nil (:documentation "Tiles the given schedule-node-band with the size"))
(defmethod apply-opt ((opt TileBand) schedule-node item config)
  
  )
(defmethod opt-applicable-p ((opt TileBand) schedule-node item config)
  ;; TODO: How to judge has data reuse?
  ;; TODO: get loop size?
  t)

(defclass Unroll (Opt) nil (:documentation "Unrolls the loop with amount"))
(defclass Packing (Opt) nil (:documentation "Packs the array with amount"))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass AutoScheduler ()
  ((best-schedule :initarg :schedule :type isl:schedule-node-domain :accessor autoscheduler-best-schedule)
   (config :initarg :config :type Auto-Scheduler-Config :accessor autoscheduler-config)))

(defgeneric cost (auto-scheduler schedule-nodes))

(defmethod get-possible-opts ((auto-scheduler AutoScheduler) schedule-node-band &aux (actions))
  "Returns a list of possible optimization candidates."
  (push (make-instance 'NoOpt) actions)
  ;; Interchange
  (let ((undernearth-band-count
          (1- (length (schedule-node-get-undernearth-bands schedule-node-band)))))
    (dotimes (amount undernearth-band-count)
      (push (make-instance 'Interchange :amount amount) actions)))
  actions)

(defmethod optimize-band ((auto-scheduler AutoScheduler) schedule-node-band item)
  "Applies all possible optimizations to the given band, returning the best one."
  ;; Only interested in the schedule-node-band
  (unless (eql (isl:schedule-node-get-type schedule-node-band) :schedule-node-band)
    (return-from optimize-band schedule-node-band))
  ;; 無限ループしないように注意？
  (let* ((config (autoscheduler-config auto-scheduler))
         (next-actions
           (loop for opt in (get-possible-opts auto-scheduler schedule-node-band)
                 if (opt-applicable-p opt schedule-node-band item config)
                   collect opt))
         (next-kernels (map 'list #'(lambda (x) (apply-opt x schedule-node-band item config)) next-actions)))
    (print "=====GENERATION=============")
    (print schedule-node-band)
    (print next-actions)
    ;; Interchange: Scalar Loadの依存を壊さないか見る必要がある (壊したらapplicable-p=NIL)
    (dolist (k next-kernels) (print (render-schedule-node (isl:schedule-node-get-schedule k))))
    schedule-node-band))
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
;; [TODO] Reconfigurable AutoScheduler
;; - [ ] Minimize the actual execution time
;; - [ ] Minimize Proximity Coincidence
;; - [ ] RandomForest
;; - [ ] Rule Based
(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the (integer 0 2) (ctx:getenv :OPTIMIZE))))
    (when (= 0 OPTIMIZE) (return-from auto-schedule)) ;; No optimization
    ;; どっちもBEAM Search but...
    ;; OPTIMIZE=1 : Parallel, Unroll, Vectorizeなど，ルールと優先度から
    ;; OPTIMIZE=2 : PROFILINGする，その代わりthreadbarrierなどを使える
    (when (>= OPTIMIZE 1)
      (let ((auto-scheduler (make-instance 'AutoScheduler :schedule (isl:schedule-get-root (poly-schedule (getattr node :polyhedral))) :config auto-scheduler)))
        (minimize-cost auto-scheduler node)))
    ;; [TODO] Final BEAM Search for local/global size, or tiling size.
    ;; Load blueprint from optimized polyhedral IR
    (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node))))
