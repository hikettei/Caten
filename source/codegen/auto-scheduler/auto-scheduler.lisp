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

(defclass Global (Parallel) nil) ;; blockIdx
(defmethod apply-opt ((opt Global) schedule-node item config) (apply-global schedule-node))

(defclass Local (Parallel) nil)  ;; threadIdx
(defmethod apply-opt ((opt Local) schedule-node item config) (apply-local schedule-node (opt-amount opt)))

(defclass Interchange (Opt) nil (:documentation "Swaps the loop with `amount` th band node in the current schedule."))
(defmethod apply-opt ((opt Interchange) schedule-node item config)
  (opt-applicable-p opt schedule-node item config))
(defmethod opt-applicable-p ((Opt Interchange) schedule-node item config)
  (apply-interchange (getattr item :polyhedral) schedule-node (opt-amount opt)))

(defclass TileBand (Opt) nil (:documentation "Tiles the given schedule-node-band with the size"))
(defmethod apply-opt ((opt TileBand) schedule-node item config) (apply-tile schedule-node (opt-amount opt)))
(defmethod opt-applicable-p ((opt TileBand) schedule-node item config)
  ;; [TODO] hand-written condition to know when the tiling is effective and limit the exploration space.
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

(defclass Coalesce (Opt) nil) ;; TODO
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass AutoScheduler ()
  ((best-schedule :initarg :schedule :type isl:schedule-node-domain :accessor autoscheduler-best-schedule)
   (n-generation :initform 0 :accessor autoscheduler-n-generation)
   (gen2act :initform (make-hash-table) :accessor autoscheduler-gen2act)
   (config :initarg :config :type Auto-Scheduler-Config :accessor autoscheduler-config)))

(defgeneric compute-costs (auto-scheduler schedule-nodes item) (:documentation "This method receives a list of candidate schedules, returning a same-lengthed list whose elements are type of float. (the higher the better)"))

(defmethod get-possible-opts ((auto-scheduler AutoScheduler) schedule-node-band config &aux (actions))
  "Returns a list of possible optimization candidates."
  (push (make-instance 'NoOpt) actions) ;; note: if you remove this line and all band is assigned any directive
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
     (push (make-instance 'Global) actions)
     (dolist (amt `(2 3 4 8 13 16 29))
       (push (make-instance 'Local :amount amt) actions)))
    (otherwise
     (warn "Currently Caten does not support n_global_loops=~a and thus the code is not parallelized." (auto-scheduler-n-global-loops config))))
  ;; Tile
  (dolist (tile-size (auto-scheduler-tile-sizes config)) (push (make-instance 'TileBand :amount tile-size) actions))
  ;; Pack (Vectorize)
  ;; [TODO] Feed the simd width by auto scheduler config
  (dolist (width `(4 8)) (push (make-instance 'Packing :amount width) actions))
  ;; Unroll
  ;; [TODO] Unroll_Limit=16?
  (push (make-instance 'Unroll :amount 16) actions)
  actions)
;; TODO: Stop Early Scalarify? NUMO cores would select the interchange of LOAD in gemm kernel
(defmethod optimize-band ((auto-scheduler AutoScheduler) schedule-node-band item prev-selected)
  "Applies all possible optimizations to the given band, returning the best one."
  ;; Only interested in the schedule-node-band
  (unless (eql (isl:schedule-node-get-type schedule-node-band) :schedule-node-band)
    (return-from optimize-band schedule-node-band))
  (symbol-macrolet ((JIT_DEBUG (ctx:getenv :JIT_DEBUG)))
    (let* ((config (autoscheduler-config auto-scheduler))
           (next-actions
             (loop for opt in (get-possible-opts auto-scheduler schedule-node-band config)
                   if (and (opt-applicable-p opt schedule-node-band item config)
                           (every #'(lambda (x) (not (typep opt (type-of x)))) prev-selected))
                     collect opt))
           (next-kernels (map 'list #'(lambda (x) (apply-opt x schedule-node-band item config)) next-actions))
           (sorted (sort (map 'list #'list (compute-costs auto-scheduler next-kernels item) next-kernels next-actions) #'> :key #'car)))
      (when (null sorted) (return-from optimize-band schedule-node-band))
      (format t "~%DEBUG: Generation=~a ============~%" (autoscheduler-n-generation auto-scheduler))
      ;; (print next-actions)
      ;; [TODO] How to verify the validity of loop interchange without providing scalar info?
       (dolist (k next-kernels) (print (render-schedule-node (isl:schedule-node-get-schedule k))))
      (format t "Selected:~%~a" (render-schedule-node (isl:schedule-node-get-schedule (second (car sorted)))))
      (setf (gethash (autoscheduler-n-generation auto-scheduler) (autoscheduler-gen2act auto-scheduler)) (third (car sorted)))
      (incf (autoscheduler-n-generation auto-scheduler))
      (values (second (car sorted)) (third (car sorted))))))

(defmethod minimize-cost ((auto-scheduler AutoScheduler) item)
  (labels ((get-absolute-pos (node history &aux (node (isl:schedule-get-root (isl:schedule-node-get-schedule node))))
             (dolist (h history)
               (setf node (isl:schedule-node-get-child node h)))
             node)
           (optimize-children (node &key (history) (prev-selected) &aux (depth (isl:schedule-node-get-schedule-depth node)))
             (multiple-value-bind (node selected) (optimize-band auto-scheduler node item prev-selected)
               (let ((generations (- (isl:schedule-node-get-schedule-depth node) depth)))
                 (dotimes (i generations) (setf history (append history (list 0)))))
               (let ((selected (if (and (or (null selected) (typep selected 'NoOpt)) (not (eql (isl:schedule-node-get-type node) :schedule-node-mark)))
                                   nil ;; move to next band
                                   (append prev-selected (list selected))))
                     (n-children (the fixnum (isl::%isl-schedule-node-n-children (isl::schedule-node-handle (get-absolute-pos node history))))))
                 (dotimes (nth n-children)
                   (setf node (optimize-children (isl:schedule-node-get-child (get-absolute-pos node history) nth) :prev-selected selected :history (append history (list nth)))))
                 node))))
    (optimize-children (autoscheduler-best-schedule auto-scheduler))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Make metrics configurable
(defclass BogoScheduler (AutoScheduler) nil)
(defmethod compute-costs ((s BogoScheduler) schedule-nodes item) (loop repeat (length schedule-nodes) collect (random 1.0)))

(defclass ProfileScheduler (AutoScheduler) nil) ;; TODO

(defclass ILPScheduler (AutoScheduler) nil) ;; TODO

(defclass RandomForestScheduler (AutoScheduler) nil) ;; TODO

(defclass LispScheduler (AutoScheduler) nil) ;; [Experimental] Execution time in lisp is propotional to the same time in gcc?

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  (symbol-macrolet ((OPTIMIZE (the integer (ctx:getenv :OPTIMIZE))))
    (when (= 0 OPTIMIZE) (return-from auto-schedule)) ;; No optimization
    ;; OPTIMIZE=1 : Parallel, Unroll, Vectorize, GLOCAL, LOCAL
    ;; OPTIMIZE=2 : Interchange, Tile(Interchange is required!), GROUPTOP
    (when (>= OPTIMIZE 2)
      (warn "OPTIMIZE=2 is still under development. Do not use this.")
      (let* ((strategy 'BogoScheduler) ;; TODO: Configurable
             (auto-scheduler (make-instance strategy :schedule (isl:schedule-get-root (poly-schedule (getattr node :polyhedral))) :config auto-scheduler)))
        (minimize-cost auto-scheduler node)))
    ;; [TODO] BEAM report
    ;; n-trial
    ;; n-generation
    ;; found-sequence
    ;; total-time-consumed
    ;; Load blueprint from optimized polyhedral IR
    (setf (getattr node :blueprint) (caten/codegen/ast-parser:lower-into-bp-from-polyhedral (caten/codegen/polyhedral:->ast (getattr node :polyhedral) (getattr node :rank)) node))))
