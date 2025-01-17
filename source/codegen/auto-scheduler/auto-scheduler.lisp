(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config
   :caten/codegen/polyhedral :caten/codegen/transform :caten/codegen/ast-parser)
  ;; Auto Scheduler
  (:export
    #:auto-schedule
    #:Opt #:opt-id #:opt-amount #:apply-opt #:opt-applicable-p
    #:AutoScheduler #:autoscheduler-best-schedule #:autoscheduler-config
    #:autoscheduler-n-generation #:autoscheduler-gen2act
    #:NoOpt #:Parallel #:Global #:Local #:Interchange #:TileBand #:Unroll #:Packing #:Coalesce
    #:get-possible-opts #:optimize-band #:minimize-cost #:compute-costs)
  ;; ILP Solvers
  (:export #:schedule-item-maximize-band-depth #:verify-schedule)
  ;; Manual Scheduler
  (:export
   #:si-apply-opt #:si-finalize-schedule #:with-manual-scheduler))

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
;; Note: Parallel vs Globalis orthogonal
(defclass Parallel (Opt) nil (:documentation "Parallelizes the given schedule-node"))
(defmethod apply-opt ((opt Parallel) schedule-node item config) (apply-parallel schedule-node))
(defmethod opt-applicable-p ((opt Parallel) schedule-node item config)
  ;; もう一個ある:
  ;; ParentがSequenceではないこと (for softmax ...)
  (and
   (< (isl:schedule-node-get-schedule-depth schedule-node) (auto-scheduler-n-global-loops config)) ;; Located in the parallel level?
   (check-legality-parallel schedule-node (poly-dependencies (getattr item :polyhedral)))))

(defclass Global (Parallel) nil) ;; blockIdx + threadIdx
(defmethod apply-opt ((opt Global) schedule-node item config) (apply-global (getattr item :polyhedral) schedule-node (opt-amount opt)))

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
(defmethod opt-applicable-p ((opt Packing) schedule-node item config) t)

(defclass Coalesce (Opt) nil)
(defmethod opt-applicable-p ((opt Coalesce) schedule-node item config) (apply-coalesce schedule-node))
(defmethod apply-opt ((opt Coalesce) schedule-node item config) (apply-coalesce schedule-node))
;; ~~ ILP Solver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod schedule-item-maximize-band-depth ((schedule-item Node))
  "Returns: (values new-schedule verified-p)"
  (assert (eql (node-type schedule-item) :Schedule-Item))
  (macrolet ((set-option (name level)
	       `(cffi:foreign-funcall
                 ,(format nil "isl_options_set_~(~a~)" name)
		 :pointer (isl::context-handle isl::*context*)
		 :int ,level
		 :void)))
    (set-option "schedule_treat_coalescing" 0)
    (set-option "schedule_outer_coincidence" 1)
    (set-option "schedule_maximize_band_depth" 0)
    (set-option "schedule_maximize_coincidence" 0)
    (set-option "schedule_whole_component" 0))
  (let* ((poly (getattr schedule-item :polyhedral))
         (sc (isl:schedule-constraints-on-domain (poly-domain poly)))
         (sc (isl:schedule-constraints-set-coincidence sc (poly-dependencies poly)))
         (sc (isl:schedule-constraints-set-proximity sc (poly-dependencies poly)))
         (sc (isl:schedule-constraints-set-validity sc (poly-dependencies poly)))
         (new-schedule (isl:schedule-constraints-compute-schedule sc)))
    (if (verify-schedule schedule-item new-schedule)
        (values new-schedule t)
        (values (poly-schedule poly) nil))))

(defmethod verify-schedule ((schedule-item Node) new-schedule)
  "Returns T if the new-schedule is verified as a new schedule.
A valid schedule is a schedule that satisfies the following conditions:
- Everything fits in a signel kernel in GPU. (i.e.: the root of schedule should not be sequence or set.
- Scalar variable dependencies are not broken.
For example, this is an invaild schedule sometime generated by the ISL scheduler:
```
if (i > 3) {
  float val_2 = 0.0;
}
val_2 = val_2 + 1.0;
```
This method ensures the generated ast is compiled without any errors, and fits in a single kernel in any case.
"
  (let* ((domain (isl:schedule-get-root new-schedule))
         (root (isl:schedule-node-get-child domain 0)))
    (when (find (isl:schedule-node-get-type root) `(:schedule-node-sequence :schedule-node-set))
      (return-from verify-schedule nil))
    (multiple-value-bind (valid-p fail-due-to)
        (caten/codegen/blueprint:verify-blueprint
         (caten/codegen/rewriting-rules:%schedule-item-write-define-global
          (lower-into-bp-from-polyhedral (->ast new-schedule 0) schedule-item)
          schedule-item))
      (values valid-p fail-due-to))))
;; ~~ Manual Scheduling Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun si-apply-opt (config schedule-item opt band only-visible)
  (declare (type node schedule-item) (type Opt opt) (type fixnum band))
  (assert (eql (node-type schedule-item) :Schedule-Item))
  (assert (getattr schedule-item :polyhedral) () "si-apply-opt: Run caten/codegen/scop:scop first to obtain Polyhedral IR!")
  (let* ((bands (map-schedule-nodes
                 #'(lambda (type node mark)
                     (when (eql type :schedule-node-band)
                       (when (or (null mark) (if only-visible (directive-visible mark) t))
                         node)))
                 (getattr schedule-item :polyhedral)))
         (tgt-band (nth band bands)))
    (assert tgt-band () "si-apply-opt: Band ~a not found in the schedule. n_bands=~a" band (length bands))
    (assert (opt-applicable-p opt tgt-band schedule-item config) () "si-apply-opt: The given opt is not applicable for the band. ~a" opt)
    (let ((new-sched (apply-opt opt tgt-band schedule-item config)))
      (setf (poly-schedule (getattr schedule-item :polyhedral)) (isl:schedule-node-get-schedule new-sched))
      t)))

(defun si-finalize-schedule (config schedule-item &key (n-optglobals 0))
  "Finalizes the blueprint modified in the Polyhedral IR Space."
  (declare (type node schedule-item))
  (assert (eql (node-type schedule-item) :Schedule-Item))
  (assert (getattr schedule-item :polyhedral) () "si-finalize-schedule: Run caten/codegen/scop:scop first to obtain Polyhedral IR!")
  (setf (getattr schedule-item :blueprint)
        (lower-into-bp-from-polyhedral
         (->ast
          (poly-schedule (getattr schedule-item :polyhedral))
          (getattr schedule-item :rank))
         schedule-item
         :vectorizes (auto-scheduler-vectorizes config)
         :n-global-offset n-optglobals)))

(defmacro with-manual-scheduler ((scheduled-item auto-scheduler) &body body)
  "A utility macro to write hand written scheduling commands to the given schedule-item, under the auto-scheduler.
This macro will bind the function `(opt opt band-idx)` locally, which will destructively apply the `opt` to the `band-idx`th band of the given schedule-node."
  (alexandria:with-gensyms (auto-scheduler-bind nglobal-bind)
    `(let ((,nglobal-bind 0) (,auto-scheduler-bind (make-instance ',auto-scheduler)))
       (caten/codegen/expr-cache:with-expr-cache ()
         (flet ((opt (opt band-idx &key (only-visible t))
                  (declare (type opt opt) (type fixnum band-idx))
                  (when (typep opt 'Global) (incf ,nglobal-bind))
                  (si-apply-opt ,auto-scheduler-bind ,scheduled-item opt band-idx only-visible)))
           (progn ,@body)
           (si-finalize-schedule ,auto-scheduler-bind ,scheduled-item :n-optglobals ,nglobal-bind))))))
;; ~~ Auto Scheduling Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  ;(let ((undernearth-band-count
  ;        (1- (length (schedule-node-get-undernearth-bands schedule-node-band)))))
  ;  (dotimes (amount undernearth-band-count)
  ;    (push (make-instance 'Interchange :amount amount) actions)))
  ;; Parallel Directive (Parallel or GLOBAL/LOCAL)
  (case (auto-scheduler-n-global-loops config)
    (0 nil)
    (1 (push (make-instance 'Parallel) actions))
    (3 ;; Block/Thread Parallelism
     (dolist (amt `(2 3 4 8 13 16 29))
       (push (make-instance 'Global :amount amt) actions)))
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
;; OPTIMIZE=1
(defmethod optimize-band-lv1 ((auto-scheduler AutoScheduler) schedule-node-band item prev-selected)
  ;; Only optimize schedule-node-band
  (unless (eql (isl:schedule-node-get-type schedule-node-band) :schedule-node-band)
    (return-from optimize-band-lv1 schedule-node-band))
  (flet ((tryit (opt)
           (when (and
                  (every #'(lambda (x) (not (typep opt (type-of x)))) prev-selected)
                  (opt-applicable-p opt schedule-node-band item (autoscheduler-config auto-scheduler)))
             (setf (gethash (autoscheduler-n-generation auto-scheduler) (autoscheduler-gen2act auto-scheduler)) opt)
             (incf (autoscheduler-n-generation auto-scheduler))
             (return-from optimize-band-lv1 (values (apply-opt opt schedule-node-band item (autoscheduler-config auto-scheduler)) opt)))))
    ;; [TODO] Make GLOBAL Tile
    ;; Hand-written optimization rules
    ;(case (auto-scheduler-n-global-loops (autoscheduler-config auto-scheduler))
    ;  (0 nil)
    ;  (1 (tryit (make-instance 'Parallel)))
    ;  (3 (tryit (make-instance 'Global :amount 1)))
    ;  (otherwise (warn "No Support for ~ad parallelism" (auto-scheduler-n-global-loops (autoscheduler-config auto-scheduler)))))
    ;; (tryit (make-instance 'Unroll :amount 4))
    ;; (tryit (make-instance 'Packing :amount 4)) float4 or vectorize
    schedule-node-band))
;; OPTIMIZE=2
(defmethod optimize-band-lv2 ((auto-scheduler AutoScheduler) schedule-node-band item prev-selected)
  "Applies all possible optimizations to the given band, returning the best one."
  ;; Only interested in the schedule-node-band
  (unless (eql (isl:schedule-node-get-type schedule-node-band) :schedule-node-band)
    (return-from optimize-band-lv2 schedule-node-band))
  (symbol-macrolet ((JIT_DEBUG (ctx:getenv :JIT_DEBUG)))
    (let* ((config (autoscheduler-config auto-scheduler))
           (next-actions
             (loop for opt in (get-possible-opts auto-scheduler schedule-node-band config)
                   if (and (opt-applicable-p opt schedule-node-band item config)
                           (every #'(lambda (x) (not (typep opt (type-of x)))) prev-selected))
                     collect opt))
           (next-kernels (map 'list #'(lambda (x) (apply-opt x schedule-node-band item config)) next-actions))
           (sorted (sort (map 'list #'list (compute-costs auto-scheduler next-kernels item) next-kernels next-actions) #'> :key #'car)))
      (when (null sorted) (return-from optimize-band-lv2 schedule-node-band))
      (format t "~%DEBUG: Generation=~a ============~%" (autoscheduler-n-generation auto-scheduler))
      ;; [TODO] How to verify the validity of loop interchange without providing scalar info?
       (dolist (k next-kernels) (print (render-schedule-node (isl:schedule-node-get-schedule k))))
      (format t "Selected:~%~a" (render-schedule-node (isl:schedule-node-get-schedule (second (car sorted)))))
      (setf (gethash (autoscheduler-n-generation auto-scheduler) (autoscheduler-gen2act auto-scheduler)) (third (car sorted)))
      (incf (autoscheduler-n-generation auto-scheduler))
      (values (second (car sorted)) (third (car sorted))))))

(defmethod minimize-cost ((auto-scheduler AutoScheduler) item optimizer)
  (labels ((get-absolute-pos (node history &aux (node (isl:schedule-get-root (isl:schedule-node-get-schedule node))))
             (dolist (h history)
               (setf node (isl:schedule-node-get-child node h)))
             node)
           (optimize-children (node &key (history) (prev-selected) &aux (depth (isl:schedule-node-get-schedule-depth node)))
             (multiple-value-bind (node selected) (funcall optimizer auto-scheduler node item prev-selected)
               (let ((generations (- (isl:schedule-node-get-schedule-depth node) depth)))
                 (dotimes (i generations) (setf history (append history (list 0)))))
               (let ((selected (if (and (or (null selected) (typep selected 'NoOpt)) (not (eql (isl:schedule-node-get-type node) :schedule-node-mark)))
                                   nil ;; move to next band
                                   (append prev-selected (list selected))))
                     (n-children (the fixnum (isl::%isl-schedule-node-n-children (isl::schedule-node-handle (get-absolute-pos node history))))))
                 (dotimes (nth n-children)
                   (setf node (optimize-children (isl:schedule-node-get-child (get-absolute-pos node history) nth) :prev-selected selected :history (append history (list nth)))))
                 node))))
    (isl:schedule-node-get-schedule (optimize-children (autoscheduler-best-schedule auto-scheduler)))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Make metrics configurable
(defclass BogoScheduler (AutoScheduler) nil)
(defmethod compute-costs ((s BogoScheduler) schedule-nodes item) (loop repeat (length schedule-nodes) collect (random 1.0)))

(defclass ProfileScheduler (AutoScheduler) nil) ;; TODO

(defclass ILPScheduler (AutoScheduler) nil) ;; TODO

(defclass RandomForestScheduler (AutoScheduler) nil) ;; TODO

(defclass LispScheduler (AutoScheduler) nil) ;; [Experimental] Execution time in lisp is propotional to the same time in gcc?

(defun schedule-node-get-bp (isl-schedule node)
  (lower-into-bp-from-polyhedral (->ast isl-schedule (getattr node :rank)) node))

(defun auto-schedule (auto-scheduler node)
  (assert (getattr node :polyhedral))
  ;; OPTIMIZE=0 | Applies no optimization
  ;; OPTIMIZE=1 | Rule based optimization without any speed measurements
  ;; OPTIMIZE=2 | BEAM Search based on cost models
  (symbol-macrolet ((OPTIMIZE (the integer (ctx:getenv :OPTIMIZE))))
    (when (= 0 OPTIMIZE) (return-from auto-schedule)) ;; No optimization
    ;; [TODO] BEAM Cache
    ;; - auto-schedule is running under OPTIMIZE=1 but found the previous result from OPTIMIZE=2
    ;; ==> Apply previous OPTIMIZE=2 result
    ;; - Cache is saved to the disk.
    (when (>= OPTIMIZE 1)
      (let* ((strategy 'BogoScheduler)
             (auto-scheduler (make-instance strategy :schedule (isl:schedule-get-root (poly-schedule (getattr node :polyhedral))) :config auto-scheduler))
             (new-schedule (minimize-cost auto-scheduler node (if (= OPTIMIZE 1) #'optimize-band-lv1 #'optimize-band-lv2)))
             (optglobals (loop for value in (alexandria:hash-table-values (autoscheduler-gen2act auto-scheduler))
                               if (typep value 'Opt)
                                 collect value)))
        (setf (poly-schedule (getattr node :polyhedral)) new-schedule)
        ;; (print "FINAL SCHEDULE")
        ;; (print (render-schedule-node new-schedule))
        ;; (caten/codegen/blueprint:print-blueprint (schedule-node-get-bp new-schedule node) t)
        ;; [TODO] BEAM Report with OPTIMIZE=1 and JIT_DEBUG=4
        ;; e.g.: n-trial, n-generation, found-opt-sequence, total-time-consumed
        ;; Load blueprint from optimized polyhedral IR
        (si-finalize-schedule (autoscheduler-config auto-scheduler) node :n-optglobals (1- (length optglobals)))))))
