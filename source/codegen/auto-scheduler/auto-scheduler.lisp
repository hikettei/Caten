(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config
   :caten/codegen/polyhedral :caten/codegen/transform :caten/codegen/ast-parser)
  ;; Auto Scheduler
  (:export
    #:auto-schedule
    #:Opt #:opt-id #:opt-amount #:apply-opt #:opt-applicable-p
    #:AutoScheduler #:autoscheduler-best-schedule #:autoscheduler-config
    #:autoscheduler-n-generation #:autoscheduler-gen2act
    #:NoOpt #:Parallel #:Global #:Local #:TileBand #:Unroll #:Packing #:Coalesce
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

(defclass Reschedule (Opt) ((new-sched :initform nil :accessor opt-reschedule-new-sched))
  (:documentation "Solves ILP to maximize the band depth. (Note: this will reset all of the optimization history. If inserted, that should be the top of the optimization tree!)"))

(defmethod apply-opt ((opt Reschedule) schedule-node item config)
  (assert (opt-reschedule-new-sched opt) () "Run (opt-applicable-p reschedule ...) first!")
  (isl:schedule-get-root (opt-reschedule-new-sched opt)))

(defmethod opt-applicable-p ((opt Reschedule) schedule-node item config)
  (assert (null (opt-amount opt)))
  (let ((new-schedule (schedule-item-maximize-band-depth item)))
    (setf (opt-reschedule-new-sched opt) new-schedule)
    (verify-schedule item new-schedule)))

(defclass Parallel (Opt) nil (:documentation "Parallelizes the given schedule-node"))
(defmethod apply-opt ((opt Parallel) schedule-node item config) (apply-parallel schedule-node))
(defmethod opt-applicable-p ((opt Parallel) schedule-node item config)
  
  ;;(print (check-legality-parallel schedule-node (poly-dependencies (getattr item :polyhedral))))
  t)

(defclass Global (Opt) nil) ;; blockIdx + threadIdx
(defmethod apply-opt ((opt Global) schedule-node item config)
  (apply-global (getattr item :polyhedral) schedule-node (opt-amount opt)))

(defmethod opt-applicable-p ((opt Global) schedule-node item config)
  (assert (listp (opt-amount opt)) () "Global amount must be given as a list.")
  (let* ((coincident
           (isl:schedule-node-band-get-coincident schedule-node))
         (split-at
           (or (position-if #'null coincident) (length coincident))))
    ;; e.g.: coincident=(T T NIL) <=> split the band at pos=2
    (and
     (<= (length (opt-amount opt)) split-at))))

(defclass TileBand (Opt) nil (:documentation "Tiles the given schedule-node-band with the size"))
(defmethod apply-opt ((opt TileBand) schedule-node item config) (apply-tile schedule-node (opt-amount opt)))
(defmethod opt-applicable-p ((opt TileBand) schedule-node item config) t)

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
;; TODO: GLOBAL MEMORY CACHE
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
    (set-option "schedule_outer_coincidence" 0)
    (set-option "schedule_maximize_band_depth" 0)
    (set-option "schedule_maximize_coincidence" 0)
    (set-option "schedule_serialize_sccs" 0)
    (set-option "schedule_whole_component" 0))
  (let* ((poly (getattr schedule-item :polyhedral))
         (sc (isl:schedule-constraints-on-domain (poly-domain poly)))
         (sc (isl:schedule-constraints-set-coincidence sc (poly-dependencies poly)))
         (sc (isl:schedule-constraints-set-proximity sc (poly-dependencies poly)))
         (sc (isl:schedule-constraints-set-validity sc (poly-dependencies poly)))
         (new-schedule (isl:schedule-constraints-compute-schedule sc)))
    new-schedule))

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
;; ~~ Sketch Generation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Sketch (:constructor make-sketch (schedule &key (opt-history nil))))
  "Sketch records the history of the applied optimization, and the schedule status."
  (schedule schedule :type isl:schedule)
  (opt-history opt-history :type list) ;; a list of (cons band-idx opt)
  (search-params nil :type list)
  (pioneer 0 :type fixnum))

(defmethod print-object ((sketch Sketch) stream)
  (print-unreadable-object (sketch stream :type t :identity t)
    (format stream "~%  ```~%~a  ```~%  :search-params ~a~%  :pioneer ~a~%  :opt-history ~a~%"
            (apply #'concatenate 'string
                   (loop for line in (cl-ppcre:split "\\n" (caten/codegen/polyhedral:render-schedule-node (sketch-schedule sketch)))
                         collect (format nil "  ~a~%" line)))
            (sketch-search-params sketch)
            (sketch-pioneer sketch)
            (sketch-opt-history sketch))))

(defmethod sketch-get-bands ((sketch sketch))
  (map-schedule-node-children
   #'(lambda (type node mark)
       (when (eql type :schedule-node-band)
         (when (or (null mark) (directive-visible mark))
           node)))
   (isl:schedule-get-root (sketch-schedule sketch))))

(defmethod sketch-get-band ((sketch sketch) idx)
  (nth idx (sketch-get-bands sketch)))

(defmethod sketch-n-band ((sketch sketch))
  (length (sketch-get-bands sketch)))

(defmethod sketch-next-generation ((sketch Sketch) (opt Opt) idx band item config)
  (let* ((new-sketch (make-sketch (isl:schedule-node-get-schedule (apply-opt opt (sketch-get-band sketch idx) item config))))
         (diff (- (sketch-n-band new-sketch) (sketch-n-band sketch))))
    (setf (sketch-opt-history new-sketch) (append (list (cons idx opt)) (sketch-opt-history sketch))
          (sketch-pioneer new-sketch) (+ 1 diff idx)) ;; the next optimization starts with idx
    new-sketch))

(defun generate-sketch (item config &aux (sketches))
  "Returns a new schedule which is called a sketch, a template kernel that is further optimized by the auto-scheduler.
See also : `docs/assets/Caten_Sketch_Generation.jpg`
(values sketch-schedule (list params-to-search))"
  (declare (type node item))
  (assert (eql (node-type item) :Schedule-Item))
  ;; Generating the root of the sketch. (Solve ILP or not to maximize the band depth)

  ;; [TODO] Solve ILP Succeed one is always better?
  (push (make-sketch (poly-schedule (getattr item :polyhedral))) sketches)
  (let ((reschedule (make-instance 'Reschedule)))
    (when (opt-applicable-p reschedule nil item config)
      (push (make-sketch (isl:schedule-node-get-schedule (apply-opt reschedule nil item config)) :opt-history (list (cons 0 reschedule))) sketches)))
  ;; --------------------------------------------------------------------------
  ;; Tile all bands and mapping each band to parallelism:
  ;; --------------------------------------------------------------------------
  ;; 1. Parallelize the outermost band.    Target to optimize
  ;; - PARALLEL (if N_GLOBAL_DIMS=1)    |      nothing
  ;; - GLOBAL   (if N_GLOBAL_DIMS>1)    |     Local_Size (which is a tile dim)
  ;; --------------------------------------------------------------------------
  
  ;; 作業中ノート: 休憩したらPARALLEL/GLOBALに手をつける
  ;; [TODO] SketchのScheduleは複数になる。最後はSortして選択
  ;; Tweak GLOBAL: 一番外側のBAND_DEPTHだけPARALLEL/GLOBALになる
  ;; [TODO] Coincidentを取得するが
  ;; - ISL Scheduleが失敗した場合: 
  ;; - ISL Scheduleが成功した場合: 
  (let ((n-global-dims (auto-scheduler-n-global-loops config)))
    ;; [TODO] Also candidates: Where to split the band?
    (loop for sketch in sketches
          for target-band = (nth 0 (sketch-get-bands sketch))
          when target-band do
            (case n-global-dims
              (1 )
              (otherwise
               ;; The smaller (- n-global-dims parallelized-dims), the better.
               ;; Restrict the search space by only pushing the best one.
               (loop named global
                     for idx in (nreverse (alexandria:iota n-global-dims))
                     for local-size = (loop repeat idx collect 4) ;; [TODO] Make it symbolic!
                     for opt = (make-instance 'Global :amount local-size) do
                       (when (and local-size (opt-applicable-p opt target-band item config))
                         (push (sketch-next-generation sketch opt 0 target-band item config) sketches)
                         ;; Exit the generation as soon as the best one is found.
                         (return-from global)))))))
                     
  ;; @DIRECTIVEについて: デフォルトでリストにする。
  ;; 2D Warp has a chance to get parallelized
  
  ;; --------------------------------------------------------------------------
  ;; 2. Mapping inner bands with the following optimizations:
  ;;      Opt       |    Applicable to       | Target to optimize
  ;; - Loop Tiling  |       all bands        |    tiling size
  ;; - Loop Packing | filters with stride=1  |  nothing(constant)

  ;; 作業中ノート:
  ;; Tile/Packの判定をどうするか？
  
  ;; --------------------------------------------------------------------------
  ;; 3. Some minor optimizations:
  ;; - Unroll:     Remove away small loops by unrolling with loop size.
  ;; - Coalescing: Optimize the memory access pattern by transforming global.
  ;; - Collapse:   (CPU Only) Merge PARALLEL+TILE into a single band.
  ;; - Shared Memory Transfer:
  ;; - etc ... (User defined via with-manual-scheduling)

  ;; --------------------------------------------------------------------------
  ;; (TODO) Sketch Generation is applicable to TPU/NPU in the future.
  ;; - Add custrom constraints that are specified by define-auto-scheduler.
  ;; --------------------------------------------------------------------------
  ;; ===> A sketch is returned. also here is a list of symbols to be optimized.
  sketches)

;; ~~ Entry Point ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun schedule-node-get-bp (isl-schedule node)
  (lower-into-bp-from-polyhedral (->ast isl-schedule (getattr node :rank)) node))

(defun auto-schedule (auto-scheduler node)
  "An entry point for the auto scheduler."
  (assert (getattr node :polyhedral))
  ;;   LEVEL    |    Description
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
      (let* ((sketch (generate-sketch node auto-scheduler)))
        ;; Load blueprint from optimized polyhedral IR
        (si-finalize-schedule auto-scheduler node)))))
