(defpackage :caten/codegen/auto-scheduler
  (:use :cl :caten/air :caten/codegen/shape-inference :caten/codegen/expr :caten/codegen/config
   :caten/codegen/polyhedral :caten/codegen/transform :caten/codegen/ast-parser)
  ;; Auto Scheduler
  (:export
    #:auto-schedule
    #:Opt #:opt-id #:opt-amount #:apply-opt #:opt-applicable-p
    #:AutoScheduler #:autoscheduler-best-schedule #:autoscheduler-config
    #:autoscheduler-n-generation #:autoscheduler-gen2act
    #:NoOpt #:Parallel #:Global #:Local #:TileBand #:Packing #:Coalesce #:Reschedule
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

(defclass Parallel (Opt)
  ((use-legality-parallel :initarg :use-legality-parallel :initform nil :accessor parallel-use-legality-parallel))
  (:documentation "Parallelizes the given schedule-node"))
(defmethod apply-opt ((opt Parallel) schedule-node item config) (apply-parallel schedule-node))
(defmethod opt-applicable-p ((opt Parallel) schedule-node item config)
  (when (parallel-use-legality-parallel opt)
    (assert (every #'null (isl:schedule-node-band-get-coincident schedule-node)) () ":use-legality-parallel is not applicable if the given schedule-node has coincident.")
    (return-from opt-applicable-p (check-legality-parallel schedule-node (poly-dependencies (getattr item :polyhedral)))))
  (let ((coincident (isl:schedule-node-band-get-coincident schedule-node)))
    (and (> (length coincident) 0) (car coincident))))

(defclass Global (Opt)
  ;; blockIdx + threadIdx
  ((use-legality-parallel :initarg :use-legality-parallel :initform nil :accessor global-use-legality-parallel)))

(defmethod apply-opt ((opt Global) schedule-node item config)
  (apply-global (getattr item :polyhedral) schedule-node (opt-amount opt)))

(defmethod opt-applicable-p ((opt Global) schedule-node item config)
  (assert (listp (opt-amount opt)) () "Global amount must be given as a list.")
  (when (global-use-legality-parallel opt)
    (assert (every #'null (isl:schedule-node-band-get-coincident schedule-node)) () ":use-legality-parallel is not applicable if the given schedule-node has coincident.")
    (return-from opt-applicable-p (check-legality-parallel schedule-node (poly-dependencies (getattr item :polyhedral)))))
  (let* ((coincident
           (isl:schedule-node-band-get-coincident schedule-node))
         (split-at
           (or (position-if #'null coincident) (length coincident))))
    ;; e.g.: coincident=(T T NIL) <=> split the band at pos=2
    (<= (length (opt-amount opt)) split-at)))

(defclass TileBand (Opt) ((only-coincident :initarg :only-coincident :initform nil :accessor tile-only-coincident))
  (:documentation "Tiles the given schedule-node-band with the size"))
(defmethod apply-opt ((opt TileBand) schedule-node item config) (apply-tile schedule-node (opt-amount opt)))
(defmethod opt-applicable-p ((opt TileBand) schedule-node item config)
  (or
   (null (tile-only-coincident opt))
   (let* ((coincident (isl::schedule-node-band-get-coincident schedule-node))
          (split-at (or (position-if #'null coincident) (length coincident))))
     (<= (length (opt-amount opt)) split-at))))

(defclass Prefetch (Opt) nil (:documentation "Inserts a trigger for transfering the data from global memory to (aligned) shared memory."))
(defmethod apply-opt ((opt Prefetch) schedule-node item config) (apply-and-insert-prefetch schedule-node (opt-amount opt)))
(defmethod opt-applicable-p ((opt Prefetch) schedule-node item config) t)

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

(defun si-finalize-schedule (config schedule-item)
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
         :vectorizes (auto-scheduler-vectorizes config))))

(defmacro with-manual-scheduler ((scheduled-item auto-scheduler) &body body)
  "A utility macro to write hand written scheduling commands to the given schedule-item, under the auto-scheduler.
This macro will bind the function `(opt opt band-idx)` locally, which will destructively apply the `opt` to the `band-idx`th band of the given schedule-node."
  (alexandria:with-gensyms (auto-scheduler-bind)
    `(let ((,auto-scheduler-bind (make-instance ',auto-scheduler)))
       (caten/codegen/expr-cache:with-expr-cache ()
         (flet ((opt (opt band-idx &key (only-visible t))
                  (declare (type opt opt) (type fixnum band-idx))
                  (si-apply-opt ,auto-scheduler-bind ,scheduled-item opt band-idx only-visible)))
           (progn ,@body)
           (si-finalize-schedule ,auto-scheduler-bind ,scheduled-item))))))
;; ~~ Sketch Generation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Sketch (:constructor make-sketch (schedule &key (opt-history nil))))
  "Sketch records the history of the applied optimization, and the schedule status."
  (schedule schedule :type isl:schedule)
  (opt-history opt-history :type list) ;; a list of (cons band-idx opt)
  (search-params nil :type list))

;; Note:
;; (defstruct (Sketch-Param (:constructor make-sketch-param ())))
;; TILE_SIZE_USE = PACK_SIZE * TILE_SIZE

(defmethod print-object ((sketch Sketch) stream)
  (print-unreadable-object (sketch stream :type t :identity t)
    (format stream "~%  ```~%~a  ```~%  :search-params ~a~%  :opt-history ~a~%"
            (apply #'concatenate 'string
                   (loop for line in (cl-ppcre:split "\\n" (caten/codegen/polyhedral:render-schedule-node (sketch-schedule sketch)))
                         collect (format nil "  ~a~%" line)))
            (sketch-search-params sketch)
            (sketch-opt-history sketch))))

(defmethod sketch-get-bands ((sketch sketch))
  (map-schedule-node-children
   #'(lambda (type node mark)
       (when (eql type :schedule-node-band)
         node))
   (isl:schedule-get-root (sketch-schedule sketch))))

(defmethod sketch-get-band-depth-list ((sketch sketch))
  (map 'list #'isl:schedule-node-get-schedule-depth (sketch-get-bands sketch)))

(defmethod sketch-get-band-at-depth ((sketch sketch) depth allow-marked)
  (map-schedule-node-children
   #'(lambda (type node mark)
       (when (and (or allow-marked (null mark)) (eql type :schedule-node-band) (= (isl:schedule-node-get-schedule-depth node) depth))
         (return-from sketch-get-band-at-depth node)))
   (isl:schedule-get-root (sketch-schedule sketch))))

(defmethod sketch-get-band ((sketch sketch) idx)
  (nth idx (sketch-get-bands sketch)))

(defmethod sketch-n-band ((sketch sketch))
  (length (sketch-get-bands sketch)))

(defmethod sketch-next-generation ((sketch Sketch) (opt Opt) idx band item config)
  (let* ((new-sketch (make-sketch (isl:schedule-node-get-schedule (apply-opt opt band item config)))))
    (setf (sketch-opt-history new-sketch) (append (list (cons idx opt)) (sketch-opt-history sketch)))
    new-sketch))

(defun generate-sketch (item config &aux (sketches) (blocksize 128))
  "Returns a new schedule which is called a sketch, a template kernel that is further optimized by the auto-scheduler.
The sketch is equivalent to the decision tree where each node corresponds for whether an optimization is applied or not.
See also : `docs/assets/Caten_Sketch_Generation.jpg`
(values sketch-schedule (list params-to-search))"
  (declare (type node item))
  (assert (eql (node-type item) :Schedule-Item))
  ;; Generating the root of the sketch. (Solve ILP or not to maximize the band depth)
  ;; [TODO] Solve ILP Succeed one is always better?
  (let ((reschedule (make-instance 'Reschedule)))
    (if (opt-applicable-p reschedule nil item config) ;; (note) 99% of the computation time consists Reschedule in generate-sketch
        (push (make-sketch (isl:schedule-node-get-schedule (apply-opt reschedule nil item config)) :opt-history (list (cons 0 reschedule))) sketches)
        (push (make-sketch (poly-schedule (getattr item :polyhedral))) sketches)))
  ;; --------------------------------------------------------------------------
  ;; all bands and mapping each band to parallelism:
  ;; --------------------------------------------------------------------------
  ;; 1. Parallelize the outermost band. |  Params to optimize
  ;; - PARALLEL (if N_GLOBAL_DIMS=1)    |      nothing
  ;; - GLOBAL   (if N_GLOBAL_DIMS>1)    |   Local_Size (which is a tile dim)
  ;; --------------------------------------------------------------------------
  ;; [TODO] Use GLOBAL COALESCE
  (let ((n-global-dims (auto-scheduler-n-global-loops config)))
    (loop for sketch in sketches
          for target-band = (nth 0 (sketch-get-bands sketch))
          for use-legality-parallel = (null (sketch-opt-history sketch)) ;; If Reschedule is not selected, ISL Schedule coincident is not assigned. (Use check-legality-parallel instead)
          for band-depth = (and target-band (schedule-node-band-get-depth target-band))
          when target-band do
            (case n-global-dims
              (1
               (let ((opt (make-instance 'Parallel :use-legality-parallel use-legality-parallel)))
                 (when (opt-applicable-p opt target-band item config)
                   (setf sketches (remove sketch sketches))
                   (push (sketch-next-generation sketch opt 0 target-band item config) sketches))))
              (otherwise
               ;; The smaller (- n-global-dims parallelized-dims), the better.
               ;; Restrict the search space by only pushing the best one.
               (loop named global
                     for idx in (map 'list #'1+ (nreverse (alexandria:iota (min band-depth n-global-dims))))
                     for local-size = (loop repeat idx collect 8) ;; [TODO] Make it symbolic!
                     for opt = (make-instance 'Global :amount local-size :use-legality-parallel use-legality-parallel) do
                       (when (and local-size (opt-applicable-p opt target-band item config))
                         (setf sketches (remove sketch sketches))
                         ;; [TODO] ND Global has a chance to be transformed as: USE_KWARP_GLOBAL
                         (push (sketch-next-generation sketch opt 0 target-band item config) sketches)
                         ;; Exit the generation as soon as the best one is found.
                         (return-from global)))))))
  ;; --------------------------------------------------------------------------
  ;; 2. Tile all bands with BLOCKSIZE to find the memory locality.
  ;; - Inserts Shared Memory Transfer Points.
  ;; - Innermost reductions have a chance to be rewritten as warpReduce.
  ;; (For 1d parallelism) Tiling the outermost band.
  (case (auto-scheduler-n-global-loops config)
    ((0 1)
     ;; TODO Only coincident are tileable
     (loop for sketch in sketches
           for target-band = (nth 0 (sketch-get-bands sketch))
           for band-depth = (and target-band (schedule-node-band-get-depth target-band))
           for has-coincidence = (and (find 'Reschedule (sketch-opt-history sketch) :key #'cdr :test #'(lambda (x y) (typep y x))) t)
           when target-band do
             (loop named tile
                   for idx in (map 'list #'1+ (nreverse (alexandria:iota (min band-depth 3))))
                   for tile-size = (loop repeat idx collect blocksize)
                   for opt = (make-instance 'TileBand :amount tile-size :only-coincident has-coincidence) do
                     (when (and tile-size (opt-applicable-p opt target-band item config))
                       ;; Branching the sketch
                       (setf sketches (remove sketch sketches))
                       (push (sketch-next-generation sketch opt 0 target-band item config) sketches)
                       (return-from tile))))))
  ;; Also the loop is tiled in the innermost depth.
  (loop for sketch in sketches
        for band-depth-list = (sketch-get-band-depth-list sketch)
        for innermost = (reduce #'max band-depth-list) do
          (loop for tgt-band = (sketch-get-band-at-depth sketch innermost nil)
                while tgt-band
                for tile-size = (list blocksize) ;; TODO
                for opt = (make-instance 'Prefetch :amount tile-size) do
                  (when (and tile-size (opt-applicable-p opt tgt-band item config))
                    (setf sketches (remove sketch sketches)
                          sketch (sketch-next-generation sketch opt 0 tgt-band item config))
                    (push sketch sketches)))
          ;; [TODO] ここがReductionだった場合，Shared Memory or warpReduce, which one would be beneficial?
          (when (> (count innermost band-depth-list) 1)
            ;; Intra Tile Fusion (Optimization for Softmax/LayerNorm)
            ;; Finding a pattern such like:
            ;; (sequence)
            ;;   L c2 @DIRECTIVE(PREFETCH_OUTER)
            ;;     L c3 @DIRECTIVE(PREFETCH_INNER)
            ;;   L c2 @DIRECTIVE(PREFETCH_OUTER)
            ;;     L c3 @DIRECTIVE(PREFETCH_INNER)
            ;;   ...
            ;; - 1. Inserts the first tile(c2) @DIRECTIVE(LOCAL)
            ;; - 2. Inserts the second tile(c2) @DIRECTIVE(BARRIER)
            ;; For GPU, this is the equivalent to doing warpReduce. (For example, SumReduce/MaxReduce in Softmax is further utilizes the thread level parallelism)
            ;; For CPU, this is the equivalent to the intra tile fusion and utilizes simd register by transferring arrays into aligned memory.
            (apply-intra-tile-fusion (sketch-get-band-at-depth sketch innermost t) innermost)
            ))
  ;; [TODO]
  ;; All of those optimization covers 90% of required optimization decision tree for CPU/GPU.
  
  ;; --------------------------------------------------------------------------
  ;; 3. Further tile the tiled ALU ops to use SIMD
  ;;      Opt       |    Applicable to       | Target to optimize
  ;; - Loop Tiling  |       all bands        |    tiling size
  ;; - Loop Packing | filters with stride=1  |  nothing(constant)
  ;; [TODO] All bands with the incremental is still set to 1 should be further tiled by the SIMD UPCAST
  ;; Producing a vectorizable/upcastable dims
  ;; [TODO] Remove This code!!!
  ;; - We dont have to tile all dims
  ;; - The child of PREFETCH_OUTER is a SIMD.
  (when nil
    ;; SIMD/UPCAST
    (loop for sketch in sketches
          for sketch-first = sketch
          for bands = (sketch-get-bands sketch)
          for innermost-depth = (reduce #'max (map 'list #'isl:schedule-node-get-schedule-depth bands)) do
            ;; Exploration space is restricted to depth < innsermost-depth ?
            (flet ((get-bands (sketch)
                     (map-schedule-node-children
                      #'(lambda (type node mark)
                          (when (and
                                 (eql type :schedule-node-band)
                                 (or (null mark) (equalp (directive-type mark) "LOCAL") (equalp (directive-type mark) "PREFETCH_INNER"))
                                 ;; (>= (isl:schedule-node-get-schedule-depth node) innermost-depth)
                                 )
                            node))
                      (isl:schedule-get-root (sketch-schedule sketch)))))
              ;; Upcast all bands
              (loop for bands = (get-bands sketch)
                    for band = (car bands)
                    for band-depth = (and band (schedule-node-band-get-depth band))
                    while band do
                      (let ((opt (make-instance 'Packing :amount (loop repeat band-depth collect 4)))) ;; [TODO] Tune this parameter!
                        (when (opt-applicable-p opt band item config)
                          (setf sketch (sketch-next-generation sketch opt 0 band item config)))))
              (setf sketches (remove sketch-first sketches))
              (push sketch sketches))))
  ;; - Collapse:   (CPU Only) Merge PARALLEL+TILE into a single band.
  ;; --------------------------------------------------------------------------
  ;; (TODO) Sketch Generation is applicable to TPU/NPU in the future.
  ;; - Add custrom constraints that are specified by define-auto-scheduler.
  ;; --------------------------------------------------------------------------
  ;; ===> A sketch is returned. also here is a list of symbols to be optimized.

  ;; [TODO]: Verify-blueprint通らないやつは削除，OPTIMIZE=1用でSortする:
  ;; [TODO]: 実際に実行しなくても，Scheduleの優劣でSortすることは可能:
  ;; A. Has Reschedule = 10000 points (always reschedule to the higher dimensional)
  ;; B. Has Tensorcore = 1000 points
  ;; Score = (A.) + (B.) + Volume(PACKED_REGION)

  ;; Workload(次にやるべきこと)
  ;; - Microkernel Transformationを完成させる
  ;;   - Finish: ASTParser
  ;;     - [ ] Finish: Vectorize(gemm8x8 mutation)
  ;;     - [ ] Refactor Vectorizer

  ;; - Improve the sketch generation


  ;; TILE_DIMS=64, PACK_DIMS=32みたいなのを仮定する
  
  ;; - Trade-Offになる箇所でSketchを分岐する
  ;; - Verify-Blueprintで先に枝刈りする
  ;; - Tile dimensionをSymbolicにする
  ;; - AST Parse, Vectorizeを全て実装する
  ;; - Sort by the score
  ;; - Memory Coalescing
  ;; - PARALLEL+TILE Fusion in the CPU
  ;; - Shared Memory Transfer Optimization
  ;; - Search with Full Symbolic
  ;; - Sketchって 40通りくらい生成されない？
  ;; - optimize local size
  ;; - ILP: Give LOCAL_SIZE <= TILE_SIZE ... 

  ;; [TODO] (ctx:getenv :SKETCH_SAMPLE_TOP_K)
  ;; Usually <= 3

  ;; [TODO]
  ;; To maximize the performance, it is equivalent to solve ILP against:
  ;; (LocalSize1 LocalSize2 LocalSize3)
  ;; TILE_SIZE
  ;; PACK_SIZE
  ;; cost = Volume(Vectorized_Size) ?
  ;;(dolist (s sketches)
  ;;  (PRINT "=== SAMPLE ===")
  ;;  (caten/codegen/blueprint:print-blueprint (lower-into-bp-from-polyhedral (->ast (sketch-schedule s) 0) item) t))
  sketches)
;; TODO: (defstruct search-space (ls1 ls2 ls3 tile-size pack-size))
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
        (setf (poly-schedule (getattr node :polyhedral)) (sketch-schedule (car sketch)))
        ;; Load blueprint from optimized polyhedral IR
        (si-finalize-schedule auto-scheduler node)))))
