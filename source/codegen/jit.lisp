(defpackage :caten/codegen/jit
  (:documentation "This is an entry point for JIT. JIT compiles unoptimized AVM into optimized AVM.")
  (:use :cl)
  (:import-from
   :caten/avm
   #:AVM
   #:avm-graph
   #:avm-name)
  (:import-from
   :caten/air
   #:Graph
   #:graph-nodes
   #:node-type
   #:getattr)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer
   #:graph-infer-iteration-space)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules)
  (:import-from
   :caten/codegen/scheduler
   #:graph-schedule)
  (:import-from
   :caten/codegen/blueprint
   #:lower-schedule-item
   #:print-blueprint)
  (:import-from
   :caten/codegen/scop
   #:scop
   #:auto-schedule)
  (:import-from
   :caten/common.logger
   #:print-info
   #:with-progress
   #:print-progress)
  (:import-from
   :caten/codegen/expr-cache
   #:with-expr-cache)
  (:export
   #:jit))

(in-package :caten/codegen/jit)

;; [Milestone]
;; - [ ] schedule-graph -> Better Graph Splitting Strategy?
;; - [ ] Fuse Permute
;; - [ ] Scop+Polyhedral -> Can ISL find the optimal embedding kernel?
;; - [ ] Support Backward (Higher Order?)
;; - [ ] TODO Purge unnecessary stride computation (in symbolic!)
(defun jit (avm)
  "Runs the JIT compilation (destructive)"
  (declare (type AVM avm))
  ;; 1. Running the type inference
  (run-type-infer avm)
  ;; 2. JIT Specific Simplifier (Loop Collapse is here)
  (apply-rewriting-rules avm)
  ;; 3. Merge dims (e.g. (10 10 10) Tensor -> (10x10x10) Tensor)
  (graph-infer-iteration-space (avm-graph avm))
  ;; 4. Schedule
  (let ((schedule-graph (graph-schedule (avm-graph avm)))
        (symbolics
          (remove-duplicates
           (loop for node in (graph-nodes (avm-graph avm))
                 if (and (eql (node-type node) :LOAD) (symbolp (getattr node :value)))
                   collect (getattr node :value)))))
    (declare (type Graph schedule-graph))
    ;; 5. Loop Bound Inference (i.e.: OP -> Loop For transformation)))
    
    ;; [TODO] Identified kernels are not compiled
    ;; COMPILE_SPEED=0 ()
    ;; COMPILE_SPEED=1 ()
    ;; COMPILE_SOEED=2 (Full Symbolic Compilation)
    ;; Impl: Static Gensymをもう一度適用してノード比較
    (let ((total-kernels (count-if #'(lambda (x) (getattr x :jitable)) (graph-nodes schedule-graph))))
      (when (>= (ctx:getenv :JIT_DEBUG) 2)
        (print-info "JIT Compilation Start (AVM=~a)" (avm-name avm)))
      (with-progress (total-kernels :debug (if (>= (ctx:getenv :JIT_DEBUG) 2) 1 -1) :timeit nil)
        (with-expr-cache () ;; Initialize a cache to treat (EXPR: a*b) as a symbolic and make symbolic collapsed loops as an affine loop.
          (mapc
           #'(lambda (x &aux (start (get-internal-real-time)))
               (when (getattr x :jitable)
                 ;; [TODO] (skipped) if cached
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (print-progress "~a" (getattr x :name))
                   (format t "=====> Lowering to blueprint~%"))
                 ;; [TODO] Debug Info (Compilation time, Function Name, etc...)
                 (lower-schedule-item x (avm-graph avm) schedule-graph)
                 ;; 6. Lower into Polyhedral IR
                 (when (and (>= (ctx:getenv :JIT_DEBUG) 2) (null (getattr x :auto-schedule-p)) (>= (ctx:getenv :AUTO_SCHEDULER) 1))
                   (format t "=====> Skipping Auto Scheduler (Symbolic incremental or scalar kernel)~%"))
                 (when (and (>= (ctx:getenv :AUTO_SCHEDULER) 1) (getattr x :auto-schedule-p))
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (format t "=====> Lowering to Polyhedral IR~%"))
                   (scop x symbolics)
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (format t "=====> Auto Scheduler~%"))
                   ;; Optimizing: Tiles, Parallelizing, Vectorizing, Unrolling
                   (auto-schedule x)
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (print (getattr x :polyhedral))
                     (fresh-line)
                     (format t "=====> Optimized kernel~%")
                     (print-blueprint (getattr x :blueprint) t)))
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (format t "Compilation Time : ~A(sec)" (float (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))
           (reverse (graph-nodes schedule-graph))))))
    ;; 7. Running memory-planner, update the storage-id
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Running the memory planner..."))
    ;; (memory-planner)
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Rendering ..."))
    ;; 8. Complete
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Compiling ..."))
    t))

;; Test Case1
;; (with-no-grad (caten/codegen:jit (caten (call (Transformer 64 4 1 1e-5 32) (make-tensor `(10 30)) (iconst 0)))))
;; Test Case2
;; (caten (!randn `(a b)))

;; memo: Transformer Model Initialization is slow.

