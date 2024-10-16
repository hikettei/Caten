(defpackage :caten/codegen/jit
  (:documentation "This is an entry point for JIT. JIT compiles unoptimized AVM into optimized AVM.")
  (:use :cl)
  (:import-from
   :caten/avm
   #:AVM
   :avm-graph)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules)
  (:import-from
   :caten/codegen/scheduler
   #:graph-schedule)
  (:export
   #:jit))

(in-package :caten/codegen/jit)

(defun jit (avm)
  "Runs the JIT compilation (destructive)"
  (declare (type AVM avm))
  ;; 1. Running the type inference
  (run-type-infer avm)
  ;; 2. JIT Specific Simplifier
  (apply-rewriting-rules avm)
  ;; 3. Create a schedule
  (let ((schedules (graph-schedule (avm-graph avm))))
    ;; 4. Loop Bound Inference

    ;; (Rewrite Items belongs to the same loop are mutated into the same :EXPR)
    ;; :EXPR is also a node.

    ;; 5. (Optional) Further optimize each schedule by running polyhedral compiler

    ;; 6. Running memory-planner, update the storage-id

    ;; 7. Lower them into Render-Graph, and completed!
    ))
