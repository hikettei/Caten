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
  ;; 2. JIT Specific Simplifier (Loop Collapse is here)
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

;; Test Case1
;; (with-no-grad (caten/codegen:jit (caten (call (Transformer 64 4 1 1e-5 32) (make-tensor `(10 30)) (iconst 0)))))
;; Test Case2
;; (caten (!randn `(a b)))

;; memo: Transformer Model Initialization is slow.
;; [IDEA} Alternatively, prepare an API to lower from loop structure directly (like TensorComprehension)
;; (with-no-grad
;;   (caten/codegen:jit (caten (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25))))))
;; TransposeやConvなど: Transpose, Move+Move+Moveなどを以下に同じループで実行するかが大事
;; カーネル内部に登場するShapeの種類を数える
;; 最大RankにPaddingして，1はAnythingとして，remove-duplicatesをして治るなら同一のカーネル
;; 同一カーネル内部でLoop Collapse
;; or, Loop Collapse in advance?
;; From Collapsed -> Complicated is wasy
;; From Complicated -> Collapsed is difficult
