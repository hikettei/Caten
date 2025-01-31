(defpackage :caten/codegen/jit
  (:use :cl :caten/runtime :caten/air :caten/codegen/backend :caten/codegen/type-relay :caten/codegen/rewriting-rules)
  (:export
   #:codegen
   #:jit))

(in-package :caten/codegen/jit)

(defun codegen (runtime &key (backend (ctx:getenv :BACKEND)))
  "
```
(codegen runtime &key (backend (ctx:getenv :BACKEND)))
```
"
  (declare (type GraphRuntime runtime))
  ;; Get configurations for the backend
  (multiple-value-bind (buffer-type runtime-type renderer-type auto-scheduler is-jit) (get-backend-configs backend)
    (when (null is-jit) (setf (runtime-buffer-type runtime) buffer-type) (return-from codegen runtime))
    (when (eql backend :METAL)
      (assert (<= (ctx:getenv :PARALLEL) 1) () "METAL does not support parallel compilation. Set PARALLEL=0"))
    (when (= 2 (ctx:getenv :DOT)) (->dot (runtime-graph runtime) :title "Base Graph"))
    ;; Running shape inference
    (run-type-infer runtime)
    ;; Applying JIT Specific Graph Rewriting Rules in advance (e.g.: Propagete Views)
    (apply-rewriting-rules runtime)
    runtime
    ))
