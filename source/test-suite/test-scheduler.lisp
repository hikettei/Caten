(in-package :caten/test-suite)
;; Tests caten/codegen/scheduler.lisp
(defun schedule-with-vars (&rest tensors)
  (ctx:with-contextvar (:JIT 0)
    (let ((avm (apply #'caten tensors)))
      (caten/codegen/shape-inference:run-type-infer avm)
      (caten/codegen/rewriting-rules:apply-rewriting-rules avm)
      (graph-schedule (avm-graph avm)))))

(defun gather-kernels (schedule-graph)
  (loop for node in (graph-nodes schedule-graph)
        if (getattr node :jitable)
          collect node))

(print (gather-kernels (schedule-with-vars (!rand `(3 3)))))
