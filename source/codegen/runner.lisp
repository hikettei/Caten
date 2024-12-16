(defpackage :caten/codegen/runner
  (:use :cl :caten/codegen/jit :caten/codegen/helpers :caten/air))

(in-package :caten/codegen/runner)
;; JIT-Compiled Kernel Runner
(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((info (getattr node :kernel-info)))
    (let ((args (map 'list #'coerce-dtyped-buffer args (getattr node :dtypes))))
      (assert (functionp (compiled-kernel-caller info)) () "Could not find the function caller for the node ~a" node)
      (apply (compiled-kernel-caller info) args)
      (apply #'values (map 'list #'(lambda (x) (nth x args)) (compiled-kernel-out-positions info))))))
