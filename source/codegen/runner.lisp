(defpackage :caten/codegen/runner
  (:documentation "Provides the jit-compiled kernel runner and benchmarking tools.")
  (:use :cl :caten/codegen/jit :caten/codegen/helpers :caten/air :caten/runtime))

(in-package :caten/codegen/runner)

(defun profile-report (runtime info elapsed-time args)
  (format t "~a |  KERNEL  | ~,6fs | ~a| ~a | ~a~%" ;; [TODO] Compute GFLOPS
          (caten/runtime/profile:render-runtime-position runtime)
          elapsed-time
          (with-output-to-string (out)
            (loop for x in args
                  if (buffer-p x) do (format out "~a " (buffer-shape x))))
          (compiled-kernel-device info)
          (compiled-kernel-name info)))

(defmethod realize-node ((node-id (eql :JIT_KERNEL)) runtime node args)
  (let ((info (getattr node :kernel-info))
        (args (map 'list #'coerce-dtyped-buffer args (getattr node :dtypes))))
    (assert (functionp (compiled-kernel-caller info)) () "Could not find the function caller for the node ~a" node)
    (let ((prg-time (caten/runtime/profile:with-real-time (apply (compiled-kernel-caller info) args))))
      (when (= (ctx:getenv :PROFILE) 1)
        (incf caten/runtime/profile::*jit-time* prg-time)
        (profile-report runtime info prg-time args)))
    (apply #'values (map 'list #'(lambda (x) (nth x args)) (compiled-kernel-out-positions info)))))
