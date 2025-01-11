(defpackage :caten/codegen/runner
  (:documentation "Provides the jit-compiled kernel runner and benchmarking tools.")
  (:use :cl :caten/codegen/jit :caten/codegen/helpers :caten/air :caten/runtime :caten/codegen/blueprint))

(in-package :caten/codegen/runner)

(defun profile-report (runtime info elapsed-time args node)
  (let* ((flops (compiled-kernel-flops info))
         (gflops (ctx:with-contextvar (:PROFILE 0) (compute-gflops flops elapsed-time (map 'list #'cons (node-reads node) args)))))
    (format t "~a |  KERNEL  | ~,6fs | ~a| ~a[~a] ~a~%"
            (caten/runtime/profile:render-runtime-position runtime)
            elapsed-time
            (with-output-to-string (out)
              (loop for x in args
                    if (buffer-p x) do (format out "~a " (buffer-shape x))))
            (compiled-kernel-device info)
            (compiled-kernel-name info)
            (if gflops (format nil " (~,6fGFLOP/s)" gflops) ""))))

(defmethod runtime-invoke-jit-kernel ((runtime GraphRuntime) kernel-info node args)
  (apply (compiled-kernel-caller kernel-info) args))

(defmethod realize-node ((node-id (eql :JIT_KERNEL)) runtime node args)
  (let ((info (getattr node :kernel-info))
        (args (map 'list #'coerce-dtyped-buffer args (getattr node :dtypes))))
    (assert (functionp (compiled-kernel-caller info)) () "Could not find the function caller for the node ~a" node)
    ;; [TODO] Create a common way to profile the kernel execution time for async and sync kernels.
    (let ((prg-time (caten/runtime/profile:with-real-time (runtime-invoke-jit-kernel runtime info node args))))
      (when (= (ctx:getenv :PROFILE) 1)
        (incf caten/runtime/profile::*jit-time* prg-time)
        (profile-report runtime info prg-time args node)))
    (apply #'values (map 'list #'(lambda (x) (nth x args)) (compiled-kernel-out-positions info)))))
