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
