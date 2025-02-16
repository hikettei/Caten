(defpackage :caten/codegen/runner
  (:use :cl :caten/runtime :caten/air :caten/codegen/backend :caten/codegen/type-relay :caten/codegen/rewriting-rules
        :caten/codegen/scheduler :caten/common.logger :caten/codegen/blueprint)
  (:import-from :caten/codegen/search #:get-optimized-ast #:search-optimized-ast)
  (:import-from :caten/codegen/helpers #:coerce-dtyped-buffer)
  (:export
   #:AbstractKernel #:invoke-kernel #:profile-report
   #:kernel-name #:kernel-device #:kernel-schedule-item #:kernel-flops #:kernel-output-buffers))

(in-package :caten/codegen/runner)
;; ~~ Runner ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defnode (:JIT :JIT_KERNEL) ()
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM."
         :slots ((kernel :type AbstractKernel)))

(defclass AbstractKernel ()
  ((name :initarg :name :accessor kernel-name)
   (device :initarg :device :accessor kernel-device)
   (schedule-item :initarg :schedule-item :accessor kernel-schedule-item)
   (flops :initarg :flops :accessor kernel-flops)
   (output-buffers :initarg :output-buffers :accessor kernel-output-buffers)))

(defgeneric invoke-kernel (kernel runtime node args)
  (:documentation "Invokes the kernel, returning the elapsed time."))

(defun profile-report (runtime info elapsed-time args node)
  (let* ((flops (compiled-kernel-flops info))
         (gflops (ctx:with-contextvar (:PROFILE 0) (compute-gflops flops elapsed-time (map 'list #'cons (node-reads node) args)))))
    (format t "~a |  KERNEL  | ~,6fs | ~a| ~a[~a] ~a~%"
            (caten/runtime/profile:render-runtime-position runtime)
            elapsed-time
            (with-output-to-string (out)
              (loop for x in args
                    if (buffer-p x) do (format out "~a " (buffer-shape x))))
            (kernel-device info)
            (kernel-name info)
            (if gflops (format nil " (~,6fGFLOP/s)" gflops) ""))))

(defmethod realize-node ((node-id (eql :JIT_KERNEL)) runtime node args)
  (let ((info (getattr node :kernel))
        ;;(args (map 'list #'coerce-dtyped-buffer args (getattr node :dtypes)))
        )
    (let ((prg-time (invoke-kernel info runtime node args)))
      (declare (type float prg-time))
      (when (= (ctx:getenv :PROFILE) 1)
        (incf caten/runtime/profile::*jit-time* prg-time)
        (profile-report runtime info prg-time args node)))
    (apply #'values (map 'list #'(lambda (x) (nth x args)) (kernel-output-buffers info)))))
