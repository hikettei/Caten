(defpackage :caten/codegen/runner
  (:use :cl :caten/runtime :caten/air :caten/codegen/backend :caten/codegen/type-relay :caten/codegen/rewriting-rules
        :caten/codegen/scheduler :caten/common.logger :caten/codegen/blueprint)
  (:import-from :caten/codegen/search #:get-optimized-ast #:search-optimized-ast)
  (:import-from :caten/codegen/helpers #:coerce-dtyped-buffer))

(in-package :caten/codegen/runner)
;; ~~ Runner ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass AbstractKernel ()
  )

(defstruct (Compiled-Kernel)
  (name (error "name must occur") :type keyword)
  (caller (error "caller must occur") :type function)
  (raw-caller (error "raw-caller must occur") :type list)
  (device (error "device must occur") :type string)
  (code (error "code must occur") :type string)
  (out-positions (error "out positions must occur") :type list)
  (flops (error "flops must occur") :type GFlops-Measurer))

(defnode (:JIT :JIT_KERNEL) ()
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM."
	 :slots ((output-buffer-n :type fixnum) (kernel-info :type Compiled-Kernel) (dtypes :type list) (cached-p :type boolean)))

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
    (let ((prg-time (runtime-invoke-jit-kernel runtime info node args)))
      (declare (type float prg-time))
      (when (= (ctx:getenv :PROFILE) 1)
        (incf caten/runtime/profile::*jit-time* prg-time)
        (profile-report runtime info prg-time args node)))
    (apply #'values (map 'list #'(lambda (x) (nth x args)) (compiled-kernel-out-positions info)))))
