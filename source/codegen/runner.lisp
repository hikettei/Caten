(defpackage :caten/codegen/runner
  (:documentation "Provides the jit-compiled kernel runner and benchmarking tools.")
  (:use :cl :caten/codegen/jit :caten/codegen/helpers :caten/air :caten/avm))

(in-package :caten/codegen/runner)
;; [TODO] caten/avmと共通化した方がいい
(defmacro with-real-time (&body body)
  (alexandria:with-gensyms (start-time end-time)
    `(let ((,start-time (get-internal-real-time)))
       (progn ,@body)
       (let ((,end-time (get-internal-real-time)))
         (float (/ (- ,end-time ,start-time) internal-time-units-per-second))))))

(defun digits (pc total)
  (format nil "t=~a~a" pc (make-string (abs (- (length (princ-to-string total)) (length (princ-to-string pc)))) :initial-element #\space)))
;; [TODO] TOTAL KERNEL EXECUTION TIME
;; [TODO] VM KERNEL EXECUTION TIME 両方をProfileする
;; AllocationとかもReportする
;; [TODO] compiled-kernel-flop
(defun profile-report (info elapsed-time)
  ;; [TODO] 桁数を固定する
  (format t "~a | ~a | [~a]~a~%"
          (digits (avm-pc *vm*) (avm-tape-length *vm*))
          elapsed-time
          (compiled-kernel-device info)
          (compiled-kernel-name info)))

(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((info (getattr node :kernel-info)))
    (let ((args (map 'list #'coerce-dtyped-buffer args (getattr node :dtypes))))
      (assert (functionp (compiled-kernel-caller info)) () "Could not find the function caller for the node ~a" node)
      (let ((prg-time (with-real-time (apply (compiled-kernel-caller info) args))))
        (when (= (ctx:getenv :PROFILE) 1)
          (profile-report info prg-time)))
      (apply #'values (map 'list #'(lambda (x) (nth x args)) (compiled-kernel-out-positions info))))))
