(defpackage :caten/runtime/profile
  (:use :cl)
  (:export
   #:*jit-time*
   #:*vm-time*
   #:*allocate-time*
   #:with-real-time
   #:render-runtime-position
   #:report-allocation
   #:start-profile
   #:report-profile-result))

(in-package :caten/runtime/profile)

(defvar *jit-time*)
(defvar *vm-time*)
(defvar *allocate-time*)

(defmacro with-real-time (&body body)
  (alexandria:with-gensyms (start-time end-time)
    `(let ((,start-time (get-internal-real-time)))
       (progn ,@body)
       (let ((,end-time (get-internal-real-time)))
         (float (/ (- ,end-time ,start-time) internal-time-units-per-second))))))

(defun render-runtime-position (runtime &aux (pc (uiop:symbol-call :caten/runtime/runtime :runtime-pc runtime)) (total (uiop:symbol-call :caten/runtime/runtime :runtime-tape-length runtime)))
  (format nil " [~a]~a" pc (make-string (abs (- (length (princ-to-string total)) (length (princ-to-string pc)))) :initial-element #\space)))

(defun report-allocation (runtime skipped-p dtype shape)
  (when (= 1 (ctx:getenv :PROFILE))
    (format t "~a | ~a | ~,4f MB | ~a ~a~%" (render-runtime-position runtime) (if skipped-p "  LOAD  " "ALLOCATE") (* (* (caten/common.dtype:dtype/size-of dtype) (apply #'* shape)) 1e-6) shape dtype)))

(defun start-profile (&key (width 80))
  (when (= 1 (ctx:getenv :PROFILE))
    (format t "~%~a~%PROFILE=1:~%" (make-string width :initial-element #\=))))

(defun report-profile-result (&key (width 80))
  (when (= 1 (ctx:getenv :PROFILE))
    (format t "~a~%Total: KERNEL=~as, VM=~as, ALLOCATE=~as Σ=~as~%~a~%" (make-string width :initial-element #\=) *jit-time* *vm-time* *allocate-time* (+ *jit-time* *vm-time* *allocate-time*) (make-string width :initial-element #\=))))
