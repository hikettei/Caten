(defpackage :caten/runtime/profile
  (:use :cl)
  (:export
   #:*jit-time*
   #:*vm-time*
   #:*allocate-time*
   #:with-real-time
   ))

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
