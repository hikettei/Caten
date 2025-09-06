(defpackage :caten/runtime/kernel
  (:use :cl)
  (:export
   #:Kernel
   #:kernel-name
   #:kernel-program
   #:%kernel-write-program

   #:kernel-load-blueprint
   #:kernel-compile
   #:kernel-launch
   #:make-kernel))

(in-package :caten/runtime/kernel)

(defclass Kernel ()
  ((name :initarg :name :reader kernel-name)
   (program :initarg :program :reader kernel-program :writer %kernel-write-program)))

(defgeneric kernel-load-blueprint (kernel blueprint))
(defgeneric kernel-compile (kernel))
(defgeneric kernel-launch (kernel runtime args))
  
(defun make-kernel (name blueprint)
  (kernel-load-blueprint (make-instance 'Kernel :name name) blueprint))
