(defpackage :caten/codegen/config
  (:use :cl)
  (:export
   #:Auto-Scheduler-Config
   #:define-auto-scheduler
   #:auto-scheduler-n-global-loops
   #:auto-scheduler-tile-sizes
   #:auto-scheduler-vectorizes))

(in-package :caten/codegen/config)

(defclass Auto-Scheduler-Config ()
  ((n-global-loops :type fixnum :accessor auto-scheduler-n-global-loops)
   (auto-scheduler-tile-sizes :type list :accessor auto-scheduler-tile-sizes)
   (auto-scheduler-vectorizes :type list :accessor auto-scheduler-vectorizes))
  (:documentation ""))

(defmethod print-object ((config Auto-Scheduler-Config) stream)
  (print-unreadable-object (config stream :type t)
    (format stream "  N-Global-Loops   | ~a |~%" (slot-value config 'n-global-loops))))

(defmacro define-auto-scheduler ((name (&rest args))
                                 &key
                                   (n-global-loop 0)
                                   (tile-sizes nil)
                                   (vectorizes nil)
                                   (documentation ""))
  "The macro define-auto-scheduler declares the configurations for the Caten Auto Scheduler.
TODO: Docs
"
  (let ((instance (gensym)))
    `(progn
       (defclass ,name (Auto-Scheduler-Config)
         nil
         (:documentation ,documentation))
       (defmethod initialize-instance :after ((,instance ,name) ,@(or args '(&key)))
         (setf
          (auto-scheduler-n-global-loops ,instance) ,n-global-loop
          (auto-scheduler-tile-sizes ,instance) ,tile-sizes
          (auto-scheduler-vectorizes ,instance) ,vectorizes)))))
