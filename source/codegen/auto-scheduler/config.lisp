(defpackage :caten/codegen/config
  (:use :cl)
  (:export
   #:Auto-Scheduler-Config
   #:define-auto-scheduler
   #:auto-scheduler-n-global-loops
   #:auto-scheduler-tile-size))
   
(in-package :caten/codegen/config)

(defclass Auto-Scheduler-Config ()
  ((n-global-loops :type fixnum :accessor auto-scheduler-n-global-loops)
   (auto-scheduler-tile-size :type fixnum :accessor auto-scheduler-tile-size))
  (:documentation ""))

(defmethod print-object ((config Auto-Scheduler-Config) stream)
  (print-unreadable-object (config stream :type t)
    (format stream "  N-Global-Loops   | ~a |~%" (slot-value config 'n-global-loops))))

(defmacro define-auto-scheduler ((name (&rest args))
                                 &key
                                   (n-global-loop 0)
                                   (tile-size 0)
                                   (documentation ""))
  "define-auto-scheduler"
  (let ((instance (gensym)))
    `(progn
       (defclass ,name (Auto-Scheduler-Config)
         nil
         (:documentation ,documentation))
       (defun ,name (,@args)
         (let ((,instance (make-instance ',name)))
           (setf
            (auto-scheduler-n-global-loops ,instance) ,n-global-loop
            (auto-scheduler-tile-size ,instance) ,tile-size)
           ,instance)))))
