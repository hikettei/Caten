(defpackage :caten/polyhedral/config
  (:use :cl :cffi)
  (:export
   #:Schedule-Options
   #:make-schedule-options
   #:apply-schedule-options-global
   #:define-auto-scheduler
   ))

(in-package :caten/polyhedral/config)

(macrolet ((define-schedule-option (&rest options-and-default &aux (maxlen (apply #'max (map 'list (alexandria:compose #'length #'car) options-and-default))))
             (flet ((lispify (name)
                      (string-upcase (cl-ppcre:regex-replace-all "_" name "-")))
                    (align (string &key (offset 1))
                      (with-output-to-string (out)
                        (format out "~a" string)
                        (dotimes (i (max 0 (+ offset (- maxlen (length string)))))
                          (princ " " out)))))
               `(progn
                  (defclass Schedule-Options ()
                    ,(loop for (name . default) in options-and-default
                           for lisp-name = (lispify name)
                           collect `(,(intern lisp-name) :initarg ,(intern lisp-name "KEYWORD") :initform ,default))
                    (:documentation ""))
                  (defun make-schedule-options (&key ,@(loop for (name . default) in options-and-default
                                                            for lisp-name = (intern (lispify name))
                                                            collect `(,lisp-name ,default)))
                    (make-instance 'Schedule-Options
                                   ,@(loop for (name . default) in options-and-default
                                           for lisp-name = (lispify name)
                                           append `(,(intern lisp-name "KEYWORD") ,(intern lisp-name)))))
                  (defmethod print-object ((config Schedule-Options) stream)
                    (print-unreadable-object (config stream :type t)
                      (format stream "~%")
                      ,@(loop for (name . default) in options-and-default
                              for lisp-name = (intern (lispify name))
                              collect `(format stream "  ~a | ~a |~%" ,(align name) (slot-value config ',lisp-name)))))
                  (defmethod apply-schedule-options-global((config Schedule-Options))
                    ,@(loop for (name . default) in options-and-default
                            for lisp-name = (intern (lispify name))
                            collect `(foreign-funcall ,(format nil "isl_options_set_~(~a~)" name) :pointer (isl::context-handle isl::*context*) :int (slot-value config ',lisp-name) :void)))))))
  (define-schedule-option
    ("schedule_serialize_sccs" . 0)
    ("schedule_outer_coincidence" . 0)
    ("schedule_maximize_coincidence" . 1)
    ("schedule_treat_coalescing" . 1)
    ("schedule_maximize_band_depth" . 1)
    ("schedule_whole_component" . 0)))

(defclass Auto-Scheduler-Config ()
  ((schedule-options :type Schedule-Options :accessor auto-scheduler-schedule-options)
   (cost-functions :type list :accessor auto-scheduler-cost-functions)
   (n-global-loops :type fixnum :accessor auto-scheduler-n-global-loops))
  (:documentation ""))

(defmethod print-object ((config Auto-Scheduler-Config) stream)
  (print-unreadable-object (config stream :type t)
    (format stream "~%~a~%" (slot-value config 'schedule-options))
    (format stream "  Cost-Functions   | ~a |~%" (slot-value config 'cost-functions))
    (format stream "  N-Global-Loops   | ~a |~%" (slot-value config 'n-global-loops))))

(deftype cost-function-t () `(member :proximity :coincidence :validity))

(defmacro define-auto-scheduler ((name (&rest args))
                                 &key
                                   (schedule-option `(make-schedule-options))
                                   (cost-functions '(:proximity :coincidence :validity))
                                   (n-global-loop 0)
                                   (documentation ""))
  "define-auto-scheduler"
  (let ((instance (gensym)) (cs (gensym)))
    `(progn
       (defclass ,name (Auto-Scheduler-Config)
         nil
         (:documentation ,documentation))
       (defun ,name (,@args)
         (let ((,instance (make-instance ',name))
               (,cs ,cost-functions))
           (assert (and (listp ,cs) (every #'(lambda (x) (typep x 'cost-function-t)) ,cs)) () "Cost functions must be a list of :proximity, :coincidence, or :validity, getting ~a" ,cs)
           (setf (auto-scheduler-schedule-options ,instance) ,schedule-option
                 (auto-scheduler-cost-functions ,instance) ,cost-functions
                 (auto-scheduler-n-global-loops ,instance) ,n-global-loop)
           ,instance)))))
;; (print (Clang-Auto-Scheduler))
;; Ref https://arxiv.org/pdf/2401.06665
(define-auto-scheduler (Clang-Auto-Scheduler (&key (fuse-softmax 1) (n-global-loop (ctx:getenv :OMP))))
    ;; Use outermost loop parallelism for maximize memory locality (better softmax/layernorm scheduling)
    :schedule-option (make-schedule-options :schedule-outer-coincidence fuse-softmax)
    :cost-functions '(:proximity :coincidence :validity)
    :n-global-loop n-global-loop ;; OMP=1 -> The outermost loop is GLOBAL, otherwise everything is a local loop
    )
