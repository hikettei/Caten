(defpackage :caten/polyhedral/config
  (:use :cl :cffi)
  (:export
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
                  (defclass Schedule-Config ()
                    ,(loop for (name . default) in options-and-default
                           for lisp-name = (lispify name)
                           collect `(,(intern lisp-name) :initarg ,(intern lisp-name "KEYWORD") :initform ,default))
                    (:documentation ""))
                  (defmethod print-object ((config Schedule-Config) stream)
                    (print-unreadable-object (config stream :type t)
                      (format stream "~%")
                      ,@(loop for (name . default) in options-and-default
                              for lisp-name = (intern (lispify name))
                              collect `(format stream "  ~a | ~a |~%" ,(align name) (slot-value config ',lisp-name)))))
                  (defmethod apply-config-global ((config Schedule-Config))
                    ,@(loop for (name . default) in options-and-default
                            for lisp-name = (intern (lispify name))
                            collect `(foreign-funcall ,name :pointer (isl::context-handle isl::*context*) :int (slot-value config ',lisp-name) :void)))))))
  (define-schedule-option
    ("schedule_serialize_sccs" . 0)
    ("schedule_outer_coincidence" . 0)
    ("schedule_maximize_coincidence" . 1)
    ("schedule_treat_coalescing" . 1)
    ("schedule_maximize_band_depth" . 1)
    ("schedule_whole_component" . 0)))

(defclass Poly-Config ()
  ()
  (:documentation ""))

(print (make-instance 'Schedule-Config))

(defmacro define-auto-scheduler (name)

  )


(define-auto-scheduler ClangScheduler

  )
