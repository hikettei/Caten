(defpackage :caten/polyhedral/config
  (:use :cl)
  (:export

   ))

(in-package :caten/polyhedral/config)

(macrolet ((def-schedule-option (&rest options-and-default)
             `(progn
                (defclass Schedule-Config ()
                  nil
                  (:documentation ""))
                (defmethod apply-config ((config Schedule-Config))

                  ))))
  (def-schedule-option
      
      ))

(defclass Poly-Config ()
  ()
  (:documentation ""))

