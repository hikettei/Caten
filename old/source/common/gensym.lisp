(defpackage :caten/common.gensym
  (:documentation "Extended Gensym")
  (:use :cl)
  (:export
   #:with-local-gensym
   #:lgensym))
(in-package :caten/common.gensym)

(defstruct LGensym (map (make-hash-table :test 'equal) :type hash-table))
(defparameter *local-gensym* (make-lgensym))
(defun lgensym (&optional (thing "val_"))
  (let ((next-id (gethash thing (lgensym-map *local-gensym*) 0)))
    (prog1 (intern (string-upcase (format nil "~a~a" thing next-id)))
      (setf (gethash thing (lgensym-map *local-gensym*)) (1+ next-id)))))
(defmacro with-local-gensym (() &body body) `(let ((*local-gensym* (make-lgensym))) ,@body))
