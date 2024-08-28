(defpackage :caten/external.backends.metal
  (:use :cl :caten/ajit :caten/air :caten/avm :cffi)
  (:import-from
   :caten/common.dtype
   #:dtype/cast))
(in-package :caten/external.backends.metal)

(defparameter *access* nil)
(defparameter *args* nil)
(defun args-p (id) (if (stringp id) (find (intern id) *args*) (find id *args*)))
