(defpackage :caten/polyhedral/search
  (:documentation "Provides a general-purpose search framework to minimize the metric.
A input is base schedule and sketch (a list of transformations), The class `SearchMethod` tries to find the best sketch to minimize the metric.")
  (:use :cl)
  (:export #:Sketch))

(in-package :caten/polyhedral/search)

(defclass Sketch () nil)
(defclass SearchMethod () nil)

(defgeneric metric (search-method))
(defgeneric apply-sketch (search-method sketch))
