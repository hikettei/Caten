(defpackage :caten/codegen/fusion-rules
  (:documentation "`caten/coodegen/fusion-rules` is a package that supports for schedule.lisp, especially when merging compilcated views.")
  (:use :cl :caten/air)
  (:export
   #:apply-fusion-rules))

(in-package :caten/codegen/fusion-rules)

(defun apply-fusion-rules ()
  "This function should only called in the `transform-and-mergeable-p` function."
  nil)
