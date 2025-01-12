(defpackage :caten/codegen/packing
  (:documentation "`caten/codegen/packing` provides a transformation tool to
replace the packed region (defined in auto-scheduler.lisp, class Packing) with the vectorized ops.
The class `Vectorize` will provide the information of the vectorized region, passed via define-auto-scheduler macro.
TensorCore optimization is also implemented as a part of Vectorize.
")
  (:use :cl)
  (:export
   #:Vectorize
   #:TensorCore))

(in-package :caten/codegen/packing)

(defstruct (Vectorize
            (:constructor vectorize (dims &key (applicable-p #'identity) (rewriter nil))))
  (dims dims :type list)
  (applicable-p applicable-p :type function)
  (rewriter rewriter :type function))

(defun TensorCore (dims)
  (declare (type list dims))
  (assert (and (= (length dims) 3) (every #'integerp dims)) () "TensorCore: dims are three-dimensional list of integers.")
  (Vectorize dims :applicable-p #'identity :rewriter #'identity))

(defun apply-vectorize ()

  )
