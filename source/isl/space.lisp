(in-package :caten/isl)

(define-isl-object space
  :free %isl-space-free
  :copy %isl-space-copy)

(define-isl-object multi-aff
  :free %isl-multi-aff-free
  :copy %isl-multi-aff-copy)

(defmethod print-object ((value space) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-space-to-str (space-handle value)) stream)))

(define-isl-function space-add-param-id %isl-space-add-param-id
  (:give space)
  (:take space)
  (:take identifier))

(define-isl-function create-space-params %isl-space-params-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam))

(define-isl-function create-space-set %isl-space-set-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam)
  (:keep integer dim))

(define-isl-function create-space-map %isl-space-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam)
  (:keep integer n_in)
  (:keep integer n_out))

(defun space-dim (space dim-type)
  (%isl-space-dim (space-handle space) dim-type))

(define-isl-function multi-aff-zero %isl-multi-aff-zero
  (:give multi-aff)
  (:take space))

(define-isl-function space-set-from-params %isl-space-set-from-params
  (:give space)
  (:take space))

(defun space-add-dims (space dim-type n)
  (%make-space (%isl-space-add-dims (space-handle (copy space)) dim-type n)))
