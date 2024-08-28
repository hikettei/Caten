(in-package :caten/isl)

(define-isl-object space
  :free %isl-space-free
  :copy %isl-space-copy)

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
