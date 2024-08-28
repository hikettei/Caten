(in-package :caten/isl)

(define-isl-object affine
  :free %isl-aff-free
  :copy %isl-aff-copy
  :list-type affine-list
  :from-str t)

(defmethod print-object ((value affine) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-aff-to-str (affine-handle value)) stream)))

;; Not sure to use the local-psace version or not for this function
(define-isl-function create-empty-affine %isl-aff-zero-on-domain
  (:give affine)
  (:take local-space))

(define-isl-function create-val-affine %isl-aff-val-on-domain
  (:give affine)
  (:take local-space)
  (:take value))

(define-isl-function create-var-affine %isl-aff-var-on-domain
  (:give affine)
  (:take local-space)
  (:keep dim-type)
  (:keep integer position))

;; Binary

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give affine)
                (:take affine)
                (:take affine))))
  (def affine-add %isl-aff-add)
  (def affine-sub %isl-aff-sub)
  (def affine-mul %isl-aff-mul)
  (def affine-div %isl-aff-div))
