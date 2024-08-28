(in-package :caten/isl)

(define-isl-object constraint
  :abstract t
  :free %isl-constraint-free
  :copy %isl-constraint-copy
  :list-type constraint-list)

(defmethod print-object ((constraint constraint) stream)
  (print-unreadable-object (constraint stream :type t)
    (let ((aff (%isl-constraint-get-aff (constraint-handle constraint))))
      (unwind-protect (write-string (%isl-aff-to-str aff) stream)
        (%isl-aff-free aff)))))

(define-isl-object equality-constraint
  :superclass constraint)
(define-isl-function make-equality-constraint %isl-constraint-alloc-equality
  (:give equality-constraint)
  (:take local-space))
(define-isl-function equality-constraint-set-constant %isl-constraint-set-constant-val
  (:give equality-constraint)
  (:take equality-constraint)
  (:take value))
(define-isl-function equality-constraint-set-coefficient %isl-constraint-set-coefficient-val
  (:give equality-constraint)
  (:take equality-constraint)
  (:keep dim-type)
  (:keep integer position)
  (:take value value))

(define-isl-object inequality-constraint
  :superclass constraint)
(define-isl-function make-inequality-constraint %isl-constraint-alloc-inequality
  (:give inequality-constraint)
  (:take local-space))
(define-isl-function inequality-constraint-set-constant %isl-constraint-set-constant-val
  (:give inequality-constraint)
  (:take inequality-constraint)
  (:take value))
(define-isl-function inequality-constraint-set-coefficient %isl-constraint-set-coefficient-val
  (:give inequality-constraint)
  (:take inequality-constraint)
  (:keep dim-type)
  (:keep integer position)
  (:take value value))
