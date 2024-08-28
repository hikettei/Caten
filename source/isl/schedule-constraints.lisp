(in-package :caten/isl)

(define-isl-object schedule-constraints
  :free %isl-schedule-constraints-free
  :copy %isl-schedule-constraints-copy
  :from-str t)

(defmethod print-object ((value schedule-constraints) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-schedule-constraints-to-str (schedule-constraints-handle value)) stream)))

;; Creation

(define-isl-function schedule-constraints-on-domain %isl-schedule-constraints-on-domain
  (:give schedule-constraints)
  (:take union-set))

;; Schedule constraints set something

(define-isl-function schedule-constraints-get-context %isl-schedule-constraints-get-context
  (:give context)
  (:keep schedule-constraints))

(define-isl-function schedule-constraints-set-context %isl-schedule-constraints-set-context
  (:give schedule-constraints)
  (:take schedule-constraints)
  (:take set))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give schedule-constraints)
                (:take schedule-constraints)
                (:take union-map))))
  (def schedule-constraints-set-validity %isl-schedule-constraints-set-validity)
  (def schedule-constraints-set-coincidence %isl-schedule-constraints-set-coincidence)
  (def schedule-constraints-set-proximity %isl-schedule-constraints-set-proximity))

(define-isl-function schedule-constraints-set-conditional-validity %isl-schedule-constraints-set-conditional-validity
  (:give schedule-constraints)
  (:take schedule-constraints)
  (:take union-map condition)
  (:take union-map validity))
