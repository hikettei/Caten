(in-package :caten/isl)

(define-isl-object schedule
  :free %isl-schedule-free
  :copy %isl-schedule-copy)

(defmethod print-object ((value schedule) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-schedule-to-str (schedule-handle value)) stream)))

(define-isl-function schedule-constraints-compute-schedule %isl-schedule-constraints-compute-schedule
  (:give schedule)
  (:take schedule-constraints))

(define-isl-function schedule-sequence %isl-schedule-sequence
  (:give schedule)
  (:take schedule)
  (:take schedule))

(define-isl-function schedule-to-str %isl-schedule-to-str
  (:give string)
  (:keep schedule))

(define-isl-function schedule-get-map %isl-schedule-get-map
  (:give union-map)
  (:keep schedule))

(define-isl-function schedule-set %isl-schedule-set
  (:give schedule)
  (:take schedule)
  (:take schedule))

(define-isl-function ast-build-node-from-schedule %isl-ast-build-node-from-schedule
  (:give ast-node)
  (:keep ast-build)
  (:take schedule))

(define-isl-function schedule-from-domain %isl-schedule-from-domain
  (:give schedule)
  (:take union-set))
