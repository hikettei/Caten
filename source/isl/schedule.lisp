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
