(defpackage :caten/polyhedral/search
  (:documentation "Provides a general-purpose search framework to minimize the metric.
A input is base schedule and sketch (a list of transformations), The class `SearchMethod` tries to find the best sketch to minimize the metric.")
  (:use :cl :caten/polyhedral/ir)
  (:export #:Sketch #:apply-sketch #:metric #:run-search))

(in-package :caten/polyhedral/search)

(defclass Sketch () nil)
(defclass SearchMethod () nil)

(defgeneric metric (search-method poly))
(defgeneric apply-sketch (search-method sketch))

(defun run-search (base-poly search-method sketch-list &aux (base-schedule (poly-schedule base-poly)))
  (declare (type list sketch-list) (type Polyhedral-IR base-poly))
  (flet ((try-sketch (sketch)
           (setf (poly-schedule base-poly) base-schedule
                 (poly-schedule base-poly) (apply-sketch sketch base-poly))
           (metric search-method base-poly)))
    (let* ((base-score (metric search-method base-poly))
           (scores (map 'list #'try-sketch sketch-list))
           (best-sketch (reduce #'max scores))
           (pos (position best-sketch scores))
           (chosen-sketch (nth pos sketch-list)))
      (setf (poly-schedule base-poly) base-schedule)
      (when (>= base-score best-sketch)
        (return-from run-search base-poly))
      (setf (poly-schedule base-poly) (apply-sketch chosen-sketch base-poly)
            sketch-list (loop for nth upfrom 0 for s in sketch-list unless (= nth pos) collect s))
      (if sketch-list
          (run-search base-poly search-method sketch-list)
          base-poly))))
