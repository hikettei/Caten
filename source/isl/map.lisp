(in-package :caten/isl)

(define-isl-object map
  :free %isl-map-free
  :copy %isl-map-copy
  :list-type map-list
  :from-str t)

(defmethod print-object ((value map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-map-to-str (map-handle value)) stream)))
;; Creation
(define-isl-function map-empty %isl-map-empty
  (:give map)
  (:take space))
(define-isl-function map-universe %isl-map-universe
  (:give map)
  (:take space))
;; Conversion
(define-isl-function basic-map-map %isl-map-from-basic-map
  (:give map)
  (:take basic-map))

(define-isl-function map-from-domain %isl-map-from-domain
  (:give map)
  (:take set))

(define-isl-function map-range %isl-map-range
  (:give set)
  (:take map))

(defun map-move-dims (map dst-type dst-pos src-type src-pos n)
  (%make-map (%isl-map-move-dims (map-handle (copy map)) dst-type dst-pos src-type src-pos n)))

(defun map-dim-min (map pos)
  (%make-map (%isl-map-dim-min (map-handle (copy map)) pos)))

(defun map-dim-max (map pos)
  (%make-map (%isl-map-dim-max (map-handle (copy map)) pos)))

(define-isl-function map-wrap %isl-map-wrap
  (:give set)
  (:take map))
