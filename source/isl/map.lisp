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
