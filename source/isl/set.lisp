(in-package :caten/isl)

(define-isl-object set
  :free %isl-set-free
  :copy %isl-set-copy
  :list-type set-list
  :from-str t)

(defmethod print-object ((value set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-set-to-str (set-handle value)) stream)))

;; Creation
(define-isl-function set-empty %isl-set-empty
  (:give set)
  (:take space))

(define-isl-function set-universe %isl-set-universe
  (:give set)
  (:take space))

;; Conversion
(define-isl-function basic-set-set %isl-set-from-basic-set
  (:give set)
  (:take basic-set))
