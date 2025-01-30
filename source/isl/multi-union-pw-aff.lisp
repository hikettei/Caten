(in-package :caten/isl)

(define-isl-object multi-union-pw-aff
  :free %isl-multi-union-pw-aff-free
  :copy %isl-multi-union-pw-aff-copy
  :from-str t)

(define-isl-object union-pw-aff
  :free %isl-union-pw-aff-free
  :copy %isl-union-pw-aff-copy)

(define-isl-object multi-val
  :free %isl-multi-val-free
  :copy %isl-multi-val-copy)

(define-isl-object pw-aff
  :free %isl-pw-aff-free
  :copy %isl-pw-aff-copy)

(defmethod print-object ((value multi-union-pw-aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-multi-union-pw-aff-to-str (multi-union-pw-aff-handle value)) stream)))

(defmethod print-object ((value multi-val) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-multi-val-to-str (multi-val-handle value)) stream)))

(define-isl-function mupa-from-union-map %isl-multi-union-pw-aff-from-union-map
  (:give multi-union-pw-aff)
  (:take union-map))

(define-isl-function multi-union-pw-aff-intersect-domain %isl-multi-union-pw-aff-intersect-domain
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take union-set))

(define-isl-function multi-union-pw-aff-scale-down-val %isl-multi-union-pw-aff-scale-down-val
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take value))

(define-isl-function union-pw-aff-scale-down-val %isl-union-pw-aff-scale-down-val
  (:give union-pw-aff)
  (:take union-pw-aff)
  (:take value))

(define-isl-function multi-union-pw-aff-floor %isl-multi-union-pw-aff-floor
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff))

(define-isl-function union-pw-aff-floor %isl-union-pw-aff-floor
  (:give union-pw-aff)
  (:take union-pw-aff))

(define-isl-function union-pw-aff-scale-val %isl-union-pw-aff-scale-val
  (:give union-pw-aff)
  (:take union-pw-aff)
  (:take value))

(defun multi-union-pw-aff-get-union-pw-aff (mupa int)
  (%make-union-pw-aff (%isl-multi-union-pw-aff-get-union-pw-aff (multi-union-pw-aff-handle (copy mupa)) int)))

(defun multi-union-pw-aff-size (mupa)
  (%isl-multi-union-pw-aff-size (multi-union-pw-aff-handle (copy mupa))))

(define-isl-function multi-union-pw-aff-min-multi-val %isl-multi-union-pw-aff-min-multi-val
  (:give multi-val)
  (:take multi-union-pw-aff))

(defun multi-val-get-val (mval nth)
  (%make-value (%isl-multi-val-get-val (multi-val-handle (copy mval)) nth)))

(defun multi-val-set-val (mval nth val)
  (%make-multi-val (%isl-multi-val-set-val (multi-val-handle (copy mval)) nth (value-handle (copy val)))))

(defun multi-union-pw-aff-set-union-pw-aff (mupa pos upa)
  (%make-multi-union-pw-aff (%isl-multi-union-pw-aff-set-union-pw-aff (multi-union-pw-aff-handle (copy mupa)) pos (union-pw-aff-handle (copy upa)))))

(define-isl-function multi-union-pw-aff-multi-val-on-domain %isl-multi-union-pw-aff-multi-val-on-domain
  (:give multi-union-pw-aff)
  (:take union-set)
  (:take multi-val))

(define-isl-function multi-union-pw-aff-neg %isl-multi-union-pw-aff-neg
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff))

(define-isl-function multi-union-pw-aff-add %isl-multi-union-pw-aff-add
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take multi-union-pw-aff))

(define-isl-function multi-val-from-val-list %isl-multi-val-from-val-list
  (:give multi-val)
  (:take space)
  (:take value-list))

(define-isl-function multi-union-pw-aff-flat-range-product %isl-multi-union-pw-aff-flat-range-product
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take multi-union-pw-aff))

(define-isl-function multi-union-pw-aff-get-space %isl-multi-union-pw-aff-get-space
  (:give space)
  (:keep multi-union-pw-aff))

(defun pw-aff-var-on-domain (local-space type pos)
  (%make-pw-aff (%isl-pw-aff-var-on-domain (isl-object-handle (copy local-space)) type pos)))

(defun multi-union-pw-aff-reset-tuple-id (mupa type)
  (declare (type multi-union-pw-aff mupa))
  (%make-multi-union-pw-aff (%isl-multi-union-pw-aff-reset-tuple-id (multi-union-pw-aff-handle (copy mupa)) type)))

(define-isl-function multi-union-pw-aff-union-add %isl-multi-union-pw-aff-union-add
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take multi-union-pw-aff))
