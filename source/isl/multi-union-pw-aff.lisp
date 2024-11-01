(in-package :caten/isl)

(define-isl-object multi-union-pw-aff
  :free %isl-multi-union-pw-aff-free
  :copy %isl-multi-union-pw-aff-copy
  :from-str t)

(define-isl-object multi-val
  :free %isl-multi-val-free
  :copy %isl-multi-val-copy)

(defmethod print-object ((value multi-union-pw-aff) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-multi-union-pw-aff-to-str (multi-union-pw-aff-handle value)) stream)))

(define-isl-function mupa-from-union-map %isl-multi-union-pw-aff-from-union-map
  (:give multi-union-pw-aff)
  (:take union-map))

(define-isl-function multi-union-pw-aff-intersect-domain %isl-multi-union-pw-aff-intersect-domain
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take union-set))

(defun multi-union-pw-aff-size (mupa)
  (%isl-multi-union-pw-aff-size (multi-union-pw-aff-handle mupa)))

(define-isl-function multi-union-pw-aff-min-multi-val %isl-multi-union-pw-aff-min-multi-val
  (:give multi-val)
  (:take multi-union-pw-aff))

(defun multi-val-get-val (mval nth)
  (%make-multi-val (%isl-multi-val-get-val (multi-val-handle mval) nth)))

(defun multi-val-set-val (mval nth val)
  (%make-multi-val (%isl-multi-val-set-val (multi-val-handle mval) nth (multi-val-handle val))))

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
