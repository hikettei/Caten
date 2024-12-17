(in-package :caten/isl)

(define-isl-object union-map
  :free %isl-union-map-free
  :copy %isl-union-map-copy
  :list-type union-map-list
  :from-str t)

(defmethod print-object ((value union-map) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-map-to-str (union-map-handle value)) stream)))

;; Creation

(define-isl-function union-map-empty %isl-union-map-empty
  (:give union-map)
  (:take space))

(define-isl-function union-map-universe %isl-union-map-universe
  (:give union-map)
  (:take space))

;; Conversion

(define-isl-function basic-map-union-map %isl-union-map-from-basic-map
  (:give union-map)
  (:take basic-map))

(define-isl-function map-union-map %isl-union-map-from-map
  (:give union-map)
  (:take map))

;; Operations

;; map -> map

(define-isl-function union-map-reverse %isl-union-map-reverse
  (:give union-map)
  (:take union-map))

;; (map, map) -> map

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give union-map)
                (:take union-map)
                (:take union-map))))
  (def union-map-intersect %isl-union-map-intersect)
  (def union-map-union %isl-union-map-union)
  (def union-map-subtract %isl-union-map-subtract)
  (def union-map-product %isl-union-map-product)
  (def union-map-lex-lt-union-map %isl-union-map-lex-lt-union-map)
  (def union-map-lex-le-union-map %isl-union-map-lex-le-union-map)
  (def union-map-lex-gt-union-map %isl-union-map-lex-gt-union-map)
  (def union-map-lex-ge-union-map %isl-union-map-lex-ge-union-map))

;; (map, map) -> bool

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give boolean)
                (:take union-map)
                (:take union-map))))
  (def union-map-equalp %isl-union-map-is-equal)
  (def union-map-subsetp %isl-union-map-is-subset)
  (def union-map-strict-subset-p %isl-union-map-is-strict-subset))

;; Domain and range

(define-isl-function union-map-domain %isl-union-map-domain
  (:give union-set)
  (:take union-map domain))

(define-isl-function union-map-deltas %isl-union-map-deltas
  (:give union-set)
  (:take union-map))

(define-isl-function union-map-range %isl-union-map-range
  (:give union-set)
  (:take union-map range))

(define-isl-function union-map-from-domain-and-range %isl-union-map-from-domain-and-range
  (:give union-map)
  (:take union-set domain)
  (:take union-set range))

(define-isl-function union-set-identity %isl-union-set-identity
  (:give union-map)
  (:take union-set))

(define-isl-function union-map-intersect-params %isl-union-map-intersect-params
  (:give union-map)
  (:take union-map)
  (:take set params))

(define-isl-function union-map-intersect-domain %isl-union-map-intersect-domain
  (:give union-map)
  (:take union-map)
  (:take union-set domain))

(define-isl-function union-map-intersect-range %isl-union-map-intersect-range
  (:give union-map)
  (:take union-map)
  (:take union-set range))

(define-isl-function union-map-subtract-domain %isl-union-map-subtract-domain
  (:give union-map)
  (:take union-map)
  (:take union-set domain))

(define-isl-function union-map-subtract-range %isl-union-map-subtract-range
  (:give union-map)
  (:take union-map)
  (:take union-set range))

(define-isl-function union-set-apply %isl-union-set-apply
  (:give union-set)
  (:take union-set)
  (:take union-map))

(define-isl-function union-map-apply-range %isl-union-map-apply-range
  (:give union-map)
  (:take union-map)
  (:take union-map range))

(define-isl-function union-map-apply-domain %isl-union-map-apply-domain
  (:give union-map)
  (:take union-map)
  (:take union-map domain))

(define-isl-function union-map-is-empty %isl-union-map-is-empty
  (:give boolean)
  (:take union-map))
