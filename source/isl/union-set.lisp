(in-package :caten/isl)

(define-isl-object union-set
  :free %isl-union-set-free
  :copy %isl-union-set-copy
  :list-type union-set-list
  :from-str t)

(defmethod print-object ((value union-set) stream)
  (print-unreadable-object (value stream :type t)
    (write-string (%isl-union-set-to-str (union-set-handle value)) stream)))

;; Creation

(define-isl-function union-set-empty %isl-union-set-empty
  (:give union-set)
  (:take space))

(define-isl-function union-set-universe %isl-union-set-universe
  (:give union-set)
  (:take space))

;; Conversion

(define-isl-function basic-set-union-set %isl-union-set-from-basic-set
  (:give union-set)
  (:take basic-set))

(define-isl-function union-set-from-set %isl-union-set-from-set
  (:give union-set)
  (:take set))

(define-isl-function set-union-set %isl-set-from-basic-set
  (:give union-set)
  (:take set))

;; Operations

;; (set, set) -> set

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give union-set)
                (:take union-set)
                (:take union-set))))
  (def union-set-intersect %isl-union-set-intersect)
  (def union-set-union %isl-union-set-union)
  (def union-set-subtract %isl-union-set-subtract)
  (def union-set-product %isl-union-set-product)
  (def union-set-lex-lt-union-set %isl-union-set-lex-lt-union-set)
  (def union-set-lex-le-union-set %isl-union-set-lex-le-union-set)
  (def union-set-lex-gt-union-set %isl-union-set-lex-gt-union-set)
  (def union-set-lex-ge-union-set %isl-union-set-lex-ge-union-set))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give boolean)
                (:take union-set)
                (:take union-set))))
  (def union-set-equalp %isl-union-set-is-equal)
  (def union-set-subsetp %isl-union-set-is-subset)
  (def union-set-strict-subset-p %isl-union-set-is-strict-subset))

(define-isl-function union-set-intersect-params %isl-union-set-intersect-params
  (:give union-set)
  (:take union-set)
  (:take set params))

(defun make-union-set-list (&rest union-set-list)
  (assert (every #'union-set-p union-set-list) () "make-union-set-list: ~a is not a list of union-set" union-set-list)
  (let* ((n (length union-set-list))
	 (ls (%make-union-set-list (%isl-union-set-list-alloc (context-handle *context*) n))))
    (loop for us in union-set-list
	  for nth upfrom 0
	  do (%isl-union-set-list-add (union-set-list-handle ls) (union-set-handle us)))
    ls))

(define-isl-function union-set-get-space %isl-union-set-get-space
  (:give space)
  (:take union-set))
