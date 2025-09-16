(in-package :caten/isl)
;;;; Identifiers
(export 'make-id-from-str)
(defun make-id-from-str (name)
  (declare (type string name))
  (%make-identifier
   (cffi:with-foreign-string (char* name)
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))
;; If we call this function twice with the same arguments it will create the same result
(export 'make-identifier)
(defun make-identifier (name &key (no-intern nil))
  (declare (symbol name))
  (%make-identifier
   (cffi:with-foreign-string (char* (if no-intern (format nil "~(~a~)" name) (string-from-symbol name)))
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))

;; Not the same result if we call this function twice
(export 'make-gensym-identifier)
(defun make-gensym-identifier (name)
  (declare (symbol name))
  (%make-identifier
   (cffi:with-foreign-string (char* (string-from-symbol
                                     (gensym
                                      (string-from-symbol name))))
     (%isl-id-alloc (context-handle *context*) char* (cffi:null-pointer)))))
(export 'identifier-name)
(defun identifier-name (identifier)
  (declare (identifier identifier))
  (let* ((handle (identifier-handle identifier))
         (char* (%isl-id-get-name handle)))
    (values
     (read-from-string
      (cffi:foreign-string-to-lisp char*)))))
(export 'identifier-name-str)
(defun identifier-name-str (identifier)
  (declare (identifier identifier))
  (let* ((handle (identifier-handle identifier))
         (char* (%isl-id-get-name handle)))
    (cffi:foreign-string-to-lisp char*)))

(define-isl-function identifier-context %isl-id-get-ctx
    (:give context)
    (:keep identifier))
(export 'make-id-list)
(defun make-id-list (&rest id-list)
  (declare (type list id-list))
  (let* ((n (length id-list))
	 (id-list (cl:map 'list #'(lambda (x) (make-identifier x :no-intern t)) id-list))
	 (ls (%make-identifier-list (%isl-id-list-alloc (context-handle *context*) n))))
    (loop for id in id-list
	  for nth upfrom 0
	  do (%isl-id-list-add (identifier-list-handle ls) (identifier-handle id)))
    ls))
;;;; Value
(define-isl-function value-context %isl-val-get-ctx
    (:give context)
  (:keep value))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give value)
                  (:parm context *context*))))
  (def value-zero %isl-val-zero)
  (def value-one %isl-val-one)
  (def value-minus-one %isl-val-negone)
  (def value-nan %isl-val-nan)
  (def value-positive-infinity %isl-val-infty)
  (def value-negative-infinity %isl-val-neginfty))

(define-isl-function value-sign %isl-val-sgn
    (:give (integer -1 1))
    (:keep value))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give boolean)
                  (:keep value))))
  (def value-zerop %isl-val-is-zero)
  (def value-onep %isl-val-is-one)
  (def value-minus-one-p %isl-val-is-negone)
  (def value-not-minusp %isl-val-is-nonneg)
  (def value-not-plusp %isl-val-is-nonpos)
  (def value-plusp %isl-val-is-pos)
  (def value-minusp %isl-val-is-neg)
  (def value-integerp %isl-val-is-int)
  (def value-rationalp %isl-val-is-rat)
  (def value-nan-p %isl-val-is-nan)
  (def value-positive-infinity-p %isl-val-is-infty)
  (def value-negative-infinity-p %isl-val-is-neginfty))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give boolean)
                  (:keep value)
                  (:keep value))))
  (def value<  %isl-val-lt)
  (def value<= %isl-val-le)
  (def value>  %isl-val-gt)
  (def value>= %isl-val-ge)
  (def value= %isl-val-eq)
  (def value/= %isl-val-ne)
  (def value-abs= %isl-val-abs-eq))

(define-isl-function value-divisible-by %isl-val-is-divisible-by
    (:give boolean)
    (:keep value)
    (:keep value))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give value)
                  (:take value))))
  (def value-abs %isl-val-abs)
  (def value-neg %isl-val-neg)
  (def value-floor %isl-val-floor)
  (def value-ceiling %isl-val-ceil)
  (def value-truncate %isl-val-trunc)
  (def value-inverse %isl-val-inv)
  (def value-expt2 %isl-val-pow2))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                  (:give value)
                  (:take value)
                  (:take value))))
  (def value-min %isl-val-min)
  (def value-max %isl-val-max)
  (def value+ %isl-val-add)
  (def value- %isl-val-sub)
  (def value-mul %isl-val-mul)
  (def value-div %isl-val-div)
  (def value-mod %isl-val-mod)
  (def value-gcd %isl-val-gcd))

;;(define-isl-function value-gcdext %isl-val-gcdext
;;  (:give value)
;;  (:take value a)
;;  (:take value b)
;;  (:give value x)
;;  (:give value y))
(export '%value)
(defun %value (value-designator)
  (etypecase value-designator
    (value
     (%isl-val-copy (value-handle value-designator)))
    ((signed-byte 64) value-designator
     (%isl-val-int-from-si (context-handle *context*) value-designator))
    (integer
     (let* ((n (ceiling (integer-length value-designator) 8))
            (num (abs value-designator))
            (handle
              (cffi:with-foreign-object (chunks :uint64 n)
                (loop for index below n do
                  (setf (cffi:mem-aref chunks :uint64 index)
                        (ldb (byte 64 (* 64 index)) num)))
                (%isl-val-int-from-chunks (context-handle *context*) n 8 chunks))))
       (if (minusp value-designator)
           (%isl-val-neg handle)
           handle)))
    (rational
     (%isl-val-div
      (%value (numerator value-designator))
      (%value (denominator value-designator))))))

(define-isl-function value %value
  (:give value)
  (:keep value-designator))

(export '%value-object)
(defun %value-object (handle)
  (flet ((%numerator (handle)
           (let ((n (%isl-val-n-abs-num-chunks handle 8))
                 (num 0))
             (cffi:with-foreign-object (chunks :uint64 n)
               (%isl-val-get-abs-num-chunks handle 8 chunks)
               (loop for index below n do
                 (setf (ldb (byte 64 (* 64 index)) num)
                       (cffi:mem-aref chunks :uint64 index))))
             (if (lispify-isl-bool (%isl-val-is-neg handle)) (- num) num))))
    (cond ((lispify-isl-bool (%isl-val-is-int handle))
           (%numerator handle))
          ((lispify-isl-bool (%isl-val-is-rat handle))
           (/ (%numerator handle)
              (let ((den (%isl-val-get-den-val handle)))
                (unwind-protect (%numerator den)
                  (%isl-val-free den)))))
          ((error "Don't know how to convert ~S to a Lisp object."
                  (%make-value handle))))))

(define-isl-function value-object %value-object
  (:give value-designator)
  (:keep value))

(export 'make-value-list)
(defun make-value-list (&rest value-list)
  (declare (type list value-list))
  (let* ((n (length value-list))
	 (value-list (cl:map 'list #'value value-list))
	 (ls (%make-value-list (%isl-val-list-alloc (context-handle *context*) n))))
    (loop for value in value-list
	  for nth upfrom 0
	  do (%isl-val-list-add (value-list-handle ls) (value-handle (__isl_take value))))
    ls))
;;;; Space
(define-isl-function space-add-param-id %isl-space-add-param-id
  (:give space)
  (:take space)
  (:take identifier))

(export 'space-get-dim-id)
(defun space-get-dim-id (space type pos)
  (%make-identifier (%isl-space-get-dim-id (space-handle space) type pos)))

(export 'space-get-tuple-name)
(defun space-get-tuple-name (space type)
  (%isl-space-get-tuple-name (space-handle space) type))

(export 'space-find-dim-by-id)
(defun space-find-dim-by-id (space type id)
  (%isl-space-find-dim-by-id (space-handle space) type (identifier-handle id)))

(export 'space-is-params)
(defun space-is-params (space)
  (eql :bool-true (%isl-space-is-params (space-handle space))))

(define-isl-function create-space-params %isl-space-params-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam))

(define-isl-function create-space-set %isl-space-set-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam)
  (:keep integer dim))

(define-isl-function create-space-map %isl-space-alloc
  (:give space)
  (:parm context *context*)
  (:keep integer nparam)
  (:keep integer n_in)
  (:keep integer n_out))

(define-isl-function space-set-from-params %isl-space-set-from-params
  (:give space)
  (:take space))

(export 'space-dim)
(defun space-dim (space dim-type)
  (%isl-space-dim (space-handle space) dim-type))

(define-isl-function multi-aff-zero %isl-multi-aff-zero
  (:give multi-aff)
  (:take space))

(define-isl-function isl-space-range-product %isl-space-range-product
  (:give space)
  (:take space)
  (:take space))

(define-isl-function space-align-params %isl-space-align-params
  (:give space)
  (:take space)
  (:take space))
;;;; LocalSpace

(define-isl-function local-space-from-space %isl-local-space-from-space
  (:give local-space)
  (:take space))

(define-isl-function local-space-space %isl-local-space-get-space
  (:give space)
  (:keep local-space))

(define-isl-function local-space-get-space %isl-local-space-get-space
  (:give space)
  (:keep local-space))
;;;; Constraint
(define-isl-function make-equality-constraint %isl-constraint-alloc-equality
  (:give equality-constraint)
  (:take local-space))

(define-isl-function equality-constraint-set-constant %isl-constraint-set-constant-val
  (:give equality-constraint)
  (:take equality-constraint)
  (:take value))

(define-isl-function constraint-get-space %isl-constraint-get-space
  (:give space)
  (:keep constraint))

(define-isl-function constraint-get-local-space %isl-constraint-get-local-space
  (:give local-space)
  (:keep constraint))

(export 'constraint-get-dim-name)
(defun constraint-get-dim-name (constraint type pos)
  (%isl-constraint-get-dim-name (constraint-handle constraint) type pos))

(define-isl-function constraint-get-aff %isl-constraint-get-aff
  (:give aff)
  (:keep constraint))

(define-isl-function equality-constraint-set-coefficient %isl-constraint-set-coefficient-val
  (:give equality-constraint)
  (:take equality-constraint)
  (:keep dim-type)
  (:keep integer position)
  (:take value value))

(define-isl-function make-inequality-constraint %isl-constraint-alloc-inequality
  (:give inequality-constraint)
  (:take local-space))

(define-isl-function inequality-constraint-set-constant %isl-constraint-set-constant-val
  (:give inequality-constraint)
  (:take inequality-constraint)
  (:take value))

(define-isl-function inequality-constraint-set-coefficient %isl-constraint-set-coefficient-val
  (:give inequality-constraint)
  (:take inequality-constraint)
  (:keep dim-type)
  (:keep integer position)
  (:take value value))

(export 'set-constant-si)
(defgeneric set-constant-si (constraint v))
(defmethod set-constant-si ((constraint equality-constraint) v)
  (%make-equality-constraint (%isl-constraint-set-constant-si (constraint-handle (__isl_take constraint)) v)))

(defmethod set-constant-si ((constraint inequality-constraint) v)
  (%make-inequality-constraint (%isl-constraint-set-constant-si (constraint-handle (__isl_take constraint)) v)))

(export 'set-constant-val)
(defgeneric set-constant-val (constraint v))

(defmethod set-constant-val ((constraint equality-constraint) v)
  (%make-equality-constraint (%isl-constraint-set-constant-val (constraint-handle (__isl_take constraint)) (value-handle (__isl_take v)))))

(defmethod set-constant-val ((constraint inequality-constraint) v)
  (%make-inequality-constraint (%isl-constraint-set-constant-val (constraint-handle (__isl_take constraint)) (value-handle (__isl_take v)))))

(export 'set-coefficient-si)
(defun set-coefficient-si (constraint type pos v)
  (funcall
   (etypecase constraint
     (equality-constraint #'%make-equality-constraint)
     (inequality-constraint #'%make-inequality-constraint))
   (%isl-constraint-set-coefficient-si (constraint-handle (__isl_take constraint)) type pos v)))
(export 'get-coefficient-val)
(defun get-coefficient-val (constraint type pos)
  (%make-value
   (%isl-constraint-get-coefficient-val (constraint-handle constraint) type pos)))
(export 'get-constant-val)
(defun get-constant-val (constraint)
  (%make-value
   (%isl-constraint-get-constant-val (constraint-handle constraint))))

;;;; BasicSet
(define-isl-function basic-set-empty %isl-basic-set-empty
  (:give basic-set)
  (:take space))

(define-isl-function basic-set-universe %isl-basic-set-universe
  (:give basic-set)
  (:take space))

(define-isl-function basic-set-intersect %isl-basic-set-intersect
  (:give basic-set)
  (:take basic-set)
  (:take basic-set))

(define-isl-function basic-set-add-constraint %isl-basic-set-add-constraint
  (:give basic-set)
  (:take basic-set)
  (:take constraint))

(export 'basic-set-get-dim-name)
(defun basic-set-get-dim-name (bset type pos)
  (%isl-basic-set-get-dim-name (basic-set-handle bset) type pos))

(export 'basic-map-get-dim-name)
(defun basic-map-get-dim-name (bmap type pos)
  (%isl-basic-map-get-dim-name (basic-map-handle bmap) type pos))

(export 'map-get-dim-name)
(defun map-get-dim-name (map type pos)
  (%isl-map-get-dim-name (map-handle map) type pos))

(define-isl-function map-params %isl-map-params
  (:give set)
  (:take map))

(export 'basic-set-drop-constraints-involving-dims)
(defun basic-set-drop-constraints-involving-dims (basic-set type first n)
  (%make-basic-set
   (%isl-basic-set-drop-constraints-involving-dims (basic-set-handle (__isl_take basic-set)) type first n)))
;;;; Set
(define-isl-function set-set-tuple-name %isl-set-set-tuple-name
  (:give set)
  (:take set)
  (:take string))

(define-isl-function set-intersect %isl-set-intersect
  (:give set)
  (:take set)
  (:take set))

(define-isl-function set-empty %isl-set-empty
  (:give set)
  (:take space))

(define-isl-function set-universe %isl-set-universe
  (:give set)
  (:take space))

(define-isl-function basic-set-set %isl-set-from-basic-set
  (:give set)
  (:take basic-set))

(define-isl-function set-from-basic-set %isl-set-from-basic-set
  (:give set)
  (:take basic-set))

(define-isl-function set-get-space %isl-set-get-space
  (:give space)
  (:take set))

(define-isl-function set-from-multi-aff %isl-set-from-multi-aff
  (:give set)
  (:take multi-aff))

(define-isl-function set-simple-hull %isl-set-simple-hull
  (:give basic-set)
  (:take set))

(export 'set-dim-max)
(defun set-dim-max (set dim)
  (%make-pw-aff (%isl-set-dim-max (set-handle (__isl_take set)) dim)))

(export 'set-dim-min)
(defun set-dim-min (set dim)
  (%make-pw-aff (%isl-set-dim-min (set-handle (__isl_take set)) dim)))

(export 'set-dim)
(defun set-dim (set type)
  (%isl-set-dim (set-handle set) type))

(export 'set-project-out)
(defun set-project-out (set type first n)
  (%make-set (%isl-set-project-out (set-handle (__isl_take set)) type first n)))

(export 'set-is-params)
(defun set-is-params (set)
  (eql :bool-true (%isl-set-is-params (set-handle set))))

(define-isl-function set-intersect-params %isl-set-intersect-params
  (:give set)
  (:take set)
  (:take set))

(define-isl-function set-subtract %isl-set-subtract
  (:give set)
  (:take set)
  (:take set))

(define-isl-function set-add-constraint %isl-set-add-constraint
  (:give set)
  (:take set)
  (:take constraint))

(export 'set-drop-constraints-involving-dims)
(defun set-drop-constraints-involving-dims (set type first n)
  (%make-set
   (%isl-set-drop-constraints-involving-dims (set-handle (__isl_take set)) type first n)))

(define-isl-function set-set-tuple-id %isl-set-set-tuple-id
  (:give set)
  (:take set)
  (:take identifier))

(define-isl-function set-get-basic-set-list %isl-set-get-basic-set-list
  (:give basic-set-list)
  (:keep set))

(export 'set-list-n-set)
(defun set-list-n-set (lst)
  (%isl-set-list-n-set (set-list-handle lst)))

(export 'set-list-get-at)
(defun set-list-get-at (lst n)
  (%make-set (%isl-set-list-get-at (set-list-handle lst) n)))

(export 'basic-set-list-get-at)
(defun basic-set-list-get-at (lst n)
  (%make-basic-set (%isl-basic-set-list-get-at (basic-set-list-handle lst) n)))

(export 'basic-set-dim)
(defun basic-set-dim (bset dim)
  (%isl-basic-set-dim (basic-set-handle (__isl_take bset)) dim))

(export 'set-get-dim-id)
(defun set-get-dim-id (set type pos)
  (%make-identifier (%isl-set-get-dim-id (set-handle set) type pos)))

(export 'set-get-dim-name)
(defun set-get-dim-name (set type pos)
  (%isl-set-get-dim-name (set-handle set) type pos))

(define-isl-function set-get-tuple-id %isl-set-get-tuple-id
  (:give identifier)
  (:keep set))

(export 'set-get-tuple-name)
(defun set-get-tuple-name (set)
  (%isl-set-get-tuple-name (set-handle set)))

(export 'set-is-empty)
(defun set-is-empty (set)
  (eql :bool-true (%isl-set-is-empty (set-handle set))))

(define-isl-function set-align-params %isl-set-align-params
  (:give set)
  (:take set)
  (:take space))

(cffi:defcfun ("isl_set_dim_max_val" %isl-set-dim-max-val) :pointer (x :pointer) (pos :int))
(cffi:defcfun ("isl_set_dim_min_val" %isl-set-dim-min-val) :pointer (x :pointer) (pos :int))

(export 'set-dim-max-val)
(defun set-dim-max-val (set pos)
  (%make-value (%isl-set-dim-max-val (set-handle (__isl_take set)) pos)))

(export 'set-dim-min-val)
(defun set-dim-min-val (set pos)
  (%make-value (%isl-set-dim-min-val (set-handle (__isl_take set)) pos)))
;;;; UnionSet
(define-isl-function union-set-n-set %isl-union-set-n-set
  (:give fixnum)
  (:keep union-set))

(define-isl-function union-set-empty %isl-union-set-empty
  (:give union-set)
  (:take space))

(define-isl-function union-set-universe %isl-union-set-universe
  (:give union-set)
  (:take space))

(define-isl-function basic-set-union-set %isl-union-set-from-basic-set
  (:give union-set)
  (:take basic-set))

(define-isl-function union-set-from-set %isl-union-set-from-set
  (:give union-set)
  (:take set))

(define-isl-function set-union-set %isl-union-set-from-set
  (:give union-set)
  (:take set))

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

(define-isl-function union-set-is-empty %isl-union-set-is-empty
  (:give boolean)
  (:take union-set))

(define-isl-function set-from-union-set %isl-set-from-union-set
  (:give set)
  (:take union-set))

(define-isl-function union-set-get-space %isl-union-set-get-space
  (:give space)
  (:keep union-set))

(define-isl-function union-set-get-set-list %isl-union-set-get-set-list
  (:give set-list)
  (:keep union-set))

(export 'union-set-list-alloc)
(defun union-set-list-alloc (n)
  (%make-union-set-list (%isl-union-set-list-alloc (context-handle *context*) n)))

(export 'union-set-list-add)
(defun union-set-list-add (lst uset)
  (%make-union-set-list (%isl-union-set-list-add (union-set-list-handle (__isl_take lst)) (union-set-handle (__isl_take uset)))))

(define-isl-function union-set-coalesce %isl-union-set-coalesce
  (:give union-set)
  (:take union-set))

(define-isl-function union-set-lexmin %isl-union-set-lexmin
  (:give union-set)
  (:take union-set))

(define-isl-function union-set-lexmax %isl-union-set-lexmax
  (:give union-set)
  (:take union-set))
;;;; BasicMap
(export 'basic-map-dim)
(defun basic-map-dim (bmap type)
  (%isl-basic-map-dim (basic-map-handle bmap) type))
  
(define-isl-function basic-map-empty %isl-basic-map-empty
   (:give basic-map)
   (:take space))

(define-isl-function basic-map-universe %isl-basic-map-universe
  (:give basic-map)
  (:take space))

(define-isl-function basic-map-from-affine %isl-basic-map-from-aff
  (:give basic-map)
  (:take affine))

(define-isl-function basic-map-intersect %isl-basic-map-intersect
  (:give basic-map)
  (:take basic-map)
  (:take basic-map))

(define-isl-function basic-map-add-constraint %isl-basic-map-add-constraint
  (:give basic-map)
  (:take basic-map)
  (:take constraint))

(define-isl-function basic-map-insert-dimension %isl-basic-map-insert-dims
  (:give basic-map)
  (:take basic-map)
  (:keep dim-type)
  (:keep integer position)
  (:keep integer n))

(define-isl-function basic-map-get-constraint-list %isl-basic-map-get-constraint-list
  (:give constraint-list)
  (:keep basic-map))

(define-isl-function basic-set-get-constraint-list %isl-basic-set-get-constraint-list
  (:give constraint-list)
  (:keep basic-set))

(define-isl-function basic-set-apply %isl-basic-set-apply
  (:give basic-set)
  (:take basic-set)
  (:take basic-map))

(define-isl-function basic-map-wrap %isl-basic-map-wrap
  (:give basic-set)
  (:take basic-map))
;;;; Map
(define-isl-function map-empty %isl-map-empty
  (:give map)
  (:take space))

(define-isl-function map-affine-hull %isl-map-affine-hull
  (:give basic-map)
  (:take map))

(define-isl-function set-affine-hull %isl-set-affine-hull
  (:give basic-set)
  (:take set))

(define-isl-function union-map-is-single-valued %isl-union-map-is-single-valued
  (:give boolean)
  (:keep union-map))

(define-isl-function union-map-params %isl-union-map-params
  (:give set)
  (:take union-map))
;; Preimage
(define-isl-function union-map-preimage-range-multi-aff %isl-union-map-preimage-range-multi-aff
  (:give union-map)
  (:take union-map)
  (:take multi-aff))

(define-isl-function union-map-preimage-domain-multi-aff %isl-union-map-preimage-domain-multi-aff
  (:give union-map)
  (:take union-map)
  (:take multi-aff))

(define-isl-function union-map-preimage-range-pw-multi-aff %isl-union-map-preimage-range-pw-multi-aff
  (:give union-map)
  (:take union-map)
  (:take pw-multi-aff))

(define-isl-function union-map-preimage-domain-pw-multi-aff %isl-union-map-preimage-domain-pw-multi-aff
  (:give union-map)
  (:take union-map)
  (:take pw-multi-aff))

(define-isl-function union-set-preimage-multi-aff %isl-union-set-preimage-multi-aff
  (:give union-set)
  (:take union-set)
  (:take multi-aff))

(define-isl-function map-preimage-range-multi-aff %isl-map-preimage-range-multi-aff
  (:give map)
  (:take map)
  (:take multi-aff))

(define-isl-function map-preimage-domain-multi-aff %isl-map-preimage-domain-multi-aff
  (:give map)
  (:take map)
  (:take multi-aff))

(define-isl-function map-preimage-range-pw-multi-aff %isl-map-preimage-range-pw-multi-aff
  (:give map)
  (:take map)
  (:take pw-multi-aff))

(define-isl-function map-preimage-domain-pw-multi-aff %isl-map-preimage-domain-pw-multi-aff
  (:give map)
  (:take map)
  (:take pw-multi-aff))

(define-isl-function set-preimage-multi-aff %isl-set-preimage-multi-aff
  (:give set)
  (:take set)
  (:take multi-aff))

(define-isl-function map-universe %isl-map-universe
  (:give map)
  (:take space))
(define-isl-function basic-map-map %isl-map-from-basic-map
  (:give map)
  (:take basic-map))
(define-isl-function map-from-domain %isl-map-from-domain
  (:give map)
  (:take set))
(define-isl-function map-domain %isl-map-domain
  (:give set)
  (:take map))
(define-isl-function map-range %isl-map-range
  (:give set)
  (:take map))
(define-isl-function map-flat-product %isl-map-flat-product
  (:give map)
  (:take map)
  (:take map))
(define-isl-function map-product %isl-map-product
  (:give map)
  (:take map)
  (:take map))
(define-isl-function map-uncurry %isl-map-uncurry
  (:give map)
  (:take map))
(define-isl-function map-get-basic-map-list %isl-map-get-basic-map-list
  (:give basic-map-list)
  (:keep map))
(define-isl-function map-compute-divs %isl-map-compute-divs
  (:give map)
  (:take map))
(define-isl-function map-get-space %isl-map-get-space
  (:give space)
  (:keep map))
(export 'map-equate)
(defun map-equate (map type1 pos1 type2 pos2)
  (%make-map (%isl-map-equate (map-handle (__isl_take map)) type1 pos1 type2 pos2)))

(export 'map-move-dims)
(defun map-move-dims (map dst-type dst-pos src-type src-pos n)
  (%make-map (%isl-map-move-dims (map-handle (__isl_take map)) dst-type dst-pos src-type src-pos n)))
(export 'map-dim)
(defun map-dim (map type)
  (%isl-map-dim (map-handle map) type))
(export 'map-dim-min)
(defun map-dim-min (map pos)
  (%make-map (%isl-map-dim-min (map-handle (__isl_take map)) pos)))
(export 'map-dim-max)
(defun map-dim-max (map pos)
  (%make-map (%isl-map-dim-max (map-handle (__isl_take map)) pos)))
(export 'map-project-out)
(defun map-project-out (map type from to)
  (%make-map (%isl-map-project-out (map-handle (__isl_take map)) type from to)))
(export 'map-is-equal)
(defun map-is-equal (m1 m2)
  (eql :bool-true (%isl-map-is-equal (map-handle m1) (map-handle m2))))

(define-isl-function map-wrap %isl-map-wrap
  (:give set)
  (:take map))

(define-isl-function map-align-params %isl-map-align-params
  (:give map)
  (:take map)
  (:take space))
(export 'map-set-tuple-id)
(defun map-set-tuple-id (map type id)
  (%make-map (%isl-map-set-tuple-id (map-handle (__isl_take map)) type (identifier-handle (__isl_take id)))))
(export 'map-get-tuple-id)
(defun map-get-tuple-id (map type)
  (%make-identifier (%isl-map-get-tuple-id (map-handle (__isl_take map)) type)))
(export 'map-get-tuple-name)
(defun map-get-tuple-name (map type)
  (%isl-map-get-tuple-name (map-handle map) type))
;;;; UnionMap
(define-isl-function union-map-empty %isl-union-map-empty
  (:give union-map)
  (:take space))
(define-isl-function union-map-wrap %isl-union-map-wrap
  (:give union-set)
  (:take union-map))
(define-isl-function union-map-universe %isl-union-map-universe
  (:give union-map)
  (:take space))
(define-isl-function union-map-uncurry %isl-union-map-uncurry
  (:give union-map)
  (:take union-map))
(define-isl-function basic-map-union-map %isl-union-map-from-basic-map
  (:give union-map)
  (:take basic-map))

(define-isl-function map-union-map %isl-union-map-from-map
  (:give union-map)
  (:take map))

(define-isl-function union-map-reverse %isl-union-map-reverse
  (:give union-map)
  (:take union-map))

(define-isl-function map-reverse %isl-map-reverse
  (:give map)
  (:take map))

(define-isl-function map-reset-tuple-id %isl-map-reset-tuple-id
  (:give map)
  (:take map)
  (:take dim-type))

(define-isl-function map-apply-range %isl-map-apply-range
  (:give map)
  (:take map)
  (:take map))

(define-isl-function map-apply-domain %isl-map-apply-domain
  (:give map)
  (:take map)
  (:take map))
             
(define-isl-function union-map-gist-range %isl-union-map-gist-range
  (:give union-map)
  (:take union-map)
  (:take union-set))
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
  (def union-map-domain-product %isl-union-map-domain-product)
  (def union-map-flat-domain-product %isl-union-map-flat-domain-product)
  (def union-map-flat-range-product %isl-union-map-flat-range-product)
  (def union-map-lex-lt-union-map %isl-union-map-lex-lt-union-map)
  (def union-map-lex-le-union-map %isl-union-map-lex-le-union-map)
  (def union-map-lex-gt-union-map %isl-union-map-lex-gt-union-map)
  (def union-map-lex-ge-union-map %isl-union-map-lex-ge-union-map))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give boolean)
                (:take union-map)
                (:take union-map))))
  (def union-map-equalp %isl-union-map-is-equal)
  (def union-map-subsetp %isl-union-map-is-subset)
  (def union-map-strict-subset-p %isl-union-map-is-strict-subset))

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

(export 'union-map-dim)
(defun union-map-dim (umap type)
  (%isl-union-map-dim (union-map-handle (__isl_take umap)) type))

(define-isl-function union-map-get-map-list %isl-union-map-get-map-list
  (:give map-list)
  (:keep union-map))

(export 'union-map-n-map)
(defun union-map-n-map (umap)
  (%isl-union-map-n-map (union-map-handle umap)))

(define-isl-function map-from-union-map %isl-map-from-union-map
  (:give map)
  (:take union-map))

(define-isl-function union-map-coalesce %isl-union-map-coalesce
  (:give union-map)
  (:take union-map))

(define-isl-function union-map-detect-equalities %isl-union-map-detect-equalities
  (:give union-map)
  (:take union-map))

(define-isl-function union-set-detect-equalities %isl-union-set-detect-equalities
  (:give union-set)
  (:take union-set))

(define-isl-function union-map-get-space %isl-union-map-get-space
  (:give space)
  (:keep union-map))
;;;; ScheduleConstraints
(define-isl-function schedule-constraints-on-domain %isl-schedule-constraints-on-domain
  (:give schedule-constraints)
  (:take union-set))

(define-isl-function schedule-constraints-get-context %isl-schedule-constraints-get-context
  (:give context)
  (:keep schedule-constraints))

(define-isl-function schedule-constraints-set-context %isl-schedule-constraints-set-context
  (:give schedule-constraints)
  (:take schedule-constraints)
  (:take set))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give schedule-constraints)
                (:take schedule-constraints)
                (:take union-map))))
  (def schedule-constraints-set-validity %isl-schedule-constraints-set-validity)
  (def schedule-constraints-set-coincidence %isl-schedule-constraints-set-coincidence)
  (def schedule-constraints-set-proximity %isl-schedule-constraints-set-proximity))

(define-isl-function schedule-constraints-set-conditional-validity %isl-schedule-constraints-set-conditional-validity
  (:give schedule-constraints)
  (:take schedule-constraints)
  (:take union-map condition)
  (:take union-map validity))
;;;; Schedule
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

(define-isl-function schedule-from-domain %isl-schedule-from-domain
  (:give schedule)
  (:take union-set))

(define-isl-function schedule-pullback-union-pw-multi-aff %isl-schedule-pullback-union-pw-multi-aff
  (:give schedule)
  (:take schedule)
  (:take union-pw-multi-aff))

(define-isl-function schedule-intersect-domain %isl-schedule-intersect-domain
  (:give schedule)
  (:take schedule)
  (:take union-set))

(export 'schedule-read-from-str)
(defun schedule-read-from-str (str)
  (%make-schedule
   (%isl-schedule-read-from-str
    (context-handle *context*)
    str)))
;;;; UnionAccess
(define-isl-function union-access-info-from-sink %isl-union-access-info-from-sink
  (:give union-access-info)
  (:take union-map))

(define-isl-function union-access-info-set-must-source %isl-union-access-info-set-must-source
  (:give union-access-info)
  (:take union-access-info)
  (:take union-map))

(define-isl-function union-access-info-set-may-source %isl-union-access-info-set-may-source
  (:give union-access-info)
  (:take union-access-info)
  (:take union-map))

;;(define-isl-function union-access-info-set-schedule %isl-union-access-info-set-schedule
;;  (:give union-access-info)
;;  (:take union-access-info)
;;  (:take schedule))

(define-isl-function union-access-info-set-schedule-map %isl-union-access-info-set-schedule-map
  (:give union-access-info)
  (:take union-access-info)
  (:take union-map))

(define-isl-function union-access-info-compute-flow %isl-union-access-info-compute-flow
  (:give union-flow)
  (:take union-access-info))

(define-isl-function union-flow-get-must-dependence %isl-union-flow-get-must-dependence
  (:give union-map)
  (:take union-flow))

(define-isl-function union-flow-get-may-dependence %isl-union-flow-get-may-dependence
  (:give union-map)
  (:take union-flow))

(define-isl-function union-access-info-set-schedule %isl-union-access-info-set-schedule
  (:give union-access-info)
  (:take union-access-info)
  (:take schedule))
;;;; MUPA
(define-isl-function multi-aff-get-aff %isl-multi-aff-get-aff
  (:give aff)
  (:take multi-aff)
  (:take fixnum))

(define-isl-function pw-multi-aff-from-map %isl-pw-multi-aff-from-map
  (:give pw-multi-aff)
  (:take map))

(define-isl-function pw-multi-aff-get-space %isl-pw-multi-aff-get-space
  (:give space)
  (:keep pw-multi-aff))

(define-isl-function union-pw-multi-aff-empty %isl-union-pw-multi-aff-empty
  (:give union-pw-multi-aff)
  (:take space))

(define-isl-function union-pw-multi-aff-from-pw-multi-aff %isl-union-pw-multi-aff-from-pw-multi-aff
  (:give union-pw-multi-aff)
  (:take pw-multi-aff))

(define-isl-function union-pw-multi-aff-union-add %isl-union-pw-multi-aff-union-add
  (:give union-pw-multi-aff)
  (:take union-pw-multi-aff)
  (:take union-pw-multi-aff))

(define-isl-function mupa-from-union-map %isl-multi-union-pw-aff-from-union-map
  (:give multi-union-pw-aff)
  (:take union-map))

(define-isl-function multi-union-pw-aff-intersect-domain %isl-multi-union-pw-aff-intersect-domain
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take union-set))

(define-isl-function multi-union-pw-aff-align-params %isl-multi-union-pw-aff-align-params
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take space))

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

(define-isl-function union-pw-aff-list-alloc %isl-union-pw-aff-list-alloc
  (:give union-pw-aff-list)
  (:parm context *context*)
  (:take fixnum))

(define-isl-function union-pw-aff-list-add %isl-union-pw-aff-list-add
  (:give union-pw-aff-list)
  (:take union-pw-aff-list)
  (:take union-pw-aff))

(define-isl-function pw-aff-from-aff %isl-pw-aff-from-aff
  (:give pw-aff)
  (:take aff))

(export 'multi-union-pw-aff-get-union-pw-aff)
(defun multi-union-pw-aff-get-union-pw-aff (mupa int)
  (%make-union-pw-aff (%isl-multi-union-pw-aff-get-union-pw-aff (multi-union-pw-aff-handle (__isl_take mupa)) int)))

(export 'multi-union-pw-aff-size)
(defun multi-union-pw-aff-size (mupa)
  (%isl-multi-union-pw-aff-size (multi-union-pw-aff-handle (__isl_take mupa))))

(define-isl-function multi-union-pw-aff-min-multi-val %isl-multi-union-pw-aff-min-multi-val
  (:give multi-val)
  (:take multi-union-pw-aff))

(export 'multi-val-get-val)
(defun multi-val-get-val (mval nth)
  (%make-value (%isl-multi-val-get-val (multi-val-handle (__isl_take mval)) nth)))

(export 'multi-val-set-val)
(defun multi-val-set-val (mval nth val)
  (%make-multi-val (%isl-multi-val-set-val (multi-val-handle (__isl_take mval)) nth (value-handle (__isl_take val)))))

(export 'multi-union-pw-aff-set-union-pw-aff)
(defun multi-union-pw-aff-set-union-pw-aff (mupa pos upa)
  (%make-multi-union-pw-aff (%isl-multi-union-pw-aff-set-union-pw-aff (multi-union-pw-aff-handle (__isl_take mupa)) pos (union-pw-aff-handle (__isl_take upa)))))

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

(define-isl-function union-pw-aff-get-space %isl-union-pw-aff-get-space
  (:give space)
  (:keep union-pw-aff))

(export 'aff-dim)
(defun aff-dim (aff type)
  (%isl-aff-dim (aff-handle aff) type))

(export 'aff-get-coefficient-val)
(defun aff-get-coefficient-val (aff type pos)
  (%make-value (%isl-aff-get-coefficient-val (aff-handle aff) type pos)))

(define-isl-function aff-get-constant-val %isl-aff-get-constant-val
  (:give value)
  (:keep aff))

(define-isl-function aff-set-coefficient-si %isl-aff-set-coefficient-si
  (:give aff)
  (:take aff)
  (:take keyword)
  (:take fixnum)
  (:take fixnum))

(defcfun ("isl_union_pw_aff_from_aff" %isl-union-pw-aff-from-aff) :pointer (x :pointer))
(define-isl-function union-pw-aff-from-aff %isl-union-pw-aff-from-aff
  (:give union-pw-aff)
  (:take aff))

(define-isl-function multi-union-pw-aff-from-union-pw-aff-list %isl-multi-union-pw-aff-from-union-pw-aff-list
  (:give multi-union-pw-aff)
  (:take space)
  (:take union-pw-aff-list))
  
(export 'pw-aff-var-on-domain)
(defun pw-aff-var-on-domain (local-space dim pos)
  (%make-pw-aff (%isl-pw-aff-var-on-domain (local-space-handle local-space) dim pos)))
  
(define-isl-function union-pw-aff-from-pw-aff %isl-union-pw-aff-from-pw-aff
  (:give union-pw-aff)
  (:take pw-aff))

(define-isl-function union-pw-aff-add %isl-union-pw-aff-add
  (:give union-pw-aff)
  (:take union-pw-aff)
  (:take union-pw-aff))

;(define-isl-function union-pw-aff-mul %isl-union-pw-aff-mul
;  (:give union-pw-aff)
;  (:take union-pw-aff)
;  (:take union-pw-aff))

(export 'multi-union-pw-aff-drop-dims)
(defun multi-union-pw-aff-drop-dims (mupa dim first n)
  (%make-multi-union-pw-aff
   (%isl-multi-union-pw-aff-drop-dims (multi-union-pw-aff-handle mupa) dim first n)))

(define-isl-function multi-union-pw-aff-range-product %isl-multi-union-pw-aff-range-product
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take multi-union-pw-aff))

(define-isl-function multi-union-pw-aff-flat-range-product %isl-multi-union-pw-aff-flat-range-product
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take multi-union-pw-aff))

(define-isl-function union-map-from-union-pw-aff %isl-union-map-from-union-pw-aff
  (:give union-map)
  (:take union-pw-aff))

(define-isl-function union-pw-aff-get-pw-aff-list %isl-union-pw-aff-get-pw-aff-list
  (:give pw-aff-list)
  (:keep union-pw-aff))

(export 'pw-aff-list-get-at)
(defun pw-aff-list-get-at (pw-aff-list n)
  (%make-pw-aff (%isl-pw-aff-list-get-at (pw-aff-list-handle pw-aff-list) n)))

(define-isl-function pw-aff-domain %isl-pw-aff-domain
  (:give set)
  (:take pw-aff))

(define-isl-function pw-aff-neg %isl-pw-aff-neg
  (:give pw-aff)
  (:take pw-aff))

(define-isl-function aff-get-space %isl-aff-get-space
  (:give space)
  (:keep aff))

(define-isl-function aff-get-local-space %isl-aff-get-local-space
  (:give local-space)
  (:keep aff))

(export 'multi-union-pw-aff-reset-tuple-id)
(defun multi-union-pw-aff-reset-tuple-id (mupa type)
  (%make-multi-union-pw-aff (%isl-multi-union-pw-aff-reset-tuple-id (multi-union-pw-aff-handle (__isl_take mupa)) type)))

(export 'multi-union-pw-aff-get-tuple-name)
(defun multi-union-pw-aff-get-tuple-name (mupa type)
  (%isl-multi-union-pw-aff-get-tuple-name (multi-union-pw-aff-handle mupa) type))

(export 'multi-union-pw-aff-get-dim-id)
(defun multi-union-pw-aff-get-dim-id (mupa type pos)
  (%make-identifier (%isl-multi-union-pw-aff-get-dim-id (multi-union-pw-aff-handle mupa) type pos)))

(export 'multi-union-pw-aff-get-dim-name)
(defun multi-union-pw-aff-get-dim-name (mupa type pos)
  (%isl-id-to-str (identifier-handle (multi-union-pw-aff-get-dim-id mupa type pos))))

(export 'multi-union-pw-aff-dim)
(defun multi-union-pw-aff-dim (mupa type)
  (%isl-multi-union-pw-aff-dim (multi-union-pw-aff-handle mupa) type))

(export 'pw-aff-get-dim-name)
(defun pw-aff-get-dim-name (pa type pos)
  (%isl-pw-aff-get-dim-name (pw-aff-handle pa) type pos))

(export 'pw-aff-dim)
(defun pw-aff-dim (pa type)
  (%isl-pw-aff-dim (pw-aff-handle pa) type))

(export 'aff-get-dim-name)
(defun aff-get-dim-name (aff type pos)
  (%isl-aff-get-dim-name (aff-handle aff) type pos))

(define-isl-function multi-union-pw-aff-union-add %isl-multi-union-pw-aff-union-add
  (:give multi-union-pw-aff)
  (:take multi-union-pw-aff)
  (:take multi-union-pw-aff))

(define-isl-function multi-union-pw-aff-get-space %isl-multi-union-pw-aff-get-space
  (:give space)
  (:keep multi-union-pw-aff))

(define-isl-function union-pw-aff-intersect-domain %isl-union-pw-aff-intersect-domain
  (:give union-pw-aff)
  (:take union-pw-aff)
  (:take union-set))

(define-isl-function multi-union-pw-aff-from-union-pw-aff %isl-multi-union-pw-aff-from-union-pw-aff
  (:give multi-union-pw-aff)
  (:take union-pw-aff))

(define-isl-function union-map-from-multi-union-pw-aff %isl-union-map-from-multi-union-pw-aff
  (:give union-map)
  (:take multi-union-pw-aff))

(define-isl-function multi-union-pw-aff-from-union-map %isl-multi-union-pw-aff-from-union-map
  (:give multi-union-pw-aff)
  (:take union-map))

(define-isl-function union-pw-aff-sub %isl-union-pw-aff-sub
  (:give union-pw-aff)
  (:take union-pw-aff)
  (:take union-pw-aff))

(define-isl-function create-empty-affine %isl-aff-zero-on-domain
  (:give affine)
  (:take local-space))

(define-isl-function create-val-affine %isl-aff-val-on-domain
  (:give affine)
  (:take local-space)
  (:take value))

(define-isl-function create-var-affine %isl-aff-var-on-domain
  (:give affine)
  (:take local-space)
  (:keep dim-type)
  (:keep integer position))

;; Binary

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give affine)
                (:take affine)
                (:take affine))))
  (def affine-add %isl-aff-add)
  (def affine-sub %isl-aff-sub)
  (def affine-mul %isl-aff-mul)
  (def affine-div %isl-aff-div))
;;;; ASTExpr

(define-isl-function ast-expr-equal-p %isl-ast-expr-is-equal
  (:give boolean)
  (:keep ast-expr)
  (:keep ast-expr))

(export '%make-ast-expr)
(defun %make-ast-expr (handle)
  (ecase (%isl-ast-expr-get-type handle)
    (:ast-expr-error (isl-error))
    (:ast-expr-op (%make-op-expr handle))
    (:ast-expr-id (%make-id-expr handle))
    (:ast-expr-int (%make-int-expr handle))))

(export '%make-op-expr)
(defun %make-op-expr (handle)
  (ecase (%isl-ast-expr-op-get-type handle)
    (:ast-expr-op-error (isl-error))
    (:ast-expr-op-and (%make-op-and handle))
    (:ast-expr-op-and-then (%make-op-and-then handle))
    (:ast-expr-op-or (%make-op-or handle))
    (:ast-expr-op-or-else (%make-op-or-else handle))
    (:ast-expr-op-max (%make-op-max handle))
    (:ast-expr-op-min (%make-op-min handle))
    (:ast-expr-op-minus (%make-op-minus handle))
    (:ast-expr-op-add (%make-op-add handle))
    (:ast-expr-op-sub (%make-op-sub handle))
    (:ast-expr-op-mul (%make-op-mul handle))
    (:ast-expr-op-div (%make-op-div handle))
    (:ast-expr-op-fdiv-q (%make-op-fdiv-q handle))
    (:ast-expr-op-pdiv-q (%make-op-pdiv-q handle))
    (:ast-expr-op-pdiv-r (%make-op-pdiv-r handle))
    (:ast-expr-op-zdiv-r (%make-op-zdiv-r handle))
    (:ast-expr-op-cond (%make-op-cond handle))
    (:ast-expr-op-select (%make-op-select handle))
    (:ast-expr-op-eq (%make-op-eq handle))
    (:ast-expr-op-le (%make-op-le handle))
    (:ast-expr-op-lt (%make-op-lt handle))
    (:ast-expr-op-ge (%make-op-ge handle))
    (:ast-expr-op-gt (%make-op-gt handle))
    (:ast-expr-op-call (%make-op-call handle))
    (:ast-expr-op-access (%make-op-access handle))
    (:ast-expr-op-member (%make-op-member handle))
    (:ast-expr-op-address-of (%make-op-address-of handle))))

(define-isl-function op-expr-get-n-arg %isl-ast-expr-op-get-n-arg
  (:give (unsigned-byte 32))
  (:keep ast-expr))

(define-isl-function op-expr-get-op-arg %isl-ast-expr-get-op-arg
  (:give ast-expr)
  (:keep ast-expr)
  (:keep fixnum))

(export 'op-expr-get-list-args)
(defun op-expr-get-list-args (ast)
  ;; assert type ast-exp op
  (let ((n (op-expr-get-n-arg ast)))
    (loop for i below n collect
      (op-expr-get-op-arg ast i))))

(define-isl-function id-expr-get-id %isl-ast-expr-get-id
  (:give identifier)
  (:keep id-expr))
;; INT

(define-isl-function int-expr-get-value %isl-ast-expr-get-val
  (:give value)
  (:keep int-expr))

;; Creation of an ast expr
;; Problably not useful unless on some specific usecases, so not everything is implemented
(define-isl-function create-ast-expr-from-val %isl-ast-expr-from-val
  (:give ast-expr)
  (:take value))

(define-isl-function ast-expr-from-id %isl-ast-expr-from-id
  (:give ast-expr)
  (:take identifier))

(define-isl-function create-ast-expr-from-add %isl-ast-expr-add
  (:give ast-expr)
  (:take ast-expr)
  (:take ast-expr))
;;;; ASTNode
(export 'ast-node-get-type)
(defun ast-node-get-type (ast-node)
  (%isl-ast-node-get-type (ast-node-handle ast-node)))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep for-node))))
  (def for-node-get-iterator %isl-ast-node-for-get-iterator)
  (def for-node-get-init %isl-ast-node-for-get-init)
  (def for-node-get-cond %isl-ast-node-for-get-cond)
  (def for-node-get-inc %isl-ast-node-for-get-inc))

(define-isl-function for-node-get-body %isl-ast-node-for-get-body
  (:give ast-node)
  (:keep for-node))

(macrolet ((def (name impl)
             `(define-isl-function ,name ,impl
                (:give ast-expr)
                (:keep if-node))))
  (def if-node-get-cond %isl-ast-node-if-get-cond)
  (def if-node-get-then %isl-ast-node-if-get-then)
  (def if-node-get-else %isl-ast-node-if-get-else))

(define-isl-function if-node-has-else %isl-ast-node-if-has-else
  (:give boolean)
  (:keep ast-node))

(define-isl-function block-node-getlist %isl-ast-node-block-get-children
  (:give ast-node-list)
  (:keep block-node))

(define-isl-function user-node-get-expr %isl-ast-node-user-get-expr
  (:give ast-expr)
  (:keep user-node))

(define-isl-function user-get-expr %isl-ast-node-user-get-expr
  (:give ast-expr)
  (:keep ast-node))

(export '%make-ast-node)
(defun %make-ast-node (handle)
  (ecase (%isl-ast-node-get-type handle)
    ((:ast-node-error :ast-expr-error) (isl-error))
    (:ast-node-for (%make-for-node handle))
    (:ast-node-if (%make-if-node handle))
    (:ast-node-block (%make-block-node handle))
    (:ast-node-mark (%make-mark-node handle))
    (:ast-node-user (%make-user-node handle))))

(define-isl-function ast-node-get-annotation %isl-ast-node-get-annotation
  (:give identifier)
  (:keep ast-node))
;;;; ASTBuild
(define-isl-function create-ast-build %isl-ast-build-alloc
  (:give ast-build)
  (:parm context *context*))

(define-isl-function ast-build-expr-from-set %isl-ast-build-expr-from-set
  (:give ast-expr)
  (:keep ast-build)
  (:take set))

(define-isl-function ast-build-expr-from-pw-aff %isl-ast-build-expr-from-pw-aff
  (:give ast-expr)
  (:keep ast-build)
  (:take pw-aff))

(define-isl-function ast-build-from-context %isl-ast-build-from-context
  (:give ast-build)
  (:take set))

(define-isl-function ast-build-set-options %isl-ast-build-set-options
  (:give ast-build)
  (:take ast-build)
  (:take union-map))

(define-isl-function ast-build-set-iterators %isl-ast-build-set-iterators
  (:give ast-build)
  (:take ast-build)
  (:take identifier-list))
;;;; ScheduleNode

(export '%make-schedule-node)
(defun %make-schedule-node (handle)
  (ecase (%isl-schedule-node-get-type handle)
    (:Schedule-Node-Leaf (%make-schedule-node-leaf handle))
    (:Schedule-Node-Filter (%make-schedule-node-filter handle))
    (:Schedule-Node-Sequence (%make-schedule-node-sequence handle))
    (:Schedule-Node-Band (%make-schedule-node-band handle))
    (:Schedule-Node-Domain (%make-schedule-node-domain handle))
    (:Schedule-Node-Expansion (%make-schedule-node-expansion handle))
    (:Schedule-Node-Extension (%make-schedule-node-extension handle))
    (:Schedule-Node-Mark (%make-schedule-node-mark handle))
    (:Schedule-Node-Set (%make-schedule-node-set handle))
    (:Schedule-Node-Context (%make-schedule-node-context handle))
    (:Schedule-Node-Guard (%make-schedule-node-guard handle))
    (:Schedule-Node-Error (%make-schedule-node-error handle))))

(define-isl-function schedule-get-root %isl-schedule-get-root
  (:give schedule-node)
  (:keep schedule))

(define-isl-function schedule-node-domain-get-domain %isl-schedule-node-domain-get-domain
  (:give union-set)
  (:keep schedule-node))

(define-isl-function schedule-node-graft-after %isl-schedule-node-graft-after
  (:give schedule-node)
  (:take schedule-node)
  (:take schedule-node))

(define-isl-function schedule-node-graft-before %isl-schedule-node-graft-before
  (:give schedule-node)
  (:take schedule-node)
  (:take schedule-node))

(define-isl-function schedule-insert-partial-schedule %isl-schedule-insert-partial-schedule
  (:give schedule)
  (:take schedule)
  (:take multi-union-pw-aff))

(define-isl-function schedule-node-insert-partial-schedule %isl-schedule-node-insert-partial-schedule
  (:give schedule-node)
  (:take schedule-node)
  (:take multi-union-pw-aff))

(define-isl-function schedule-node-from-domain %isl-schedule-node-from-domain
  (:give schedule-node)
  (:take union-set))

(define-isl-function schedule-node-get-schedule %isl-schedule-node-get-schedule
  (:give schedule)
  (:keep schedule-node))

(define-isl-function schedule-node-band-get-partial-schedule %isl-schedule-node-band-get-partial-schedule
  (:give multi-union-pw-aff)
  (:keep schedule-node))

(define-isl-function schedule-node-band-get-partial-schedule-union-map %isl-schedule-node-band-get-partial-schedule-union-map
  (:give union-map)
  (:keep schedule-node))

(define-isl-function schedule-node-get-domain %isl-schedule-node-get-domain
  (:give union-set)
  (:keep schedule-node))

(define-isl-function schedule-node-band-get-space %isl-schedule-node-band-get-space
  (:give space)
  (:keep schedule-node))

(export 'schedule-node-get-child)
(defun schedule-node-get-child (schedule-node n)
  (%make-schedule-node (%isl-schedule-node-get-child (schedule-node-handle schedule-node) n)))

(export 'schedule-node-get-ancestor)
(defun schedule-node-get-ancestor (schedule-node generation)
  (%make-schedule-node (%isl-schedule-node-ancestor (schedule-node-handle schedule-node) generation)))

(export 'schedule-node-get-children)
(defun schedule-node-get-children (schedule-node)
  (declare (type schedule-node schedule-node))
  (when (eql :bool-true (%isl-schedule-node-has-children (schedule-node-handle schedule-node)))
    (let ((n (%isl-schedule-node-n-children (schedule-node-handle schedule-node))))
      (loop for nth upfrom 0 below n
	    collect (%make-schedule-node (%isl-schedule-node-child (schedule-node-handle (__isl_take schedule-node)) nth))))))

(export 'schedule-node-get-type)
(defun schedule-node-get-type (schedule-node)
  (declare (type schedule-node schedule-node))
  (%isl-schedule-node-get-type (schedule-node-handle schedule-node)))

(define-isl-function schedule-insert-sequence %isl-schedule-node-insert-sequence
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set-list))

(define-isl-function schedule-node-insert-mark %isl-schedule-node-insert-mark
  (:give schedule-node)
  (:take schedule-node)
  (:take identifier))

(define-isl-function schedule-node-delete %isl-schedule-node-delete
  (:give schedule-node)
  (:take schedule-node))

(define-isl-function schedule-node-first-child %isl-schedule-node-first-child
  (:give schedule-node)
  (:take schedule-node))

(define-isl-function schedule-node-band-set-ast-build-options %isl-schedule-node-band-set-ast-build-options
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set))

(define-isl-function schedule-node-band-tile %isl-schedule-node-band-tile
  (:give schedule-node)
  (:take schedule-node)
  (:take multi-val))

(define-isl-function schedule-node-band-scale %isl-schedule-node-band-scale
  (:give schedule-node)
  (:take schedule-node)
  (:take multi-val))

(define-isl-function schedule-node-band-scale-down %isl-schedule-node-band-scale-down
  (:give schedule-node)
  (:take schedule-node)
  (:take multi-val))

(define-isl-function schedule-node-band-mod %isl-schedule-node-band-mod
  (:give schedule-node)
  (:take schedule-node)
  (:take multi-val))

(define-isl-function schedule-node-band-get-ast-isolate-option %isl-schedule-node-band-get-ast-isolate-option
  (:give set)
  (:keep schedule-node))

(export 'schedule-node-band-member-set-isolate-ast-loop-type)
(defun schedule-node-band-member-set-isolate-ast-loop-type (schedule-node-band pos type)
  (%make-schedule-node (%isl-schedule-node-band-member-set-isolate-ast-loop-type (schedule-node-handle (__isl_take schedule-node-band)) pos type)))

(export 'schedule-node-band-member-set-ast-loop-type)
(defun schedule-node-band-member-set-ast-loop-type (schedule-node-band pos type)
  (%make-schedule-node (%isl-schedule-node-band-member-set-ast-loop-type (schedule-node-handle (__isl_take schedule-node-band)) pos type)))

(define-isl-function schedule-node-mark-get-id %isl-schedule-node-mark-get-id
  (:give identifier)
  (:keep schedule-node))

(export 'schedule-node-get-schedule-depth)
(defun schedule-node-get-schedule-depth (node)
  (declare (type schedule-node node))
  (%isl-schedule-node-get-schedule-depth (schedule-node-handle node)))

(export 'schedule-node-band-split)
(defun schedule-node-band-split (node pos)
  (declare (type schedule-node node))
  (%make-schedule-node (%isl-schedule-node-band-split (schedule-node-handle (__isl_take node)) pos)))

(define-isl-function schedule-node-parent %isl-schedule-node-parent
  (:give schedule-node)
  (:take schedule-node))

(export 'schedule-node-band-member-set-coincident)
(defun schedule-node-band-member-set-coincident (band pos val)
  (%make-schedule-node (%isl-schedule-node-band-member-set-coincident (schedule-node-handle (__isl_take band)) pos val)))

(export 'schedule-node-band-set-permutable)
(defun schedule-node-band-set-permutable (band val)
  (%make-schedule-node (%isl-schedule-node-band-set-permutable (schedule-node-handle (__isl_take band)) val)))

(define-isl-function schedule-node-band-sink %isl-schedule-node-band-sink
  (:give schedule-node)
  (:take schedule-node))

(export 'schedule-node-is-equal)
(defun schedule-node-is-equal (band1 band2)
  (declare (type schedule-node band1 band2))
  (%isl-schedule-node-is-equal (schedule-node-handle band1) (schedule-node-handle band2)))

(define-isl-function schedule-node-get-prefix-schedule-relation %isl-schedule-node-get-prefix-schedule-relation
  (:give union-map)
  (:keep schedule-node))

(define-isl-function schedule-node-get-prefix-schedule-union-map %isl-schedule-node-get-prefix-schedule-union-map
  (:give union-map)
  (:keep schedule-node))

(define-isl-function schedule-node-get-prefix-schedule-multi-union-pw-aff %isl-schedule-node-get-prefix-schedule-multi-union-pw-aff
  (:give multi-union-pw-aff)
  (:keep schedule-node))

(define-isl-function schedule-node-get-subtree-expansion %isl-schedule-node-get-subtree-expansion
  (:give union-map)
  (:keep schedule-node))

(define-isl-function schedule-node-insert-filter %isl-schedule-node-insert-filter
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set))

(define-isl-function schedule-node-insert-sequence %isl-schedule-node-insert-sequence
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set-list))

(define-isl-function schedule-node-insert-set %isl-schedule-node-insert-set
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set-list))

(define-isl-function schedule-node-filter-get-filter %isl-schedule-node-filter-get-filter
  (:give union-set)
  (:keep schedule-node))

(define-isl-function schedule-node-next-sibling %isl-schedule-node-next-sibling
  (:give schedule-node)
  (:take schedule-node))

(define-isl-function schedule-node-order-before %isl-schedule-node-order-before
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set))

(define-isl-function schedule-node-order-after %isl-schedule-node-order-after
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set))

(export 'schedule-node-sequence-splice-child)
(defun schedule-node-sequence-splice-child (node pos)
  (%make-schedule-node (%isl-schedule-node-sequence-splice-child (schedule-node-handle (__isl_take node)) pos)))

(define-isl-function schedule-node-cut %isl-schedule-node-cut
  (:give schedule-node)
  (:take schedule-node))
;;;; AST
(define-isl-function ast-build-node-from-schedule %isl-ast-build-node-from-schedule
   (:give ast-node)
   (:take ast-build)
   (:take schedule))

(define-isl-function ast-build-node-from-schedule-map %isl-ast-build-node-from-schedule-map
  (:give ast-node)
  (:take ast-build)
  (:take union-map))
;;;; Printer
(define-isl-object isl-printer :free %isl-printer-free)

(define-isl-function isl-printer-to-str %isl-printer-to-str
  (:give isl-printer)
  (:parm context *context*))
;;;; Matrix
(export 'basic-map-equalities-matrix)
(defun basic-map-equalities-matrix (bmap &key (order (list :dim-cst :dim-param :dim-in :dim-out :dim-div)))
  (assert (= 5 (length order)))
  (%make-mat (apply #'%isl-basic-map-equalities-matrix (basic-map-handle bmap) order)))
(export 'basic-map-inequalities-matrix)
(defun basic-map-inequalities-matrix (bmap &key (order (list :dim-cst :dim-param :dim-in :dim-out :dim-div)))
  (assert (= 5 (length order)))
  (%make-mat (apply #'%isl-basic-map-inequalities-matrix (basic-map-handle bmap) order)))
(export 'basic-set-equalities-matrix)
(defun basic-set-equalities-matrix (bset &key (order (list :dim-cst :dim-param :dim-set :dim-div)))
  (assert (= 4 (length order)))
  (%make-mat (apply #'%isl-basic-set-equalities-matrix (basic-set-handle bset) order)))
(export 'basic-set-inequalities-matrix)
(defun basic-set-inequalities-matrix (bset &key (order (list :dim-cst :dim-param :dim-set :dim-div)))
  (assert (= 4 (length order)))
  (%make-mat (apply #'%isl-basic-set-inequalities-matrix (basic-set-handle bset) order)))
(export 'basic-set-from-constraint-matrices)
(defun basic-set-from-constraint-matrices (space eq ineq &key (order (list :dim-cst :dim-param :dim-set :dim-div)))
  (assert (= 4 (length order)))
  (%make-basic-set (apply #'%isl-basic-set-from-constraint-matrices (space-handle space) (mat-handle eq) (mat-handle ineq) order)))
(export 'basic-map-from-constraint-matrices)
(defun basic-map-from-constraint-matrices (space eq ineq &key (order (list :dim-cst :dim-param :dim-in :dim-out :dim-div)))
  (assert (= 5 (length order)))
  (%make-basic-map (apply #'%isl-basic-map-from-constraint-matrices (space-handle space) (mat-handle eq) (mat-handle ineq) order)))

(export 'mat-ref)
(defun mat-ref (mat row col)
  (assert (>= row 0))
  (assert (>= col 0))
  (assert (< row (%isl-mat-rows (mat-handle mat))))
  (assert (< col (%isl-mat-cols (mat-handle mat))))
  (let ((val (%isl-mat-get-element-val (mat-handle mat) row col)))
    (prog1 (%isl-val-get-num-si val) (%isl-val-free val))))

(defun (setf mat-ref) (value mat row col)
  (declare (type integer value))
  (assert (>= row 0))
  (assert (>= col 0))
  (assert (< row (%isl-mat-rows (mat-handle mat))))
  (assert (< col (%isl-mat-cols (mat-handle mat))))
  (%isl-mat-set-element-si (mat-handle mat) row col value))

(export 'mat-rows)
(defun mat-rows (mat) (%isl-mat-rows (mat-handle mat)))
(export 'mat-cols)
(defun mat-cols (mat) (%isl-mat-cols (mat-handle mat)))
