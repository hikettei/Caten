(in-package :caten/isl)

(define-isl-object ast-expr
  :free %isl-ast-expr-free
  :copy %isl-ast-expr-copy
  :list-type ast-expr-list
  :abstract t)

(defmethod print-object ((ast-expr ast-expr) stream)
  (print-unreadable-object (ast-expr stream :type t)
    (write-string (%isl-ast-expr-to-str (ast-expr-handle ast-expr)) stream)))

(define-isl-function ast-expr-equal-p %isl-ast-expr-is-equal
  (:give boolean)
  (:keep ast-expr)
  (:keep ast-expr))

(define-isl-object op-expr
  :superclass ast-expr
  :abstract t)

(define-isl-object id-expr
  :superclass ast-expr)

(define-isl-object int-expr
  :superclass ast-expr)

(defun %make-ast-expr (handle)
  (ecase (%isl-ast-expr-get-type handle)
    (:ast-expr-error (isl-error))
    (:ast-expr-op (%make-op-expr handle))
    (:ast-expr-id (%make-id-expr handle))
    (:ast-expr-int (%make-int-expr handle))))

;; OP

(define-isl-object op-and
  :superclass op-expr)

(define-isl-object op-and-then
  :superclass op-expr)

(define-isl-object op-or
  :superclass op-expr)

(define-isl-object op-or-else
  :superclass op-expr)

(define-isl-object op-max
  :superclass op-expr)

(define-isl-object op-min
  :superclass op-expr)

(define-isl-object op-minus
  :superclass op-expr)

(define-isl-object op-add
  :superclass op-expr)

(define-isl-object op-sub
  :superclass op-expr)

(define-isl-object op-mul
  :superclass op-expr)

(define-isl-object op-div
  :superclass op-expr)

(define-isl-object op-fdiv-q
  :superclass op-expr)

(define-isl-object op-pdiv-q
  :superclass op-expr)

(define-isl-object op-pdiv-r
  :superclass op-expr)

(define-isl-object op-zdiv-r
  :superclass op-expr)

(define-isl-object op-cond
  :superclass op-expr)

(define-isl-object op-select
  :superclass op-expr)

(define-isl-object op-eq
  :superclass op-expr)

(define-isl-object op-le
  :superclass op-expr)

(define-isl-object op-lt
  :superclass op-expr)

(define-isl-object op-ge
  :superclass op-expr)

(define-isl-object op-gt
  :superclass op-expr)

(define-isl-object op-call
  :superclass op-expr)

(define-isl-object op-access
  :superclass op-expr)

(define-isl-object op-member
  :superclass op-expr)

(define-isl-object op-address-of
  :superclass op-expr)

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

(defun my-and (a b)
  (let ((aa a)
        (bb b))
    (and aa bb)))
(defun my-and-then (a b) (and a b))
(defun my-or (a b)
  (let ((aa a)
        (bb b))
    (or aa bb)))
(defun my-or-else (a b) (or a b))

;; integer division
(declaim (inline idiv))
(defun idiv (a b)
  (declare (integer a b))
  (the (values integer &optional)
       (/ a b)))

(declaim (inline my-fdiv-q my-pdiv-q my-pdiv-r my-zdiv-r))
;; isl_ast_expr_op_fdiv_q
;; Result of integer division, rounded towards negative infinity. The divisor is known to be positive.
(defun fdiv-q (a b)
  (declare (integer a)
           (type (integer 1) b))
  (values (floor a b)))

;; isl_ast_expr_op_pdiv_q
;; Result of integer division, where dividend is known to be non-negative. The divisor is known to be positive.
(defun pdiv-q (a b)
  (declare (unsigned-byte a)
           (type (integer 1) b))
  (values (floor a b)))

;; isl_ast_expr_op_pdiv_r
;; Remainder of integer division, where dividend is known to be non-negative. The divisor is known to be positive.
(defun pdiv-r (a b)
  (declare (unsigned-byte a)
           (type (integer 1) b))
  (nth-value 1 (floor a b)))

;; isl_ast_expr_op_zdiv_r
;; Equal to zero iff the remainder on integer division is zero. The divisor is known to be positive
(defun zdiv-r (a b)
  (declare (integer a)
           (type (integer 1) b))
  (nth-value 1 (floor a b)))

(defun my-cond (a b c)
  (if a b c))

(defun my-select (a b c)
  (let ((aa a)
        (bb b)
        (cc c))
    (if aa bb cc)))

;; Return the lisp operator corresponding to this cl-isl operation
(defun op-expr-get-operator (ast-expr)
  (ecase (type-of ast-expr)
    ('op-and 'my-and)
    ('op-and-then 'my-and-then)
    ('op-or 'my-or)
    ('op-or-else 'my-or-else)
    ('op-max 'max)
    ('op-min 'min)
    ('op-minus '-)
    ('op-add '+)
    ('op-sub '-)
    ('op-mul '*)
    ('op-div 'idiv)
    ('op-fdiv-q 'fdiv-q)
    ('op-pdiv-q 'pdiv-q)
    ('op-pdiv-r 'pdiv-r)
    ('op-zdiv-r 'zdiv-r)
    ('op-cond 'my-cond)
    ('op-select 'my-select)
    ('op-eq 'eql)
    ('op-le '<=)
    ('op-lt '<)
    ('op-ge '>=)
    ('op-gt '>)
    ('op-call (error "Not implemented sorry"))
    ('op-access (error "Not implemented sorry"))
    ('op-member (error "Not implemented sorry"))
    ('op-address-of (error "Not implemented sorry"))
    ('op-expr-get-list-args (error "Not implemented sorry"))))

(define-isl-function op-expr-get-n-arg %isl-ast-expr-op-get-n-arg
  (:give (unsigned-byte 32))
  (:keep ast-expr))

(define-isl-function op-expr-get-op-arg %isl-ast-expr-get-op-arg
  (:give ast-expr)
  (:keep ast-expr)
  (:keep (unsigned-byte 32)))

;;Returns a list of every son of the ast.
(defun op-expr-get-list-args (ast)
  ;; assert type ast-exp op
  (let ((n (op-expr-get-n-arg ast)))
    (loop for i below n collect
      (op-expr-get-op-arg ast i))))

;; ID

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

(define-isl-function create-ast-expr-from-add %isl-ast-expr-add
  (:give ast-expr)
  (:take ast-expr)
  (:take ast-expr))
