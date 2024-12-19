(defpackage :caten/polyhedral/ast
  (:use :cl)
  (:export
   #:parse-isl-ast
   #:astblock-body
   #:user-name #:user-args
   #:expr-op #:expr-args
   #:astfor-idx #:astfor-from #:astfor-to #:astfor-by #:astfor-body #:astfor-scope
   #:astif-condition #:astif-then-node #:astif-else-node
   #:ASTBlock #:User #:Expr #:ASTFor #:AstIf))

(in-package :caten/polyhedral/ast)

(eval-when (:execute :compile-toplevel :load-toplevel)
  (defstruct (ASTBlock
              (:constructor make-block (body)))
    (body body :type list))

  (defstruct (User
              (:constructor make-user (name args)))
    "T_name(index)"
    (name name :type string) (args args :type list))

  (defstruct (Expr
              (:constructor make-expr (op &rest args)))
    (op op :type keyword) (args args :type list))
  
  (defstruct (ASTFor
              (:constructor make-for (idx from to by body)))
    (idx idx :type string)
    (from from :type Expr)
    (to to :type Expr)
    (by by :type Expr)
    (body body :type (or ASTBlock User ASTFor ASTIF))
    (scope :local :type (member :local :global)))

  (defstruct (AstIf
              (:constructor make-if (condition then-node else-node)))
    (condition condition :type Expr)
    (then-node then-node :type (or ASTBlock User ASTFOR ASTIF))
    (else-node else-node :type (or ASTBlock User ASTFOR ASTIF null))))

(declaim (ftype (function (cffi:foreign-pointer) t) parse-isl-ast))
(defun parse-isl-ast (ast)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-node-get-type ast)))
    (ecase type
      (:ast-node-error (isl::isl-error))
      (:ast-node-for   (parse-isl-ast-for ast))
      (:ast-node-if    (parse-isl-ast-if ast))
      (:ast-node-block (parse-isl-ast-block ast))
      (:ast-node-mark  (parse-isl-ast-mark ast))
      (:ast-node-user  (parse-isl-ast-user ast)))))

(declaim (ftype (function (cffi:foreign-pointer) ASTBlock) parse-isl-ast-block))
(defun parse-isl-ast-block (ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((children (isl::%isl-ast-node-block-get-children ast))
	 (n        (isl::%isl-ast-node-list-n-ast-node children)))
    (make-block
     (loop for i upfrom 0 below n
	   for child = (isl::%isl-ast-node-list-get-at children i)
	   collect
	   (parse-isl-ast child)))))

(declaim (ftype (function (cffi:foreign-pointer) User) parse-isl-ast-user))
(defun parse-isl-ast-user (ast)
  (declare (type cffi:foreign-pointer ast))
  (let ((expr (isl::%isl-ast-node-user-get-expr ast)))
    (let* ((first-expr (isl::%isl-ast-expr-op-get-arg expr 0))
	   (n          (isl::%isl-ast-expr-get-op-n-arg expr))
	   (id         (isl::%isl-ast-expr-id-get-id first-expr))
	   (name       (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	   (args       (loop for i upfrom 1 below n
			     collect
			     (parse-isl-expr (isl::%isl-ast-expr-op-get-arg expr i)))))
      (make-user name args))))

(declaim (ftype (function (cffi:foreign-pointer) t) parse-isl-ast-mark))
(defun parse-isl-ast-mark (ast)
  (let* ((mark (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id ast))))
         (user (parse-isl-ast (isl::%isl-ast-node-mark-get-node ast))))
    (typecase user
      (AstFor
       (when (string= mark "parallel")
         (setf (astfor-scope user) :global)))
      (otherwise
       (warn "mark: ignored the mark ~a for ~a" mark user)))
    user))

(declaim (ftype (function ((or cffi:foreign-pointer isl:ast-node)) (values Expr &optional)) parse-isl-expr))
(defun parse-isl-expr (ast)
  (declare (type (or cffi:foreign-pointer isl:ast-node) ast))
  (let* ((ast (if (isl:ast-node-p ast)
		  (isl::ast-node-handle ast)
		  ast))
	 (type (isl::%isl-ast-expr-get-type ast)))
    (ecase type
      (:ast-expr-error (isl::isl-error))
      (:ast-expr-id
       (let* ((id (isl::%isl-ast-expr-id-get-id ast))
	      (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id))))
	 (declare (type string name))
         (make-expr :const (intern name))))
      (:ast-expr-int
       (let* ((id (isl::%isl-ast-expr-int-get-val ast))
	      (num (isl::%isl-val-get-d id)))
	 (declare (type number num))
	 (let ((num (round num)))
           (make-expr :const num))))
      (:ast-expr-op
       (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
	      (args (loop for nth upfrom 0 below n-arg
			  collect (parse-isl-expr (isl::%isl-ast-expr-op-get-arg ast nth))))
	      (op-type (isl::%isl-ast-expr-op-get-type ast)))
	 (flet ((->expr (lhs rhs)
		  (assert (not (eql op-type :ast-expr-op-error)) () ":isl_ast_expr_op_error")
		  (ecase op-type
		    (:ast-expr-op-and (make-expr :and lhs rhs))
		    (:ast-expr-op-and-then (make-expr :and lhs rhs))
		    (:ast-expr-op-or (make-expr :or lhs rhs))
		    (:ast-expr-op-or-else (make-expr :or lhs rhs))
		    (:ast-expr-op-max (make-expr :max lhs rhs))
		    (:ast-expr-op-min  (make-expr :min lhs rhs))
		    (:ast-expr-op-minus (make-expr :neg lhs)) ;; (- a)
		    (:ast-expr-op-add (make-expr :add lhs rhs))
		    (:ast-expr-op-sub (make-expr :sub lhs rhs))
		    (:ast-expr-op-mul (make-expr :mul lhs rhs))
		    (:ast-expr-op-div (make-expr :idiv lhs rhs))		 
		    (:ast-expr-op-fdiv-q (make-expr :idiv lhs rhs))
		    (:ast-expr-op-pdiv-q (make-expr :idiv lhs rhs))
		    (:ast-expr-op-pdiv-r (make-expr :mod lhs rhs))
		    (:ast-expr-op-zdiv-r (make-expr :mod lhs rhs))
		    ;; (:expr-op-cond)
		    (:ast-expr-op-eq (make-expr := lhs rhs))
                    ;; Rewrite LE to simplify the expression
		    (:ast-expr-op-le (make-expr :< lhs (make-expr :const (progn (assert (and (expr-p rhs) (numberp (car (expr-args rhs))))) (1+ (car (expr-args rhs)))))))
		    (:ast-expr-op-lt (make-expr :< lhs rhs)) ;; <
		    (:ast-expr-op-ge (make-expr :not (make-expr :< lhs rhs))) ;; >=
		    (:ast-expr-op-gt (make-expr :> lhs rhs)) ;; >
		    ;; (:expr-op-call)
		    ;; (:expr-op-access)
		    ;; (:expr-op-member)
		    ;; (:expr-op-address-of)
		    (otherwise
		     (error "~a is not supported by caten" op-type)))))
	   (if (= (length args) 1)
	       (->expr (car args) nil)
	       (case op-type
		 (:ast-expr-op-select
		  (assert (= (length args) 3))
                  (make-expr :where (car args) (second args) (third args)))
		 (otherwise
		  (reduce #'->expr args))))))))))

(declaim (ftype (function (cffi:foreign-pointer) ASTFor) parse-isl-ast-for))
(defun parse-isl-ast-for (ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((iter (isl::%isl-ast-node-for-get-iterator ast))
	 (id (isl::%isl-ast-expr-get-id iter))
	 (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	 (from (parse-isl-expr (isl::%isl-ast-node-for-get-init ast)))
	 (by (parse-isl-expr (isl::%isl-ast-node-for-get-inc ast)))
	 (to (parse-isl-expr (isl::%isl-ast-node-for-get-cond ast)))
	 (body (parse-isl-ast (isl::%isl-ast-node-for-get-body ast))))
    (make-for name from to by body)))

(declaim (ftype (function (cffi:foreign-pointer) AstIf) parse-isl-ast-if))
(defun parse-isl-ast-if (ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((condition
	   (parse-isl-expr (isl::%isl-ast-node-if-get-cond ast)))
	 (then-node
	   (parse-isl-ast (isl::%isl-ast-node-if-get-then-node ast)))
	 (else-p (isl::%isl-ast-node-if-has-else-node ast))
	 (else-node
	   (when (eql else-p :bool-true)
	     (parse-isl-ast (isl::%isl-ast-node-if-get-else-node ast)))))
    (make-if condition then-node else-node)))
