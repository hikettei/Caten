(in-package :caten/ajit)
;; Translates From ISL_AST -> Lisp_AST
;; ~~ Lisp AST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (ASTBlock
	    (:constructor make-block (body)))
  (body body :type list))

(defstruct (User
	    (:constructor make-user (name args)))
  "T_name(index)"
  (name name :type string) (args args :type list))

(defstruct (Expr (:constructor make-expr (op x &optional (y nil) (z nil))))
  (op op) (x x) (y y) (z z))

(defmethod print-object ((expr Expr) stream)
  (if (eql (expr-op expr) :Aref)
      (format stream "{~(~a~):~(~a~)}" (expr-x expr) (buffer-dtype (expr-y expr)))
      (if (expr-z expr)
	  (format stream "~(~a~)(~(~a~), ~(~a~), ~(~a~))" (expr-op expr) (expr-x expr) (expr-y expr) (expr-z expr))
	  (if (expr-y expr)
	      (format stream "~(~a~)(~(~a~), ~(~a~))" (expr-op expr) (expr-x expr) (expr-y expr))
	      (format stream "~(~a~)(~(~a~))" (expr-op expr) (expr-x expr))))))

(defstruct (ASTFor
	    (:constructor make-for (idx from to by body execute-once)))
  (idx idx :type string)
  (from from :type Expr)
  (to to :type Expr)
  (by by :type Expr)
  (body body)
  (execute-once execute-once :type boolean))

(defstruct (AstIf
	    (:constructor make-if (condition then-node else-node)))
  (condition condition :type Expr)
  (then-node then-node)
  (else-node else-node))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun finalize-schedule (polyhedral)
  "Finalizes the polyhedral model (frees the memory), returning Lisp_AST (see above)"
  (declare (type Polyhedral polyhedral))
  (with-slots ((avm avm)) polyhedral
    (parse-isl-ast (finalize-polyhedral polyhedral))))

(declaim (ftype (function (ast-node) t) parse-isl-ast))
(defun parse-isl-ast (ast)
  (declare (type ast-node ast))
  (let ((type (isl::%isl-ast-node-get-type (isl::ast-node-handle ast))))
    (print type)
    (ecase type
      (:ast-node-error (isl::isl-error))
      (:ast-node-for   (parse-isl-ast-for ast))
      (:ast-node-if    (parse-isl-ast-if ast))
      (:ast-node-block (parse-isl-ast-block ast))
      (:ast-node-mark  (error ":isl_ast_node_mark is not supported"))
      (:ast-node-user  (parse-isl-ast-user ast)))))

(declaim (ftype (function (ast-node) ASTBlock) parse-isl-ast-block))
(defun parse-isl-ast-block (ast)
  (declare (type ast-node ast))
  (let* ((children (isl::%isl-ast-node-block-get-children (isl::ast-node-handle ast)))
	 (n        (isl::%isl-ast-node-list-n-ast-node (isl::ast-node-handle ast))))
    (make-block
     (loop for i upfrom 0 below n
	   for child = (isl::%make-ast-node (isl::%isl-ast-node-list-get-at children i))
	   collect
	   (parse-isl-ast child)))))

(declaim (ftype (function (ast-node) User) parse-isl-ast-user))
(defun parse-isl-ast-user (ast)
  (declare (type ast-node ast))
  (let ((expr (isl::%isl-ast-node-user-get-expr (isl::ast-node-handle ast))))
    (let* ((first-expr (isl::%isl-ast-expr-op-get-arg expr 0))
	   (n          (isl::%isl-ast-expr-get-op-n-arg expr))
	   (id         (isl::%isl-ast-expr-id-get-id first-expr))
	   (name       (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	   (args       (loop for i upfrom 1 below n
			     collect
			     (parse-isl-expr (isl::%make-ast-node (isl::%isl-ast-expr-op-get-arg expr i))))))
      (make-user name args))))

(declaim (ftype (function (ast-node) Expr) parse-isl-expr))
(defun parse-isl-expr (ast)
  (declare (type ast-node ast))
  (let ((type (isl::%isl-ast-expr-get-type (isl::ast-node-handle ast))))
    (ecase type
      (:ast-expr-error (isl::isl-error))
      (:ast-expr-id
       (let* ((id (isl::%isl-ast-expr-id-get-id (isl::ast-node-handle ast)))
	      (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id))))
	 (declare (type string name))
	 (make-expr :Const name)))
      (:ast-expr-int
       (let* ((id (isl::%isl-ast-expr-int-get-val (isl::ast-node-handle ast)))
	      (num (isl::%isl-val-get-d id)))
	 (declare (type number num))
	 (let ((num (round num))) (make-expr :Const num))))
      (:ast-expr-op
       (let ((n-arg (isl::%isl-ast-expr-get-op-n-arg (isl::ast-node-handle ast))))
	 (assert (or (= n-arg 1) (= n-arg 2) (= n-arg 3)) () "Assertion Failed with nargs == 1 or 2 or 3")
	 (multiple-value-bind (lhs rhs z)
	     (values
	      (parse-isl-expr (isl::%isl-ast-expr-op-get-arg ast 0))
	      (when (>= n-arg 2)
		(parse-isl-expr (isl::%isl-ast-expr-op-get-arg ast 1)))
	      (when (>= n-arg 3)
		(parse-isl-expr (isl::%isl-ast-expr-op-get-arg ast 2))))
	   (let ((op-type (isl::%isl-ast-expr-op-get-type (isl::ast-node-handle ast))))
	     (assert (not (eql op-type :ast-expr-op-error)) () ":isl_ast_expr_op_error")
	     (ecase op-type
	       (:expr-op-and (make-expr :and lhs rhs))
	       (:expr-op-and-then (make-expr :and lhs rhs))
	       (:expr-op-or (make-expr :or lhs rhs))
	       (:expr-op-or-else (make-expr :or lhs rhs))
	       (:expr-op-max (make-expr :max lhs rhs z))
	       (:expr-op-min  (make-expr :min lhs rhs z))
	       (:expr-op-minus (make-expr :neg lhs nil)) ;; (- a)
	       (:expr-op-add (make-expr :+ lhs rhs z))
	       (:expr-op-sub (make-expr :- lhs rhs z))
	       (:expr-op-mul (make-expr :* lhs rhs z))
	       (:expr-op-div (make-expr :/ lhs rhs z))		 
	       (:expr-op-fdiv-q (make-expr :/ lhs rhs))
	       (:expr-op-pdiv-q (make-expr :/ lhs rhs))
	       (:expr-op-pdiv-r (make-expr :% lhs rhs))
	       (:expr-op-zdiv-r (make-expr :% lhs rhs))
	       ;;(:expr-op-cond)
	       ;;(:expr-op-select)
	       (:expr-op-eq (make-expr :equal lhs rhs z))
	       (:expr-op-le (make-expr :<= lhs rhs z))
	       (:expr-op-lt (make-expr :< lhs rhs z))
	       (:expr-op-ge (make-expr :>= lhs rhs z))
	       (:expr-op-gt (make-expr :> lhs rhs z))
	       ;;(:expr-op-call)
	       ;;(:expr-op-access)
	       ;;(:expr-op-member)
	       ;;(:expr-op-address-of)
	       (otherwise
		(error "~a is not supported by caten" op-type))))))))))

(declaim (ftype (function (ast-node) ASTFor) parse-isl-ast-for))
(defun parse-isl-ast-for (ast)
  (declare (type ast-node ast))
  (let* ((execute-once (isl::%isl-ast-node-for-is-degenerate ast))
	 (iter (isl::%isl-ast-node-for-get-iterator ast))
	 (id (isl::%isl-ast-expr-get-id iter))
	 (name (isl::%isl-id-get-name id))
	 (from (parse-isl-expr (isl::%isl-ast-node-for-get-init ast)))
	 (by (parse-isl-expr (isl::%isl-ast-node-for-get-inc ast)))
	 (to (parse-isl-expr (isl::%isl-ast-node-for-get-cond ast)))
	 (body (parse-isl-ast (isl::%isl-ast-node-for-get-body ast))))
    (make-for name from to by body execute-once)))

(declaim (ftype (function (ast-node) AstIf) parse-isl-ast-if))
(defun parse-isl-ast-if (ast)
  (declare (type ast-node ast))
  (let* ((condition
	   (parse-isl-expr (isl::%isl-ast-node-if-get-cond ast)))
	 (then-node
	   (parse-isl-ast (isl::%isl-ast-node-if-get-then-node ast)))
	 (else-p
	   (isl::%isl-ast-node-if-has-else-node ast))
	 (else-node
	   (when else-p
	     (parse-isl-ast (isl::%isl-ast-node-if-get-else-node ast)))))
    (make-if condition then-node else-node)))
