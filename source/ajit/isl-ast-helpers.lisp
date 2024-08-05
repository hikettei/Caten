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

(defstruct (Expr (:constructor make-expr (op x &optional (y nil))))
  (op op) (x x) (y y))

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
    (parse-isl-ast (isl-obj-ptr (finalize-polyhedral polyhedral)))))

(declaim (ftype (function (cffi:foreign-pointer) t) parse-isl-ast))
(defun parse-isl-ast (ast)
  (declare (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let ((type (%"isl_ast_node_get_type":isl-ast-node-type :pointer ast)))
      (ecase type
	(:isl_ast_node_error (error ":isl-ast-node-error"))
	(:isl_ast_node_for   (parse-isl-ast-for ast))
	(:isl_ast_node_if    (parse-isl-ast-if ast))
	(:isl_ast_node_block (parse-isl-ast-block ast))
	(:isl_ast_node_mark  (error ":isl_ast_node_mark is not supported"))
	(:isl_ast_node_user  (parse-isl-ast-user ast))))))

(declaim (ftype (function (cffi:foreign-pointer) ASTBlock) parse-isl-ast-block))
(defun parse-isl-ast-block (ast)
  (declare (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let* ((children (%"isl_ast_node_block_get_children":pointer :pointer ast))
	   (n        (%"isl_ast_node_list_n_ast_node":int :pointer children)))
      (make-block
      (loop for i upfrom 0 below n
	    for child = (%"isl_ast_node_list_get_at":pointer :pointer children :int i)
	    collect
	    (parse-isl-ast child))))))

(declaim (ftype (function (cffi:foreign-pointer) User) parse-isl-ast-user))
(defun parse-isl-ast-user (ast)
  (declare (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let ((expr (%"isl_ast_node_user_get_expr":pointer :pointer ast)))
      (%"isl_ast_expr_free":void :pointer expr)
      (let* ((first-expr (%"isl_ast_expr_op_get_arg":pointer :pointer expr :int 0))
	     (n          (%"isl_ast_expr_get_op_n_arg":int   :pointer expr))
	     (id         (prog1
			     (%"isl_ast_expr_id_get_id":pointer :pointer first-expr)
			   (%"isl_ast_expr_free":void :pointer first-expr)))
	     (name       (prog1
			     (%"isl_id_get_name":string :pointer id)
			   (%"isl_id_free":void :pointer id)))
	     (args       (loop for i upfrom 1 below n
			       collect
			       (parse-isl-expr (%"isl_ast_expr_op_get_arg":pointer :pointer expr :int i)))))
	(make-user name args)))))

(declaim (ftype (function (cffi:foreign-pointer) Expr) parse-isl-expr))
(defun parse-isl-expr (ast)
  (declare (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let ((type (%"isl_ast_expr_get_type":isl-ast-expr-type :pointer ast)))
      (ecase type
	(:isl_ast_expr_error  (error "parse-isl-expr :isl_ast_expr_error"))
	(:isl_ast_expr_id
	 (let* ((id (%"isl_ast_expr_id_get_id":pointer :pointer ast))
		(name (prog1
			  (%"isl_id_get_name":string :pointer id)
			(%"isl_id_free":void :pointer id))))
	   (make-expr :Const name)))
	(:isl_ast_expr_int
	 (let* ((id (%"isl_ast_expr_int_get_val":pointer :pointer ast))
		(num (prog1
			 (%"isl_val_get_d":double :pointer id)
		       (%"isl_val_free":void :pointer id))))
	   (let ((num (round num)))
	     (make-expr :Const num))))
	(:isl_ast_expr_op
	 (let ((n-arg (%"isl_ast_expr_get_op_n_arg":int :pointer ast)))
	   (assert (or (= n-arg 1) (= n-arg 2)) () "Assertion Failed with nargs == 1 or 2")
	   (multiple-value-bind (lhs rhs)
	       (values
		(parse-isl-expr (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 0))
		(when (= n-arg 2)
		  (parse-isl-expr (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 1))))
	     (let ((op-type (%"isl_ast_expr_op_get_type":isl-ast-expr-op-type :pointer ast)))
	       (assert (not (eql op-type :isl_ast_expr_op_error)) () ":isl_ast_expr_op_error")
	       (ecase op-type
		 (:isl_ast_expr_op_and (make-expr :and lhs rhs))
		 (:isl_ast_expr_op_and_then (make-expr :and lhs rhs))
		 (:isl_ast_expr_op_or (make-expr :or lhs rhs))
		 (:isl_ast_expr_op_or_else (make-expr :or lhs rhs))
		 (:isl_ast_expr_op_max (make-expr :max lhs rhs))
		 (:isl_ast_expr_op_min  (make-expr :min lhs rhs))
		 (:isl_ast_expr_op_minus (make-expr :neg lhs nil)) ;; (- a)
		 (:isl_ast_expr_op_add (make-expr :+ lhs rhs))
		 (:isl_ast_expr_op_sub (make-expr :- lhs rhs))
		 (:isl_ast_expr_op_mul (make-expr :* lhs rhs))
		 (:isl_ast_expr_op_div (make-expr :/ lhs rhs))		 
		 ;;(:isl_ast_expr_op_fdiv_q (make-expr :floor-div-cast-to-int lhs rhs))
		 (:isl_ast_expr_op_pdiv_q (make-expr :/ lhs rhs))
		 (:isl_ast_expr_op_pdiv_r (make-expr :% lhs rhs))
		 (:isl_ast_expr_op_zdiv_r (make-expr :% lhs rhs))
		 ;;(:isl_ast_expr_op_cond)
		 ;;(:isl_ast_expr_op_select)
		 (:isl_ast_expr_op_eq (make-expr :equal lhs rhs))
		 (:isl_ast_expr_op_le (make-expr :<= lhs rhs))
		 (:isl_ast_expr_op_lt (make-expr :< lhs rhs))
		 (:isl_ast_expr_op_ge (make-expr :<= lhs rhs))
		 (:isl_ast_expr_op_gt (make-expr :> lhs rhs))
		 ;;(:isl_ast_expr_op_call)
		 ;;(:isl_ast_expr_op_access)
		 ;;(:isl_ast_expr_op_member)
		 ;;(:isl_ast_expr_op_address_of)
		 (otherwise
		  (error "~a is not supported." op-type))
		 )))))))))

(declaim (ftype (function (cffi:foreign-pointer) ASTFor) parse-isl-ast-for))
(defun parse-isl-ast-for (ast)
  (declare (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let* ((execute-once (%"isl_ast_node_for_is_degenerate":boolean :pointer ast))
	   (iter (%"isl_ast_node_for_get_iterator":pointer  :pointer ast))
	   (id (%"isl_ast_expr_get_id":pointer :pointer iter))
	   (name (prog1
		     (%"isl_id_get_name":string :pointer id)
		   (%"isl_id_free":void :pointer id)))
	   (from (parse-isl-expr (%"isl_ast_node_for_get_init":pointer :pointer ast)))			   
	   (by (parse-isl-expr (%"isl_ast_node_for_get_inc":pointer :pointer ast)))
	   (to (parse-isl-expr (%"isl_ast_node_for_get_cond":pointer :pointer ast)))
	   (body (parse-isl-ast (%"isl_ast_node_for_get_body":pointer :pointer ast))))
      (make-for name from to by body execute-once))))

(declaim (ftype (function (cffi:foreign-pointer) AstIf) parse-isl-ast-if))
(defun parse-isl-ast-if (ast)
  (declare (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let* ((condition
	     (parse-isl-expr (%"isl_ast_node_if_get_cond":pointer :pointer ast)))
	   (then-node
	     (parse-isl-ast (%"isl_ast_node_if_get_then_node":pointer :pointer ast)))
	   (else-p
	     (%"isl_ast_node_if_has_else_node":boolean :pointer ast))
	   (else-node
	     (when else-p
	       (parse-isl-ast (%"isl_ast_node_if_get_else_node":pointer :pointer ast)))))
      (make-if condition then-node else-node))))
