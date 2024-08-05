(in-package :caten/ajit)

(defcenum :isl-ast-node-type
  (:isl_ast_node_error -1)
  (:isl_ast_node_for 1)
  :isl_ast_node_if
  :isl_ast_node_block
  :isl_ast_node_mark
  :isl_ast_node_user)

(defcenum :isl-ast-expr-type
  (:isl_ast_expr_error -1)
  :isl_ast_expr_op
  :isl_ast_expr_id
  :isl_ast_expr_int)

(defcenum :isl-ast-expr-op-type
  (:isl_ast_expr_op_error -1)
  :isl_ast_expr_op_and
  :isl_ast_expr_op_and_then
  :isl_ast_expr_op_or
  :isl_ast_expr_op_or_else

  ;; maxmin
  :isl_ast_expr_op_max
  :isl_ast_expr_op_min

  ;; (- a)
  :isl_ast_expr_op_minus

  ;; Binary_Ops
  :isl_ast_expr_op_add
  :isl_ast_expr_op_sub
  :isl_ast_expr_op_mul
  :isl_ast_expr_op_div
  
  :isl_ast_expr_op_fdiv_q
  :isl_ast_expr_op_pdiv_q
  :isl_ast_expr_op_pdiv_r
  :isl_ast_expr_op_zdiv_r
  :isl_ast_expr_op_cond
  :isl_ast_expr_op_select
  :isl_ast_expr_op_eq
  :isl_ast_expr_op_le
  :isl_ast_expr_op_lt
  :isl_ast_expr_op_ge
  :isl_ast_expr_op_gt
  :isl_ast_expr_op_call
  :isl_ast_expr_op_access
  :isl_ast_expr_op_member
  :isl_ast_expr_op_address_of)

(defun finalize-schedule (polyhedral)
  "Lowers the ISL-ast into polyhedron"
  (declare (type Polyhedral polyhedral))
  (with-slots ((avm avm)) polyhedral
    (let ((ast (isl-obj-ptr (finalize-polyhedral polyhedral))))
      (parse-isl-ast avm ast))))

(defun parse-isl-ast (avm ast)
  (declare (type avm avm)
	   (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let ((type (%"isl_ast_node_get_type":isl-ast-node-type :pointer ast)))
      (ecase type
	(:isl_ast_node_error (error ":isl-ast-node-error"))
	(:isl_ast_node_for
	 ;; isl-ast-for
	 (parse-isl-ast-for avm ast))
	(:isl_ast_node_if
	 (error "Not implemented: parse-isl-ast-if")
	 )
	(:isl_ast_node_block
	 (parse-isl-ast-block avm ast))
	(:isl_ast_node_mark
	 "")
	(:isl_ast_node_user
	 (parse-isl-ast-user avm ast))))))

(defstruct (ASTBlock
	    (:constructor make-block (body)))
  (body body :type list))

(declaim (ftype (function (AVM cffi:foreign-pointer) ASTBlock) parse-isl-ast-block))
(defun parse-isl-ast-block (avm ast)
  (declare (type avm avm)
	   (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let* ((children (%"isl_ast_node_block_get_children":pointer :pointer ast))
	   (n        (%"isl_ast_node_list_n_ast_node":int :pointer children)))
      (make-block
      (loop for i upfrom 0 below n
	    for child = (%"isl_ast_node_list_get_at":pointer :pointer children :int i)
	    collect
	    (parse-isl-ast avm child))))))

(defstruct (User
	    (:constructor make-user (name args)))
  (name name :type string) (args args :type list))

(declaim (ftype (function (AVM cffi:foreign-pointer) User) parse-isl-ast-user))
(defun parse-isl-ast-user (avm ast)
  (declare (type avm avm)
	   (type cffi:foreign-pointer ast))
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
			       (parse-isl-expr avm (%"isl_ast_expr_op_get_arg":pointer :pointer expr :int i)))))
	(make-user name args)))))

(defstruct (Expr (:constructor make-expr (op x &optional (y nil))))
  (op op) (x x) (y y))

;; TODO: need-upper-bound is not anymore used
(declaim (ftype (function (AVM cffi:foreign-pointer &key (:need-upper-bound boolean)) Expr) parse-isl-expr))
(defun parse-isl-expr (avm ast &key (need-upper-bound nil))
  (declare (type avm avm)
	   (type cffi:foreign-pointer ast))
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
		(parse-isl-expr avm (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 0))
		(when (= n-arg 2)
		  (parse-isl-expr avm (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 1))))
	     (let ((op-type (%"isl_ast_expr_op_get_type":isl-ast-expr-op-type :pointer ast)))
	       (assert (not (eql op-type :isl_ast_expr_op_error)) () ":isl_ast_expr_op_error")
	       ;; i < 10, here 10 is needed if need-upper-bound is set to t
	       (when need-upper-bound
		 ;; i < 10, here 10 is needed if need-upper-bound is set to t
		 (ecase op-type
		   ;; a < b
		   ;; ->
		   ;; a <= b-1
		   (:isl_ast_expr_op_lt ;; a < b
		    (return-from parse-isl-expr (make-expr :- rhs 1)))
		   (:isl_ast_expr_op_le ;; a <= b
		    (return-from parse-isl-expr (make-expr :Const rhs)))))
	       (ecase op-type
		 (:isl_ast_expr_op_and (make-expr :and lhs rhs))
		 (:isl_ast_expr_op_and_then (make-expr :and lhs rhs))
		 (:isl_ast_expr_op_or (make-expr :or lhs rhs))
		 (:isl_ast_expr_op_or_else (make-expr :or lhs rhs))
		 (:isl_ast_expr_op_max (make-expr :max lhs rhs))
		 (:isl_ast_expr_op_min  (make-expr :min lhs rhs))
		 (:isl_ast_expr_op_minus (make-expr :- 0 lhs)) ;; (- a)
		 (:isl_ast_expr_op_add (make-expr :+ lhs rhs))
		 (:isl_ast_expr_op_sub (make-expr :- lhs rhs))
		 (:isl_ast_expr_op_mul (make-expr :* lhs rhs))
		 (:isl_ast_expr_op_div (make-expr :/ lhs rhs))		 
		 (:isl_ast_expr_op_fdiv_q (make-expr :floor-div-cast-to-int lhs rhs))
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
		 )))))))))

(defstruct (ASTFor
	    (:constructor make-for (idx from to by body execute-once)))
  (idx idx :type string)
  (from from :type Expr)
  (to to :type Expr)
  (by by :type Expr)
  (body body)
  (execute-once execute-once :type boolean))

(declaim (ftype (function (AVM cffi:foreign-pointer) ASTFor) parse-isl-ast-for))
(defun parse-isl-ast-for (avm ast)
  (declare (type avm avm)
	   (type cffi:foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let* ((execute-once (%"isl_ast_node_for_is_degenerate":boolean :pointer ast))
	   (iter (%"isl_ast_node_for_get_iterator":pointer  :pointer ast))
	   (id (%"isl_ast_expr_get_id":pointer :pointer iter))
	   (name (prog1
		     (%"isl_id_get_name":string :pointer id)
		   (%"isl_id_free":void :pointer id)))
	   (from (parse-isl-expr avm (%"isl_ast_node_for_get_init":pointer :pointer ast)))			   
	   (by (parse-isl-expr avm (%"isl_ast_node_for_get_inc":pointer :pointer ast)))
	   (to (parse-isl-expr avm (%"isl_ast_node_for_get_cond":pointer :pointer ast)))
	   (body (parse-isl-ast
		  avm
		  (%"isl_ast_node_for_get_body":pointer :pointer ast))))
      (make-for name from to by body execute-once))))
