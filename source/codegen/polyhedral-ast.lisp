(defpackage :caten/codegen/polyhedral-ast
  (:documentation "ISL Polyhedral IR ==> Caten Blueprint IR")
  (:use :cl :caten/codegen/expr :caten/codegen/expr-cache :caten/air :caten/codegen/shape-inference)
  (:export #:lower-into-bp-from-polyhedral))

(in-package :caten/codegen/polyhedral-ast)
;; ~~ ISL AST <-> Lisp Intermidate Object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (ASTBlock
	    (:constructor make-block (body)))
  (body body :type list))

(defstruct (User
	    (:constructor make-user (name args)))
  "T_name(index)"
  (name name :type string) (args args :type list))

(defstruct (ASTFor
	    (:constructor make-for (idx from to by body)))
  (idx idx :type string)
  (from from :type Expr)
  (to to :type Expr)
  (by by :type Expr)
  (body body :type (or ASTBlock User ASTFor ASTIF)))

(defstruct (AstIf
	    (:constructor make-if (condition then-node else-node)))
  (condition condition :type Expr)
  (then-node then-node :type (or ASTBlock User ASTFOR ASTIF))
  (else-node else-node :type (or ASTBlock User ASTFOR ASTIF null)))

(declaim (ftype (function (cffi:foreign-pointer) t) parse-isl-ast))
(defun parse-isl-ast (ast)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-node-get-type ast)))
    (ecase type
      (:ast-node-error (isl::isl-error))
      (:ast-node-for   (parse-isl-ast-for ast))
      (:ast-node-if    (parse-isl-ast-if ast))
      (:ast-node-block (parse-isl-ast-block ast))
      (:ast-node-mark  (error ":isl_ast_node_mark is not supported"))
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
	      (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
              (cache (restore-expr name)))
	 (declare (type string name))
         (if cache
             cache
             (expr-const (intern name) :int64))))
      (:ast-expr-int
       (let* ((id (isl::%isl-ast-expr-int-get-val ast))
	      (num (isl::%isl-val-get-d id)))
	 (declare (type number num))
	 (let ((num (round num)))
           (expr-const num :int64))))
      (:ast-expr-op
       (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
	      (args (loop for nth upfrom 0 below n-arg
			  collect (parse-isl-expr (isl::%isl-ast-expr-op-get-arg ast nth))))
	      (op-type (isl::%isl-ast-expr-op-get-type ast)))
	 (flet ((->expr (lhs rhs)
		  (assert (not (eql op-type :ast-expr-op-error)) () ":isl_ast_expr_op_error")
		  (ecase op-type
		    (:ast-expr-op-and (expr-and lhs rhs))
		    (:ast-expr-op-and-then (expr-and lhs rhs))
		    (:ast-expr-op-or (expr-or lhs rhs))
		    (:ast-expr-op-or-else (expr-or lhs rhs))
		    (:ast-expr-op-max (expr-max lhs rhs))
		    (:ast-expr-op-min  (expr-min lhs rhs))
		    (:ast-expr-op-minus (expr-neg lhs)) ;; (- a)
		    (:ast-expr-op-add (expr-add lhs rhs))
		    (:ast-expr-op-sub (expr-sub lhs rhs))
		    (:ast-expr-op-mul (expr-mul lhs rhs))
		    (:ast-expr-op-div (expr-idiv lhs rhs))		 
		    (:ast-expr-op-fdiv-q (expr-idiv lhs rhs))
		    (:ast-expr-op-pdiv-q (expr-idiv lhs rhs))
		    (:ast-expr-op-pdiv-r (expr-mod lhs rhs))
		    (:ast-expr-op-zdiv-r (expr-mod lhs rhs))
		    ;; (:expr-op-cond)
		    (:ast-expr-op-eq (expr-= lhs rhs))
                    ;; Rewrite LE to simplify the expression
		    (:ast-expr-op-le (expr-< lhs (expr-add rhs (expr-const 1 :int64)))) ;; <=
		    (:ast-expr-op-lt (expr-< lhs rhs)) ;; <
		    (:ast-expr-op-ge (expr-not (expr-< lhs rhs))) ;; >=
		    (:ast-expr-op-gt (expr-> lhs rhs)) ;; >
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
		  (apply #'expr-where args))
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
;; ~~ ISL Object <--> Blueprint ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun r/for (idx upfrom below by)
  (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by))

(defun r/endfor (idx)
  (make-node :Render :ENDFOR nil nil :idx idx))

(defun r/if (condition)
  (make-node :Render :IF nil nil :condition condition))

(defun r/endif ()
  (make-node :Render :ENDIF nil nil))

(defun create-rendering-graph-nodes (lisp-ast items)
  (let ((new-graph))
    (labels ((find-user (node-id args)
               (let ((node (find (princ-to-string node-id) items
                                 :key (alexandria:compose #'princ-to-string #'node-id)
                                 :test #'equalp)))
                 (assert node () "~a is not found in ~a" node-id items)
                 (assert (eql (node-type node) :EXPR))
                 (let ((base (getattr node :iterations)))
                   (setf (getattr node :iterations) args)
                   (if (and (null args) (> (length base) 0))
                       (setf (getattr node :iterations) base)
                       (progn
                         (setf (getattr node :iterations) (ensure-iteration-space-length (length base) (getattr node :iterations)))
                         (assert (= (length (getattr node :iterations)) (length base)) () "Before and after the polyhedral compilation, the rank of iteration space should not be changed. ~a -> ~a" (getattr node :iterations) args))))
                 node))
             (lower (object)
	       (when (listp object) (return-from lower (map 'list #'lower object)))
	       (trivia:ematch object
		 ((AstBlock :body body) (map 'list #'lower body))
		 ((AstFor :idx idx :from upfrom :to to :by by :body body)
		  (push (r/for idx upfrom to by) new-graph)
		  (lower body)
		  (push (r/endfor idx) new-graph))
		 ((User :name name :args args)
                  (push (find-user name args) new-graph))
		 ((AstIf :condition cond :then-node then :else-node else)
		  (push (r/if cond) new-graph)
		  (lower then)
                  (assert (null else) () "else should be none")
		  (push (r/endif) new-graph))
		 (_
		  (error "create-rendering-graph: ~a should not occur here!" object)))))
      (lower lisp-ast))
    (nreverse new-graph)))

(defun lower-into-bp-from-polyhedral (ast scheduled-item)
  (declare (type isl:ast-node ast))
  (assert (eql (node-type scheduled-item) :Schedule-Item))
  (create-rendering-graph-nodes
   (parse-isl-ast (isl::ast-node-handle ast))
   (getattr scheduled-item :blueprint)))
