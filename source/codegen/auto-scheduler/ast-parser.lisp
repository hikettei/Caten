(defpackage :caten/codegen/ast-parser
  (:documentation "
Transform the Polyhedral IR into the Blueprint IR.
```
[ISL Polyhedral IR] ==> <Polyhedral-AST> ===> Caten Blueprint IR
```
scop.lisp for the opposite things.
")
  (:use :cl :caten/codegen/expr :caten/codegen/expr-cache :caten/air :caten/codegen/shape-inference :trivia
        :caten/codegen/polyhedral-ast :caten/codegen/transform :caten/codegen/directive)
  (:export #:lower-into-bp-from-polyhedral))

(in-package :caten/codegen/ast-parser)

(defstruct Context
  (dynamic-shape-table (make-hash-table) :type hash-table)
  (n-global-offset 0 :type fixnum)
  (n-global-dims 0 :type fixnum))

(defmethod ctx-get-rank ((ctx Context))
  (if (= 0 (context-n-global-offset ctx))
      (context-n-global-dims ctx)
      (- (context-n-global-offset ctx) (context-n-global-dims ctx))))

(defmethod ctx-intern ((ctx Context) obj)
  (let ((key (string-upcase (princ-to-string obj))))
    (or (gethash key (context-dynamic-shape-table ctx))
        (intern key))))

(declaim (ftype (function (context cffi:foreign-pointer) t) parse-isl-ast))
(defun parse-isl-ast (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-node-get-type ast)))
    (ecase type
      (:ast-node-error (isl::isl-error))
      (:ast-node-for   (parse-isl-ast-for ctx ast))
      (:ast-node-if    (parse-isl-ast-if ctx ast))
      (:ast-node-block (parse-isl-ast-block ctx ast))
      (:ast-node-mark  (parse-isl-ast-mark ctx ast))
      (:ast-node-user  (parse-isl-ast-user ctx ast)))))

(declaim (ftype (function (context cffi:foreign-pointer) ASTBlock) parse-isl-ast-block))
(defun parse-isl-ast-block (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((children (isl::%isl-ast-node-block-get-children ast))
	 (n        (isl::%isl-ast-node-list-n-ast-node children)))
    (make-block
     (loop for i upfrom 0 below n
	   for child = (isl::%isl-ast-node-list-get-at children i)
	   collect
	   (parse-isl-ast ctx child)))))

(declaim (ftype (function (context cffi:foreign-pointer) User) parse-isl-ast-user))
(defun parse-isl-ast-user (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let ((expr (isl::%isl-ast-node-user-get-expr ast)))
    (let* ((first-expr (isl::%isl-ast-expr-op-get-arg expr 0))
	   (n          (isl::%isl-ast-expr-get-op-n-arg expr))
	   (id         (isl::%isl-ast-expr-id-get-id first-expr))
	   (name       (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	   (args       (loop for i upfrom 1 below n
			     collect
			     (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg expr i)))))
      (make-user name args))))

(declaim (ftype (function (context cffi:foreign-pointer) t) parse-isl-ast-mark))
(defun parse-isl-ast-mark (ctx ast)
  "@DIRECTIVE Transformation Triggers"
  (let* ((directive (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id ast)))))
         (user (parse-isl-ast ctx (isl::%isl-ast-node-mark-get-node ast))))
    ;; A Sequence of Mark => Set Priority
    ;; [TODO] Sort marks in the isl-schedule level (e.g.: GLOBAL should come after UNROLL)
    (macrolet ((is (name) `(equalp (directive-type directive) ,name)))
      (typecase user
        (AstFor
         ;; Entry point for transformations
         (cond
           ((is "GLOBAL")
            (let ((replacement (astfor-mutate-global user (ctx-get-rank ctx) (directive-amount directive))))
              (incf (context-n-global-dims ctx))
              (return-from parse-isl-ast-mark replacement)))
           ((is "PARALLEL")
            (setf (astfor-scope user) :global))
           ;; UNROLL_OUTER + UNROLL_INNER = UNROLL
           ((is "UNROLL_OUTER")
            (let ((body (astfor-body user)))
              (when (or
                     (not (typep body 'ASTFor))
                     (null (and (astfor-marks body) (every #'(lambda (x) (equalp (directive-type x) "UNROLL_INNER")) (astfor-marks body)))))
                (return-from parse-isl-ast-mark user))
              (let* ((n-unroll (directive-amount directive))
                     (user     (copy-astfor user))
                     (unrolled (make-unrolled-body user body n-unroll))
                     (reminder (compute-reminder-for-unroll user body n-unroll)))
                (setf (astfor-body user) unrolled)
                (return-from parse-isl-ast-mark (make-block (list user reminder))))))
           ((is "UNROLL_INNER")
            ;; UNROLL_BODY is triggered by the UNROLL_PARENT. Without it the form is ignored.
            (assert (null (astfor-marks user)) () "UNROLL_INNER should be orthogonal with other directives.")
            (setf (astfor-marks user) (list directive)))))
        (AstBlock
         ;; Nested Directive Transformations
         (cond
           ((is "PARALLEL")
            (when (astfor-p (car (astblock-body user)))
              (setf (astfor-scope (car (astblock-body user))) :global)))
           ((is "GLOBAL")
            ;; @DIRECTIVE(GLOBAL)
            ;; for (...;...;+=4) {...} // BODY
            ;; for (...;...;+=1) {...} // Reminder
            (when (and
                   (= (length (astblock-body user)) 2)
                   (typep (first (astblock-body user)) 'AstFor)
                   (typep (second (astblock-body user)) 'AstFor))
              (multiple-value-bind (body reminder) (apply #'values (astblock-body user))
                (let ((replacement
                        (make-block
                         (list
                          (astfor-mutate-global body (ctx-get-rank ctx) (directive-amount directive))
                          (astfor-mutate-reminder-global (astfor-idx body) reminder)))))
                  (incf (context-n-global-dims ctx))
                  (return-from parse-isl-ast-mark replacement)))))))))
    user))

(declaim (ftype (function (context (or cffi:foreign-pointer isl:ast-node)) (values Expr &optional)) parse-isl-expr))
(defun parse-isl-expr (ctx ast)
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
             
             (expr-const (ctx-intern ctx name) :int64))))
      (:ast-expr-int
       (let* ((id (isl::%isl-ast-expr-int-get-val ast))
	      (num (isl::%isl-val-get-d id)))
	 (declare (type number num))
	 (let ((num (round num)))
           (expr-const num :int64))))
      (:ast-expr-op
       (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
	      (args (loop for nth upfrom 0 below n-arg
			  collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg ast nth))))
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

(declaim (ftype (function (context cffi:foreign-pointer) ASTFor) parse-isl-ast-for))
(defun parse-isl-ast-for (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((iter (isl::%isl-ast-node-for-get-iterator ast))
	 (id (isl::%isl-ast-expr-get-id iter))
	 (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	 (from (parse-isl-expr ctx (isl::%isl-ast-node-for-get-init ast)))
	 (by (parse-isl-expr ctx (isl::%isl-ast-node-for-get-inc ast)))
	 (to (parse-isl-expr ctx (isl::%isl-ast-node-for-get-cond ast)))
	 (body (parse-isl-ast ctx (isl::%isl-ast-node-for-get-body ast))))
    (make-for name from to by body)))

(declaim (ftype (function (context cffi:foreign-pointer) AstIf) parse-isl-ast-if))
(defun parse-isl-ast-if (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((condition
	   (parse-isl-expr ctx (isl::%isl-ast-node-if-get-cond ast)))
	 (then-node
	   (parse-isl-ast ctx (isl::%isl-ast-node-if-get-then-node ast)))
	 (else-p (isl::%isl-ast-node-if-has-else-node ast))
	 (else-node
	   (when (eql else-p :bool-true)
	     (parse-isl-ast ctx (isl::%isl-ast-node-if-get-else-node ast)))))
    (make-if condition then-node else-node)))
;; ~~ ISL Object <--> Blueprint ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun r/for (idx upfrom below by scope)
  (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by :scope scope))

(defun r/endfor (idx)
  (make-node :Render :ENDFOR nil nil :idx idx))

(defun r/if (condition)
  (make-node :Render :IF nil nil :condition condition))

(defun r/endif ()
  (make-node :Render :ENDIF nil nil))

(defun create-rendering-graph-nodes (lisp-ast items &aux (space))
  (let ((new-graph))
    (labels ((find-user (node-id args)
               (let ((node (find (princ-to-string node-id) items
                                 :key (alexandria:compose #'princ-to-string #'node-id)
                                 :test #'equalp)))
                 (assert node () "~a is not found in ~a" node-id items)
                 (assert (eql (node-type node) :EXPR))
                 (setf node (copy-node node))
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
	       (ematch object
		 ((ASTBlock :body body) (map 'list #'lower body))
		 ((AstFor :idx idx :from upfrom :to to :by by :body body :scope scope)
                  ;; [TODO] Generalize this
                  ;; TILE_BAND is not an unroll idx?
                  (let ((is-tile-band (find "TILE_BAND" (astfor-marks object) :test #'equalp)))
                    (when (not (expr-scalar-equivalent-p upfrom (expr-detach-loop-bound to)))
                      (when (null is-tile-band) (push idx space))
		      (push (r/for idx upfrom to by scope) new-graph)
		      (lower body)
                      (when (null is-tile-band) (setf space (remove idx space :test #'string=)))
		      (push (r/endfor idx) new-graph))))
		 ((User :name name :args args)
                  (push (caten/codegen/directive:unroll-expr (reverse space) (find-user name args) object) new-graph))
		 ((AstIf :condition cond :then-node then :else-node else)
		  (push (r/if cond) new-graph)
		  (lower then)
                  (assert (null else) () "else should be none")
		  (push (r/endif) new-graph))
                 ((AstExpr :expr expr :is-defglobal-p defglobal-p)
                  (assert (eql (node-type expr) :EXPR) () "ASTEXPR can only have an EXPR.")
                  (when defglobal-p
                    (push (string-downcase (princ-to-string (car (node-writes expr)))) space))
                  (push expr new-graph))
		 (_
		  (error "create-rendering-graph: ~a should not occur here!" object)))))
      (lower lisp-ast))
    (nreverse new-graph)))

(defun dynamic-shape-table (item)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (name . dtype) in (getattr item :dynamic-shapes)
          do (setf (gethash (string-upcase (princ-to-string name)) table) name))
    table))

(defun lower-into-bp-from-polyhedral (ast scheduled-item &key (n-global-offset 0))
  (declare (type isl:ast-node ast))
  (assert (eql (node-type scheduled-item) :Schedule-Item))
  (create-rendering-graph-nodes
   (parse-isl-ast
    (make-context :n-global-offset n-global-offset :dynamic-shape-table (dynamic-shape-table scheduled-item))
    (isl::ast-node-handle ast))
   (getattr scheduled-item :blueprint)))
