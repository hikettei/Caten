(in-package :caten/ajit)
;; A special graph dedicated to the rendering process
(defun r/for (idx upfrom below by) (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by))
(defun r/endfor (idx) (make-node :Render :ENDFOR nil nil :idx idx))
(defun r/funcall (name args)
  ;; :idx = (T12 -> 12)
  (make-node :Render :FUNCALL nil nil :name name :args args :idx (parse-integer (subseq name 1))))
(defun r/if (condition) (make-node :Render :IF nil nil :condition condition))
(defun r/else () (make-node :Render :ELSE nil nil))
(defun r/endif () (make-node :Render :ENDIF nil nil))
(defun create-rendering-graph (polyhedron lisp-ast)
  (declare (type polyhedral polyhedron)
	   (ignore polyhedron))
  (let ((new-graph nil))
    (labels ((lower (object)
	       (when (listp object) (return-from lower (map 'list #'lower object)))
	       (trivia:ematch object
		 ((AstBlock :body body) (map 'list #'lower body))
		 ((AstFor :idx idx :from upfrom :to to :by by :body body :execute-once _)
		  (push (r/for idx upfrom to by) new-graph)
		  (lower body)
		  (push (r/endfor idx) new-graph))
		 ((User :name name :args args)
		  (push (r/funcall name args) new-graph))
		 ((AstIf :condition cond :then-node then :else-node else)
		  (push (r/if cond) new-graph)
		  (lower then)
		  (when else
		    (push (r/else) new-graph)
		    (lower else))
		  (push (r/endif) new-graph))
		 ((Expr :op _ :x _ :y _)
		  (error "create-rendering-graph: Expr should not occur here!")))))
      (lower lisp-ast))
    (apply #'make-graph (reverse new-graph))))

(defun count-n-kernels (rendering-graph &aux (count 0) (level 0))
  "Counts the number of the outermost loops (= n-kernels)"
  (loop for node in (graph-nodes rendering-graph)
	if (eql (node-type node) :FOR)
	  do (when (= 0 level) (incf count))
	     (incf level)
	else if (eql (node-type node) :ENDFOR) do
	  (decf level))
  count)

(defun expr-recursive-replace (expr map)
  (declare (type expr) (type function map))
  (flet ((->new (x) (if (stringp x) (format nil "~a" (or (funcall map (intern x)) x)) x)))
    (when (expr-p (expr-x expr))
      (expr-recursive-replace (expr-x expr) map))
    (when (expr-p (expr-y expr))
      (expr-recursive-replace (expr-y expr) map))
    (when (eql (expr-op expr) :Const)
      (setf (expr-x expr) (->new (expr-x expr))))))

;; ~~ [From aIR -> Expr] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] ISL_AST_HELPER 全部これで書き換える
;; Deftype koko de suru
;; aIR Graphの方が良くない？
;; REMOVE EXPR!
(defun %make-expr (op to &rest args) (make-node :RENDER op (list to) args))
(macrolet ((expr (name (&rest args) (&rest types))
	     `(defun ,(symb 'make- name) (,@args)
		(declare ,@(loop for arg in args for typ in types collect `(type ,typ ,arg)))
		(%make-expr ,(intern (symbol-name (symb 'make- name)) "KEYWORD") ,@args))))
  ;; Buffer Ops
  (expr Const (obj) (t))
  (expr Cast  (obj dtype) (Expr Keyword))
  (expr Aref  (id  buffer) (Symbol Buffer))

  ;; Ternary Ops
  (expr WHERE (condition x y) (Expr Expr Expr))
  ;; Binary Ops
  
  ;; Compare
  (expr <  (x y) (Expr Expr))
  (expr <= (x y) (Expr Expr))
  (expr >  (x y) (Expr Expr))
  (expr >= (x y) (Expr Expr))
  (expr != (x y) (Expr Expr))
  (expr == (x y) (Expr Expr))

  ;; Logical
  (expr AND (x y) (Expr Expr))
  (expr OR  (x y) (Expr Expr))
  )
			   
		  

(declaim (ftype (function (Symbol Graph) Expr) create-expr-from-air))
(defun create-expr-from-air (output graph)
  (declare (type symbol output) (type graph graph))
  (let* ((node (id->value graph output))
	 (parents (loop for arg in (node-reads node)
			for typ in (relay-reads (read-type-relay node))
			if (symbolp arg)
			  collect
			  (let ((val (id->value graph arg)))
			    (if val
				(create-expr-from-air arg graph)
				(expr-aref arg typ)))		    
			else
			  collect (expr-const arg))))
    (when (eql (node-type node) :INDEX-COMPONENTS)
      (return-from create-expr-from-air (make-expr :INDEX-COMPONENTS (first parents) (cdr parents))))
    (assert (<= (length parents) 3) () "~a cannot be grouped to multi expr! (too many arguments)" node)
    (case (node-type node)
      (:WHERE (make-expr (node-type node) (first parents) (second parents) (third parents)))
      (:LT    (make-expr (node-type node) (second parents) (third parents)))
      (:NEQ   (make-expr (node-type node) (second parents) (third parents)))
      (:Load (expr-const (getattr node :value)))
      (:Cast (expr-cast (second parents) (getattr node :dtype)))
      (otherwise (make-expr (node-type node) (first parents) (second parents))))))

(defun create-multiexpr-node (graph output output-type read-from read-type)
  (declare (type symbol output read-from) (type graph graph)
	   (type Buffer read-type output-type))
  (let* ((*allocated-aref*)
	 (expr (create-expr-from-air output graph)))
    (make-node :EXPR :EXPR (list output) `(,read-from ,@(map 'list #'expr-x *allocated-aref*)) :expr expr
	       :_type_relay (make-inferred-type `(,read-type ,@(map 'list #'expr-y *allocated-aref*)) (list output-type))
	       :buffers *allocated-aref*)))

;; BinaryOps ADD(x y z c) -> EXPR(EXPR(EXPR ...)))
(defun recursively-group-expr (poly graph outputs)
  (declare (type graph graph) (type list outputs))
  (let* ((seen)
	 (stash))
    (flet ((pause? (x) (declare (type node x)) (find (car (node-writes x)) (poly-deps-across-group poly)))
	   (first? (x) (declare (type node x)) (not (find (node-id x) seen)))
	   (stash (x) (declare (type node x)) (push (car (node-writes x)) stash))
	   (saw (x) (declare (type node x)) (push (node-id x) seen)))
      (labels ((fuse-helper (id &aux (x (id->value graph id)))
		 (if (first? x)
		     (progn
		       (saw x)
		       (let* ((parents
				(loop for arg in (node-reads x)
				      for type in (relay-reads (read-type-relay x))
				      if (symbolp arg) do
					(let ((arg-node (id->value graph arg)))
					  ;; (Assert or .. ....)
					  ;; Render Type宣言必要！
					  ;; Arg-Node is :ALLOCATE? -> SPLIT
					  ;; Typep arg-node :EXPR
					  (if (pause? arg-node) ;; If arg-node needs to be explicitly allocated? -> SPLIT
					      (and (stash arg-node) (expr-aref arg type))
					      (fuse-helper arg)))
				      else
					do (expr-const arg))))
			 (make-expr-helper parents)))
		     (expr-const id))))
	(let ((expr (map 'list #'fuse-helper outputs)))
	  (loop while stash do (setf expr (append (list (fuse-helper (pop stash))) expr)))
	  ;; TODO: Update type
	  expr)))))

(defun graph-out-to (graph) (remove-duplicates (apply #'append (map 'list #'(lambda (x) (recursively-find-output-id x graph)) (graph-seen graph)))))
(defun apply-multiexpr-grouping (poly)
  "Group several computation into a single :EXPR Node to simplify.
E.g.:
  T0 : X <- sin(m)
  T1 : Y <- cos(X)
is updated to:
  T0+T1 : Y <- cos(sin(m))
It uses aIR graph features; accordingly must be applied before doing memory-planner optimization!"
  (declare (type polyhedral poly) (optimize (speed 3)))
  (maphash
   #'(lambda (ts graph)
       (declare (type graph graph) (ignore ts))
       (let* ((exprs (recursively-group-expr poly graph (graph-out-to graph))))
	 (setf (graph-nodes graph) exprs)))
   (poly-pipeline poly)))
