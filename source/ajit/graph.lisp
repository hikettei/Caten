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
    (when (expr-p (expr-z expr))
      (expr-recursive-replace (expr-z expr) map))
    (when (eql (expr-op expr) :Const)
      (setf (expr-x expr) (->new (expr-x expr))))))

;; ~~ [From aIR -> Expr] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(macrolet ((expr (name (&rest args) (&rest types))
	     `(defun ,(symb 'make- name) (,@args)
		(declare ,@(loop for arg in args for typ in types collect `(type ,typ ,arg)))
		(make-expr ,(intern (symbol-name name) "KEYWORD") ,@args))))
  ;; Buffer Ops
  (expr Const (obj) (t))
  (expr Cast  (obj dtype) (Node Keyword))
  (expr Aref  (id  buffer) (Symbol Buffer)))

(declaim (ftype (function (Node &rest t) Expr) air->expr))
(defun air->expr (node &rest parents)
  (case (node-type node)
    (:INDEX-COMPONENTS (make-expr :INDEX-COMPONENTS (first parents) (cdr parents)))
    (:Cast             (make-cast (second parents) (getattr node :dtype)))
    (:Load             (make-const (getattr node :value)))
    (:MOVE             (second parents))
    (:STORE            (second parents))
    (otherwise
     ;; TODO: Reduce for BinaryOps for MULTIEXPR!!!!
     ;; BinaryOps ADD(x y z c) -> EXPR(EXPR(EXPR ...)))
     (assert (<= (length parents) 3) () "~a cannot be grouped to multi expr! (too many arguments)" node)
     (apply #'make-expr (node-type node) parents))))

(defparameter *aref-list* nil)
(defparameter *group-seen* nil)
(defun recursively-group-expr (poly graph outputs read-by-time &key (no-multi-expr nil))
  (declare (type graph graph) (type list outputs))
  (let* ((stash) (read-by-time (apply #'append read-by-time)))
    (flet ((pause? (x) (declare (type node x))
	     (or (find (car (node-writes x)) (poly-deps-across-group poly))
		 (not (typep (node-type x) 'op/expr))
		 (> (count x read-by-time) 1)
		 no-multi-expr))
	   (first? (x) (declare (type node x)) (not (find (node-id x) *group-seen*)))
	   (stash (x) (declare (type node x)) (push (car (node-writes x)) stash))
	   (saw (x) (declare (type node x)) (push (node-id x) *group-seen*))
	   (make-aref-helper (arg type &aux (aref (make-aref arg type)))
	     (push aref *aref-list*)
	     aref))
      (labels ((fuse-helper (id &key (type nil) &aux (x (id->value graph id)))
		 (if (and x (first? x))
		     (progn
		       (saw x)
		       (let* ((parents
				(loop for arg in (node-reads x)
				      for type in (relay-reads (read-type-relay x))
				      for arg-node = (when (symbolp arg) (id->value graph arg))
				      if arg-node
					collect
					(if (pause? arg-node) 
					    (progn
					      (stash arg-node)
					      (make-aref-helper arg type))
					    (fuse-helper arg :type type))
				      else
					collect (if (symbolp arg) (make-aref-helper arg type) (make-const arg)))))
			 (apply #'air->expr x parents)))
		     (if (symbolp id)
			 (progn (assert type) (make-aref-helper id type))
			 (expr-const id))))
	       (fuse (x &aux (*aref-list*) (*group-seen*) (node (id->value graph x)))
		 (when node
		   (if (pause? node)
		       (if (typep (node-type node) 'op/expr)
			   (list (fuse-helper x) node *aref-list*)
			   (let ((reads (node-reads node)))
			     (mapc #'(lambda (x &aux (n (id->value graph x))) (when n (stash n))) reads)
			     node))
		       (list (fuse-helper x) node *aref-list*)))))
	(let ((exprs (map 'list #'fuse outputs))
	      (stash (reverse stash)))
	  (loop while stash do (setf exprs (append (list (fuse (pop stash))) exprs)))
	  (setf exprs (loop for e in exprs if e collect e))
	  (let ((exprs
		  (loop for expr in exprs
			if (listp expr)
			  collect
			  (multiple-value-bind (expr node arefs) (apply #'values expr)
			    (let ((out-to (if (eql (node-type node) :WHERE)
					      (second (node-reads node))
					      (car (node-reads node))))
				  (out-to-type
				    (if (eql (node-type node) :WHERE)
					(second (relay-reads (read-type-relay node)))
					(car (relay-reads (read-type-relay node))))))
			      ;; EXPR (out-to aref ...)
			      (make-node :EXPR :EXPR (node-writes node) `(,out-to ,@(map 'list #'expr-x arefs))
					 :expr expr :_type_relay (make-inferred-type `(,out-to-type ,@(map 'list #'expr-y arefs)) (relay-writes (read-type-relay node)))
					 :buffers arefs)))
			else
			  collect expr)))
	    (values
	     exprs
	     (remove-duplicates
	      (loop for expr in exprs
		    if (eql (node-type expr) :EXPR)
		      append (node-reads expr))))))))))

(defun graph-out-to (graph) (remove-duplicates (apply #'append (map 'list #'(lambda (x) (recursively-find-output-id x graph)) (graph-seen graph)))))
(defun apply-multiexpr-grouping (poly)
  "Group several computation into a single :EXPR Node to simplify.
E.g.:
  T0 : X <- sin(m)
  T1 : Y <- cos(X)
is updated to:
  T0+T1 : Y <- cos(sin(m))
It uses aIR graph features; accordingly must be applied before doing memory-planner optimization!"
  (declare (type polyhedral poly))
  (let ((read-by-time))
    (maphash
     #'(lambda (ts graph)
	 (declare (ignore ts))
	 (let ((reads (loop for node in (graph-nodes graph)
			    append
			    (remove-duplicates (loop for r in (node-reads node) if (symbolp r) collect r)))))
	   (push reads read-by-time)))
     (poly-pipeline poly))
    (setf read-by-time (reverse read-by-time))
    (let ((c 0) (removed-vars))
      (declare (type fixnum c) (type list removed-vars))
      (maphash
       #'(lambda (ts graph &aux (out-to (graph-out-to graph)))
	   (declare (type graph graph) (ignore ts))
	   (dolist (node (graph-nodes graph))
	     (assert (null (some #'(lambda (x) (find x removed-vars)) (node-reads node)))
		     ()
		     "~a is removed by the multiexpr! (a bug)" (node-reads node)))
	   (when out-to
	     (multiple-value-bind (expr-nodes expr-reads) (recursively-group-expr poly graph out-to  (nthcdr c read-by-time))
	       (declare (type list expr-nodes expr-reads))
	       (setf removed-vars (remove-duplicates (append removed-vars (intersection (nth c read-by-time) expr-reads))))
	       (setf (graph-nodes graph) expr-nodes)))
	   (incf c))
       (poly-pipeline poly))
      removed-vars)))
