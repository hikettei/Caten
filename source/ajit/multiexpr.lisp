(in-package :caten/ajit)
;; multiexpr.lisp
;; Groups the nested structure in the node into a Expr, making more chances to eliminate extra buffers and in-place mutation

(defun expr-recursive-replace (expr map)
  (declare (type expr) (type function map))
  (flet ((->new (x) (if (stringp x) (format nil "~a" (or (funcall map (intern x)) x)) (funcall map x))))
    (when (expr-p (expr-x expr))
      (expr-recursive-replace (expr-x expr) map))
    (when (expr-p (expr-y expr))
      (expr-recursive-replace (expr-y expr) map))
    (when (expr-p (expr-z expr))
      (expr-recursive-replace (expr-z expr) map))
    (when (or (eql (expr-op expr) :Const) (eql (expr-op expr) :Aref))
      (setf (expr-x expr) (->new (expr-x expr))))
    (when (eql (expr-op expr) :INDEX-COMPONENTS)
      (map 'list #'(lambda (x) (expr-recursive-replace x map)) (expr-y expr)))))

(defun expr-recursive-deps (expr)
  (declare (type expr expr))
  (let ((out))
    (when (or (eql (expr-op expr) :AREF)
	      (eql (expr-op expr) :CONST))
      (when (or (stringp (expr-x expr)) (symbolp (expr-x expr)))
	(push (expr-x expr) out)))
    (setf out (append out
		      (when (expr-p (expr-x expr)) (expr-recursive-deps (expr-x expr)))
		      (when (expr-p (expr-y expr)) (expr-recursive-deps (expr-y expr)))
		      (when (expr-p (expr-z expr)) (expr-recursive-deps (expr-z expr)))
                      (when (listp (expr-y expr)) (apply #'append (map 'list #'expr-recursive-deps (expr-y expr))))))
    out))

(defun expr-recursive-settype (expr id buffer)
  (declare (type expr expr) (type symbol id) (type buffer buffer))
  (when (and
	 (eql (expr-op expr) :AREF)
	 (eql (expr-x expr) id))
    (setf (expr-y expr) buffer))
  (when (expr-p (expr-x expr)) (expr-recursive-settype (expr-x expr) id buffer))
  (when (expr-p (expr-y expr)) (expr-recursive-settype (expr-y expr) id buffer))
  (when (expr-p (expr-z expr)) (expr-recursive-settype (expr-z expr) id buffer)))

(defun expr-recursively-apply (expr f)
  "Applies the function f to the all buffers in the expr."
  (check-type f function)
  (labels ((->new (x) (if (listp x) (map 'list #'->new x) (if (buffer-p x) (funcall f x) x))))
    (when (expr-p expr)
      (setf (expr-x expr) (->new (expr-x expr))
	    (expr-y expr) (->new (expr-y expr))
	    (expr-z expr) (->new (expr-z expr)))
      (when (expr-x expr)
	(expr-recursively-apply (expr-x expr) f))
      (when (expr-y expr)
	(expr-recursively-apply (expr-y expr) f))
      (when (expr-z expr)
	(expr-recursively-apply (expr-z expr) f))
      (when (eql (expr-op expr) :INDEX-COMPONENTS)
	(map 'list #'(lambda (x) (expr-recursively-apply x f)) (expr-y expr))))))

(defun expr-graft-after (expr tgt-id replace-to)
  (check-type replace-to Expr)
  (flet ((->new (x)
	   (when (equalp (princ-to-string x) (princ-to-string tgt-id))
	     (setf (expr-op expr) (expr-op replace-to)
		   (expr-x expr) (expr-x replace-to)
		   (expr-y expr) (expr-y replace-to)
		   (expr-z expr) (expr-z replace-to)))))
    (when (expr-p expr)
      (when (expr-x expr)
	(expr-graft-after (expr-x expr) tgt-id replace-to))
      (when (expr-y expr)
	(expr-graft-after (expr-y expr) tgt-id replace-to))
      (when (expr-z expr)
	(expr-graft-after (expr-z expr) tgt-id replace-to))
      (when (or (eql (expr-op expr) :Const) (eql (expr-op expr) :Aref))
	(->new (expr-x expr)))
      (when (eql (expr-op expr) :INDEX-COMPONENTS)
	(map 'list #'(lambda (x) (expr-graft-after x tgt-id replace-to)) (expr-y expr))))))

(defun simplify-expr (expr &aux (changed-p nil))
  (labels ((changed () (setf changed-p t))
	   (%simplify-expr (expr)
	     (trivia:ematch expr
	       ((Expr :op :ADD :x (Expr :op :Const :x (= 0)) :y y :z (trivia:guard z (null z)))
		(changed)
		(%simplify-expr y))
	       ((Expr :op :ADD :x x :y (Expr :op :Const :x (= 0)) :z (trivia:guard z (null z)))
		(changed)
		(%simplify-expr x))
	       ((Expr :op :MUL :x (Expr :op :Const :x (= 1)) :y y :z (trivia:guard z (null z)))
		(changed)
		(%simplify-expr y))
	       ((Expr :op :MUL :x x :y (Expr :op :Const :x (= 1)) :z (trivia:guard z (null z)))
		(changed)
		(%simplify-expr x))
	       ((Expr :op :MUL :x (Expr :op :Const :x (= 0) :y dtype) :y _ :z (trivia:guard z (null z)))
		(changed)
		(make-const 0 dtype))
	       ((Expr :op :MUL :x _ :y (Expr :op :Const :x (= 0) :y dtype) :z (trivia:guard z (null z)))
		(changed)
		(make-const 0 dtype))
	       ((Expr :op :ADD :x (Expr :op :const :x (trivia:guard x (numberp x))) :y (Expr :op :const :x (trivia:guard y (numberp y))) :z (trivia:guard z (null z)))
		(changed)
		(make-const (+ x y) nil))
	       ((Expr :op :MUL :x (Expr :op :const :x (trivia:guard x (numberp x))) :y (Expr :op :const :x (trivia:guard y (numberp y))) :z (trivia:guard z (null z)))
		(changed)
		(make-const (* x y) nil))
	       (_
		(if (expr-p expr)
		    (make-expr (expr-op expr) (%simplify-expr (expr-x expr)) (%simplify-expr (expr-y expr)) (%simplify-expr (expr-z expr)))
		    expr)))))
    (setf expr (%simplify-expr expr))
    (if changed-p
	(simplify-expr expr)
	expr)))

;; ~~ [From aIR -> Expr] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(macrolet ((expr (name (&rest args) (&rest types) guard-const)
	     `(defun ,(symb 'make- name) (,@args)
		(declare ,@(loop for arg in args for typ in types collect `(type ,typ ,arg)))
		(let ((,(car args) (if (and (stringp ,(car args)) (numberp (read-from-string ,(car args) nil)))
				       (read-from-string ,(car args))
				       (if (and ,guard-const (expr-p ,(car args)) (eql :Const (expr-op ,(car args))))
					   (expr-x ,(car args))
					   (if (and (symbolp ,(car args)) (numberp (read-from-string (format nil "~a" ,(car args)) nil)))
					       (read-from-string (format nil "~a" ,(car args)))
					       ,(car args))))))
		  (make-expr ,(intern (symbol-name name) "KEYWORD") ,@args)))))
  ;; Buffer Ops
  (expr Const (obj type) (t (or null Buffer)) t)
  (expr Cast  (obj dtype) (Expr Keyword) nil)
  (expr Aref  (id  buffer) (Symbol Buffer) nil))

(declaim (ftype (function (Node &rest t) (values Expr &optional)) air->expr))
(defun air->expr (node &rest parents)
  (flet ((use (obj)
	   (when (and (expr-p obj) (find (expr-op obj) `(:aref :const)))
	     (when (symbolp (expr-x obj))
	       (push obj *aref-list*)))
	   obj))
    (case (node-type node)
      (:INDEX-COMPONENTS (make-expr :INDEX-COMPONENTS (first parents) (map 'list #'use (cdr parents))))
      (:Cast             (make-cast (use (second parents)) (getattr node :dtype)))
      (:Load             (use (make-const (getattr node :value) (make-const-buffer (buffer-dtype (car (relay-writes (read-type-relay node))))))))
      (:!=               (make-expr :!= (use (second parents)) (use (third parents))))
      (:<                (make-expr :< (use (second parents)) (use (third parents))))
      (:MOVE
       (if (getattr node :_jit_dont_render_me)
	   (use (car parents))
	   (use (second parents))))
      (:STORE (use (second parents)))
      (otherwise
       (if (and (eql (node-type node) :ADD) (>= (length parents) 3))
	   (flet ((add (x y) (make-expr :ADD x y)))
	     (mapc #'use parents)
	     (reduce #'add parents))
	   (progn
	     (assert (<= (length parents) 3) () "~a cannot be grouped to multi expr! (too many arguments)" node)
	     (mapc #'use parents)
	     (apply #'make-expr (node-type node) parents)))))))

;; ~~ MULTIEXPR CREATION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *aref-list* nil)
(defun recursively-group-expr (across-group graph outputs read-by-time &key (no-multi-expr nil) (seen))
  (declare (type graph graph) (type list outputs))
  (let* ((stash) (read-by-time (apply #'append read-by-time)) (buffers))
    (flet ((pause? (x) (declare (type node x))
	     (or (find (car (node-writes x)) across-group)
		 (not (typep (node-type x) 'op/expr))
		 (> (count (car (node-writes x)) read-by-time) 1)
		 no-multi-expr))
	   (first? (x) (declare (type node x)) (not (find `(,(car (node-writes x)) ,(node-id x)) seen :test #'equal)))
	   (stash (x) (declare (type node x)) (push (car (node-writes x)) stash))
	   (saw (x) (declare (type node x)) (push `(,(car (node-writes x)) ,(node-id x)) seen))
	   (buffer-eq (x y) (and (equal (buffer-shape x) (buffer-shape y))
				 (equal (buffer-dtype x) (buffer-dtype y))
				 (equal (buffer-views x) (buffer-views y))))
	   (make-aref-helper (id type &aux (arf (make-aref id type)))
	     (push arf buffers)
	     arf))
      (labels ((fuse-helper (id &key (type nil) (args nil) &aux (x (id->value graph id)))
		 (if (and x (first? x))
		     (progn
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
					    (progn
					      (saw x)
					      (fuse-helper arg :type type :args t)))
				      else
					collect (if (symbolp arg) (make-aref-helper arg type) (make-const arg type)))))
			 (apply #'air->expr x parents)))
		     (if (symbolp id)
			 (when args (make-aref-helper id type))
			 (make-const id type))))
	       (fuse (x &aux (*aref-list*) (node (id->value graph x)) (type (when node (car (relay-writes (read-type-relay node))))))
		 (when node
		   (if (pause? node)
		       (if (typep (node-type node) 'op/expr)
			   (list (fuse-helper x :type type) node *aref-list*)
			   (let ((reads (node-reads node)))
			     (mapc #'(lambda (x &aux (n (id->value graph x))) (when n (stash n))) reads)
			     node))
		       (list (fuse-helper x :type type) node *aref-list*)))))
	(setf stash (reverse stash))
	(let ((exprs (map 'list #'fuse outputs)))
	  (loop while stash do (setf exprs (append (list (fuse (pop stash))) exprs)))
	  (assert (null stash))
	  (setf exprs (loop for e in exprs if e collect e)
		exprs (remove-duplicates exprs :key #'(lambda (x) (if (node-p x) (node-id x) (gensym)))))
	  (let* ((exprs
		   (loop for expr in exprs
			 if (listp expr)
			   collect
			   (multiple-value-bind (expr node arefs) (apply #'values expr)
			     (let* ((out-type (if (eql (node-type node) :WHERE)
						  (second (relay-reads (read-type-relay node)))
						  (car (relay-reads (read-type-relay node)))))
				    (out-to (if (eql (node-type node) :WHERE)
						(if (symbolp (second (node-reads node)))
						    (second (node-reads node))
						    (or
						     (second (getattr node :_reads_old_for_multiexpr))
						     (error "Cannot infer the output id for ~a" expr)))
						(let* ((arefs1 (loop for a in arefs if (eql (expr-op a) :AREF) collect a))
						       (c (find out-type arefs1 :key #'expr-y :test #'buffer-eq)))
						  (if c
						      (expr-x c)
						      (if (symbolp (car (node-reads node)))
							  (car (node-reads node))
							  (or
							   (car (getattr node :_reads_old_for_multiexpr))
							   (error "Cannot infer the output id for ~a" expr))))))))
			       ;; EXPR (out-to aref ...)
			       (assert (symbolp out-to))
			       (assert (not (eql (car (node-writes node)) out-to)) () "Breaks the structure of the DAG")
			       (make-node :JIT :EXPR (node-writes node) `(,out-to ,@(map 'list #'expr-x arefs))
					  :expr expr :_type_relay (make-inferred-type `(,out-type ,@(map 'list #'expr-y arefs)) (relay-writes (read-type-relay node)))
					  :reduction (getattr node :reduction :allow-undefined t))))
			 else
			   collect expr)))
	    ;; :expr nil is removed
	    (setf exprs (loop for e in exprs if (or (not (eql (node-type e) :EXPR)) (getattr e :expr)) collect e))
	    (values
	     exprs
	     (remove-duplicates
	      (loop for expr in exprs
		    if (eql (node-type expr) :EXPR)
		      append (node-reads expr))))))))))

(defun graph-out-to (graph) (remove-duplicates (apply #'append (map 'list #'(lambda (x) (recursively-find-output-id x graph)) (graph-seen graph)))))
(defun si->graph (schedule)
  (let ((g (apply #'make-graph (si-nodes schedule))))
    (setf (graph-seen g) (nodes-depends-on (si-nodes schedule)))
    g))
(defun apply-multiexpr-grouping (schedules across-group-deps)
  "Group several computation into a single :EXPR Node to simplify.
Especially, when rendering the code, all computation should be expressed as :EXPR except for the CUSTOM Kernel.
For example:
  T0 : X <- sin(m)
  T1 : Y <- cos(X)
is rewritten as:
  T0+T1 : Y <- cos(sin(m))
Here, X is a temporary variable if it is not specified in across-group-deps, so it and its allocation can be purged from the graph!
It uses aIR graph features (id->value, etc); accordingly must be applied before doing memory-planner optimization!
across-group-deps = save-for-backward in the almost use-case."
  (declare (type list schedules across-group-deps))
  (let ((tn-read-list) (removed-vars))
    ;; Do not apply multiexpr across different timestamp. e.g.:
    ;;       Before       =>       After
    ;; T0 { A = 1 + 1 
    ;;    { B = sin(A)    => C = sin(sin(1+1))
    ;;    { C = sin(B)
    ;; T1 { D = sin(B)
    ;;    { E = sin(D)    => F = sin(sin(B)) <- B is purged! (need a stopper when folding T0)
    ;;    { F = sin(E)
    (loop for schedule in schedules
	  for reads = (loop for node in (si-nodes schedule)
			    append
			    (remove-duplicates (loop for r in (node-reads node) if (symbolp r) collect r)))
	  do (push reads tn-read-list))
    ;; read-by-time = (T0_Read_list T1_Read_List, ..., Tn_Read_list)
    ;; T0 Read_list should not rewrite the expr that violates {T1~Tn}_Read_list deps!
    (setf tn-read-list (reverse tn-read-list))
    (loop for schedule in schedules
	  for n upfrom 0
	  for graph = (si->graph schedule)
	  for out-to = (graph-out-to graph)
	  for across-read-dep-list = (nthcdr n tn-read-list) do
	    (when out-to
	      (multiple-value-bind (expr-nodes expr-reads)
		  (recursively-group-expr across-group-deps graph out-to across-read-dep-list)
		(nconc removed-vars expr-reads)
		(setf (si-nodes schedule) expr-nodes))))
    removed-vars))


