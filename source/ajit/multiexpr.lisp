(in-package :caten/ajit)

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

;; ~~ [From aIR -> Expr] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(macrolet ((expr (name (&rest args) (&rest types))
	     `(defun ,(symb 'make- name) (,@args)
		(declare ,@(loop for arg in args for typ in types collect `(type ,typ ,arg)))
		(make-expr ,(intern (symbol-name name) "KEYWORD") ,@args))))
  ;; Buffer Ops
  (expr Const (obj) (t))
  (expr Cast  (obj dtype) (Expr Keyword))
  (expr Aref  (id  buffer) (Symbol Buffer)))

(declaim (ftype (function (Node &rest t) (values Expr &optional)) air->expr))
(defun air->expr (node &rest parents)
  (flet ((use (obj) (when (and (expr-p obj) (eql (expr-op obj) :AREF)) (push obj *aref-list*)) obj))
    (case (node-type node)
      (:INDEX-COMPONENTS (make-expr :INDEX-COMPONENTS (use (first parents)) (map 'list #'use (cdr parents))))
      (:Cast             (make-cast (use (second parents)) (getattr node :dtype)))
      (:Load             (make-const (getattr node :value)))
      (:!=               (make-expr :!= (use (second parents)) (use (third parents))))
      (:<                (make-expr :< (use (second parents)) (use (third parents))))
      (:MOVE
       (if (getattr node :_jit_dont_render_me)
	   (use (car parents))
	   (use (second parents))))
      (:STORE            (use (second parents)))
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
(defun recursively-group-expr (across-group graph outputs read-by-time  &key (no-multi-expr nil) (seen))
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
					collect (if (symbolp arg) (make-aref-helper arg type) (make-const arg)))))
			 (apply #'air->expr x parents)))
		     (if (symbolp id)
			 (when args (make-aref-helper id type))
			 (make-const id))))
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
						  (second (relay-reads(read-type-relay node)))
						  (car (relay-reads (read-type-relay node)))))
				    (out-to (if (eql (node-type node) :WHERE)
						(second (node-reads node))
						(let ((c (find out-type arefs :key #'expr-y :test #'buffer-eq)))
						  (if c
						      (expr-x c)
						      (car (node-reads node)))))))
			       ;; EXPR (out-to aref ...)
			       (make-node :EXPR :EXPR (node-writes node) `(,out-to ,@(map 'list #'expr-x arefs))
					  :expr expr :_type_relay (make-inferred-type `(,out-type ,@(map 'list #'expr-y arefs)) (relay-writes (read-type-relay node)))
					  :buffers arefs)))
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


