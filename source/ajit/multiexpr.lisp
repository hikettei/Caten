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

;; ~~ [From aIR -> Expr] ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  (case (node-type node)
    (:INDEX-COMPONENTS (make-expr :INDEX-COMPONENTS (first parents) (cdr parents)))
    (:Cast             (make-cast (second parents) (getattr node :dtype)))
    (:Load             (make-const (getattr node :value)))
    (:!=               (make-expr :!= (second parents) (third parents)))
    (:<                (make-expr :< (second parents) (third parents)))
    (:MOVE
     (if (getattr node :_jit_dont_render_me)
	 (car parents)
	 (second parents)))
    (:STORE            (second parents))
    (otherwise
     (if (and (eql (node-type node) :ADD) (>= (length parents) 3))
	 (flet ((add (x y) (make-expr :ADD x y)))
	   (reduce #'add parents))
	 (progn
	   (assert (<= (length parents) 3) () "~a cannot be grouped to multi expr! (too many arguments)" node)
	   (apply #'make-expr (node-type node) parents))))))

(defparameter *aref-list* nil)
(defun recursively-group-expr (across-group graph outputs read-by-time &key (no-multi-expr nil) (seen))
  (declare (type graph graph) (type list outputs))
  (let* ((stash) (read-by-time (apply #'append read-by-time)))
    (flet ((pause? (x) (declare (type node x))
	     (or (find (car (node-writes x)) across-group)
		 (not (typep (node-type x) 'op/expr))
		 (> (count (car (node-writes x)) read-by-time) 1)
		 no-multi-expr))
	   (first? (x) (declare (type node x)) (not (find `(,(car (node-writes x)) ,(node-id x)) seen :test #'equal)))
	   (stash (x) (declare (type node x)) (push (car (node-writes x)) stash))
	   (saw (x) (declare (type node x)) (push `(,(car (node-writes x)) ,(node-id x)) seen))
	   (make-aref-helper (arg type &aux (aref (make-aref arg type)))
	     (push aref *aref-list*)
	     aref))
      (labels ((fuse-helper (id &key (type nil) &aux (x (id->value graph id)))
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
					      (fuse-helper arg :type type)))
				      else
					collect (if (symbolp arg) (make-aref-helper arg type) (make-const arg)))))
			 (apply #'air->expr x parents)))
		     (if (symbolp id)
			 (progn (assert type) (make-aref-helper id type))
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
	 (let ((reads
		 (loop for node in (graph-nodes graph)
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
	   ;;(dolist (node (graph-nodes graph))
	   ;;  (assert (null (some #'(lambda (x) (find x removed-vars)) (node-reads node)))
	   ;;         ()
	   ;;	     "~a is removed by the multiexpr! (a bug)" (node-reads node)))
	   (when out-to
	     (multiple-value-bind (expr-nodes expr-reads) (recursively-group-expr (poly-deps-across-group poly) graph out-to (nthcdr c read-by-time))
	       (declare (type list expr-nodes expr-reads))
	       (setf removed-vars (remove-duplicates (append removed-vars (intersection (nth c read-by-time) expr-reads))))
	       (setf (graph-nodes graph) expr-nodes)))
	   (incf c))
       (poly-pipeline poly))
      removed-vars)))
