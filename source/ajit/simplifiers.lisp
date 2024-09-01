(in-package :caten/ajit)
;; Here, applying a jit-specific optimization to the avm.graph.
;; E.g.: we can purge the view nodes since we already have a
;; type information at `type-relay.lisp`.
;; TODO: Refactor around typing relay (esp: memory-write dependencies are messing)
;; ~~ Step1, Before Grouping Process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %safely-purge-views-from-graph (avm)
  "(1.) Removes :VIEW from avm graph, (2.) Updates the read/write graph relations"
  (declare (type avm avm))
  (let ((rewrite-map
	  (loop for node in (graph-nodes (avm-graph avm))
		if (eql (node-type node) :View)
		  collect (cons (car (node-reads node)) (car (node-writes node))))))
    (labels ((->find (id) (find id rewrite-map :key #'car :test #'eql))
	     (view-composed? (id &aux (node (id->value (avm-graph avm) id)))
	       (and node (eql (node-type node) :View)))
	     (->aft (id &aux (last id))
	       (if (symbolp id)
		   (labels ((f (x &aux (next (->find x)))
			      (if next
				  (progn
				    (setf last (cdr next))
				    (if (view-composed? (cdr next))
					(f (cdr next))
					last))
				  nil)))
		     (f last)
		     last)
		   id)))
      (setf (graph-nodes (avm-graph avm))
	    (loop for n in (graph-nodes (avm-graph avm))
		  unless (eql (node-type n) :View)
		    collect
		    (progn		      
		      (setf (node-writes n) (map 'list #'(lambda (x) (or (->aft x) x)) (node-writes n))
			    (node-reads n) (loop for r in (node-reads n)
						 for val = (id->value (avm-graph avm) r)
						 if (and (symbolp r) (eql (node-type val) :View))
						   collect (->aft (car (node-writes val)))
						 else
						   collect r))
		      n))))))

(defun wmma-relay-from (t1 tc nth)
  (make-inferred-type `(,(nth nth (relay-reads tc)) ,@(relay-reads t1)) (relay-writes tc)))
(defun wmma-relay-from-contiguous (t1 t2 t3)
  (let ((a-base (second (relay-reads t3)))
	(a (second (relay-reads t1)))
	(b-base (third (relay-reads t3)))
	(b (second (relay-reads t2)))
	(c (car (relay-reads t3))))
    (flet ((manual-infer (from to)
	     ;; from = broadcasted and contiguous-applied buffer
	     ;; to = before the `from` operation buffer.
	     (let ((from (copy-buffer from)))
	       (assert (= (buffer-nrank from) (buffer-nrank to)))
	       (assert (eql (buffer-dtype from) (buffer-dtype to)))
	       (setf (buffer-stride from) (buffer-stride to)
		     (buffer-inferred-permute from) (buffer-inferred-permute to)
		     ;; merge views (use broadcasted one)
		     (buffer-views from) (loop for n upfrom 0 below (buffer-nrank from)
					       if (and (nth n (buffer-views from)) (nth n (buffer-views to)))
						 collect (if (fourth (nth n (buffer-views from)))
							     (nth n (buffer-views from))
							     (nth n (buffer-views to)))
					       else
						 collect (or (nth n (buffer-views from))
							     (nth n (buffer-views to)))))
	       from)))
      (make-inferred-type `(,c ,(manual-infer a-base a) ,(manual-infer b-base b)) (relay-writes t3)))))
;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b) :_type_relay t1) c) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 1)))
    ((:Add (c (:Mul (a b) :_type_relay t1)) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 0))))
(defsimplifier
    (contiguous-after-wmma :speed 0)
    ((:WMMA (c (:Move (_ a) :_type_relay t1) (:Move (_ b) :_type_relay t2)) :reduction reduction :_type_relay t3) -> (:WMMA (c a b) :reduction reduction :_type_relay (wmma-relay-from-contiguous t1 t2 t3))))

(defun apply-jit-specific-simplifiers (avm)
  "A toplevel for jit-specific optimizers. (WMMA Simplification, Removing Views, Contiguous Node Removals)"
  (declare (type avm avm))
  (%safely-purge-views-from-graph avm)
  (wmma-rewriter (avm-graph avm) :no-verify t)
  (contiguous-after-wmma (avm-graph avm) :no-verify t))
;; ~~ Step2, Before Applying Polyhedral Compiler (pipeline w/ DOMAIN)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Under the step2, some nodes have the following attributes:
;; - :_loop_bound_nodes      : a list of nodes used to compute the bound
;; - :_loop_bound_nodes_type : types for _loop_bound_types
(defun get-parent-nodes (node graph &aux (seen nil))
  (declare (type node node) (type graph graph))
  (labels ((explore (node graph)
	     (when (null (find (node-id node) seen))
	       (push (node-id node) seen)
	       (append
		;; :Allocate is placed in the VM-level
		;; nodes used to compute the shape is never used!
		(when (not (eql (node-type node) :Allocate))
		  (loop for r in (append (node-reads node) (getattr node :_loop_bound_nodes))
			if (and (symbolp r) (id->value graph r)) append (explore (id->value graph r) graph)))
		(list node)))))
    (explore node graph)))

(defun create-reduction-alias-f (nodes)
  (let ((table (make-hash-table)))
    (labels ((ref (x)
	       (if (gethash x table)
		   (ref (gethash x table))
		   x)))
      (loop for node in nodes
	    if (getattr node :reduction)
	      do (setf (gethash (car (node-writes node)) table) (car (node-reads node)))
	    else
	      do (setf (node-reads node) (map 'list #'ref (node-reads node))))
      #'ref)))
