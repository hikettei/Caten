(in-package :caten/ajit)
;; Here, applying a jit-specific optimization to the avm.graph.
;; E.g.: we can purge the view nodes since we already have a
;; type information at `type-relay.lisp`.
;; TODO: Refactor around typing relay (esp: memory-write dependencies are messing)
;; ~~ Step1, Before Grouping Process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %safely-purge-views-from-graph (avm)
  "(1.) Removes :VIEW from avm graph, (2.) Updates the read/write graph relations"
  (declare (type avm avm))
  (let ((views
	  (loop for node in (graph-nodes (avm-graph avm))
		if (eql (node-type node) :View)
		  collect node)))
    (setf (graph-nodes (avm-graph avm))
	  (loop for node in (graph-nodes (avm-graph avm))
		unless (eql (node-type node) :View)
		  collect node))
    (loop for view in views
	  for key = (car (node-writes view))
	  for to = (car (node-reads view)) do
	    (flet ((new (id) (if (eql id key) to id)))
	      (dolist (n (graph-nodes (avm-graph avm))) (setf (node-reads n) (map 'list #'new (node-reads n))))))))

(defun wmma-relay-from (t1 tc nth)
  (make-inferred-type `(,(nth nth (relay-reads tc)) ,@(relay-reads t1)) (relay-writes tc)))
(defun wmma-relay-from1 (t1 t2 t3)
  (make-inferred-type `(,@(relay-writes t3) ,(second (relay-reads t1)) ,(second (relay-reads t2))) (relay-writes t3))
  (setf (nth 1 (relay-reads t3)) (second (relay-reads t1)))
  t3)
;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b) :_type_relay t1) c) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 1)))
    ((:Add (c (:Mul (a b) :_type_relay t1)) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 0))))
(defsimplifier
    (contiguous-after-wmma :speed 0)
    ((:WMMA (c (:Move (_ a) :_type_relay t1) (:Move (_ b) :_type_relay t2)) :reduction reduction :_type_relay t3) -> (:WMMA (c a b) :reduction reduction :_type_relay (wmma-relay-from1 t1 t2 t3))))

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
