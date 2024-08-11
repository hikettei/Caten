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
		      (setf (node-writes n) (map 'list #'(lambda (x) (or (->aft x) x)) (node-writes n)))
		      n))))))

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
(defun get-parent-nodes (node graph)
  (declare (type node node) (type graph graph))
  (append
   ;; :Allocate is placed in the VM-level
   ;; nodes used to compute the shape is never used!
   (when (not (eql (node-type node) :Allocate))
     (loop for r in `(,@(node-reads node) ,@(getattr node :_loop_bound_nodes))
	   if (symbolp r) append (get-parent-nodes (id->value graph r) graph)))
   (list node)))
;; no duplicates are allowed
(defun %simplify-pipeline (pipeline top-ids)
  "Removes all unused nodes from the pipeline (incl, :FOR, :ENDFOR)"
  (declare (type hash-table pipeline))
  (let* ((tmpgraph
	   (apply
	    #'make-graph
	    (loop for graph being the hash-values of pipeline
		  append (graph-nodes graph))))
	 (top-ids (append
		   top-ids
		   (loop for value being the hash-values of pipeline
			 append (graph->loop-size value))))
	 ;; Q, What nodes should be included in the body?
	 ;; - all subgraph from top-ids
	 ;; - all nodes used to determine the bound of :FOR
	 (used-nodes (map 'list #'(lambda (x &aux (val (id->value tmpgraph x))) (and val (get-parent-nodes val tmpgraph))) top-ids))
	 (used-node-ids (map 'list #'node-id (apply #'append used-nodes)))
	 (seen))
    (maphash
     #'(lambda (k graph)
	 (declare (ignore k))
	 (setf (graph-nodes graph)
	       (loop for node in (graph-nodes graph)
		     if (and (null (find (node-id node) seen)) (or (find (node-type node) `(:FOR :ENDFOR)) (find (node-id node) used-node-ids)))
		       collect (progn (push (node-id node) seen)  node))))
     pipeline)
    (maphash
     #'(lambda (k graph)
	 (when (every #'(lambda (x) (or (eql (node-type x) :FOR) (eql (node-type x) :ENDFOR))) (graph-nodes graph))
	   (remhash k pipeline)))
     pipeline)))

;; ~~ Step3, Before Rendering Process (pipeline w/o DOMAIN)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-multiexpr-grouping (pipeline)
  "Group several computation into a single :EXPR Node to simplify.
E.g.:
  T0 : X <- sin(m)
  T1 : Y <- cos(X)
is updated to:
  T0+T1 : Y <- cos(sin(m))
It uses aIR graph features; accordingly must be applied before doing memory-planner optimization!"
  (declare (type hash-table pipeline))
  (let ((copied-graph (make-hash-table)))
    (maphash
     #'(lambda (ts graph)
	 (declare (type graph graph))
	 (let* ((toplevels (graph-seen graph))
		(outputs
		    (remove-duplicates
		     (apply #'append (map 'list #'(lambda (x) (recursively-find-output-id x graph)) toplevels))))
		(out-memory-ids
		  (map
		   'list
		   #'(lambda (x &aux (node (id->value graph x)))
		       (when node
			 (case (node-type node)
			   (:WHERE (list (second (node-reads node)) (second (relay-reads (read-type-relay node)))))
			   (otherwise (list (first (node-reads node)) (first (relay-reads (read-type-relay node))))))))
		   outputs))
		(out-types
		  (map
		   'list
		   #'(lambda (x &aux (node (id->value graph x)))
		       (when node
			 (car (relay-writes (read-type-relay node)))))
		   outputs)))
	   (when (and
		  outputs
		  (every #'identity out-memory-ids)
		  (every
		   #'(lambda (x &aux (type (node-type x)))
		       (and (vm-instruction-p x)
			    ;; Only element wise ops are fused!
			    (not (find type `(:Allocate :MOVE :WHERE :WMMA :STORE)))
			    (if (eql (node-class x) :BinaryOps)
				(= (length (node-reads x)) 2)
				t)))
		   (graph-nodes graph)))
	     (let ((fused-nodes (map 'list #'(lambda (x xt r) (create-multiexpr-node graph x xt (first r) (second r))) outputs out-types out-memory-ids)))
	       (setf (gethash ts copied-graph) (apply #'make-graph (copy-list fused-nodes)))))))
     pipeline)
    copied-graph))

(defun create-alias-map (pipeline pipeline-ids alloc-ids)
  "Creates a hash-table if (car reads) becomes write rule is applied."
  (declare (type hash-table pipeline))
  (let ((table (make-hash-table)))
    (labels ((load-from-map (x) (or (gethash x table) x))
	     (alias (k v) (setf (gethash k table) (load-from-map v))))
      (loop for key in `(,@alloc-ids ,@pipeline-ids)
	    for graph = (gethash key pipeline) do
	      (loop for node in (graph-nodes graph) do
		(case (node-type node)
		  ;; whichth args to write the result?
		  (:ALLOCATE (alias (car (node-writes node)) (car (node-writes node))))
		  (:WHERE    (alias (car (node-writes node)) (second (node-reads node))))
		  (otherwise (alias (car (node-writes node)) (car (node-reads node))))))))
    table))

(defun create-ref-count (pipeline nth)
  (let ((refcount (make-hash-table)))
    (labels ((c (x)
	       (if (null (gethash x refcount))
		   (setf (gethash x refcount) 0)
		   (incf (gethash x refcount)))))
      (loop with graph = (gethash nth pipeline)
	    for node in (graph-nodes graph) do
	      (case (node-type node)
		(:ALLOCATE nil)
		(otherwise
		 (map 'list #'c (node-reads node))))))
    refcount))

(defun alias/update-node (node alias)
  (declare (type node node))
  (flet ((ref (x) (if (symbolp x) (or (gethash x alias) x) x)))
    (when (eql (node-type node) :EXPR)
      (let ((buffers (getattr node :buffers)))
	(assert (every #'(lambda (x) (eql :AREF (expr-op x))) buffers))
	(mapc
	 #'(lambda (aref)
	     (setf (expr-x aref) (ref (expr-x aref))))
	 buffers)))
    ;;(assert (null (getattr node :_reads)))
    ;;(assert (null (getattr node :_writes)))
    (setf (getattr node :_loop_bound_nodes) (map 'list #'ref (getattr node :_loop_bound_nodes))
	  (getattr node :_reads)  (node-reads node)
	  (getattr node :_writes) (node-writes node)
	  (node-reads node) (map 'list #'ref (node-reads node))
	  (node-writes node) (map 'list #'ref (node-writes node)))))

(defun create-multiexpr-grouped-pipeline (pipeline pipeline-ids alloc-ids &key (multiexpr t))
  (let ((alias-map (create-alias-map pipeline pipeline-ids alloc-ids))
	(out-pipeline (make-hash-table))
	(new-pipeline (if multiexpr (apply-multiexpr-grouping pipeline) pipeline)))
    (loop for key being the hash-keys of pipeline
	  for graph = (gethash key pipeline)
	  for multi = (gethash key new-pipeline)
	  if multi do
	    (map 'list #'(lambda (x) (alias/update-node x alias-map)) (graph-nodes multi))
	    (setf (gethash key out-pipeline) multi)
	  else do
	    (map 'list #'(lambda (x) (alias/update-node x alias-map)) (graph-nodes graph))
	    (setf (gethash key out-pipeline) graph))
    (maphash
     #'(lambda (k _)
	 (declare (ignore _))
	 (setf (gethash k pipeline) (gethash k out-pipeline)))
     pipeline)
    alias-map))

(defun apply-memory-planner (pipeline avm schedule-graph &key (multiexpr t))
  (declare (type hash-table pipeline) (avm avm))
  (let* ((pipeline-ids (loop for r in (graph-nodes schedule-graph) if (eql (node-type r) :FUNCALL) collect (getattr r :idx)))
	 (alloc-ids (loop for k in (hash-table-keys pipeline) unless (find k pipeline-ids) collect k))
	 (alias-map (create-multiexpr-grouped-pipeline pipeline pipeline-ids alloc-ids :multiexpr multiexpr))
	 (tmpvar-table (make-hash-table))
	 (t-1 (make-graph)))
    ;; Minimizing the number of allocations by following the rule:
    ;; 1. (car reads) becomes write, (except for %WHERE)
    ;;  | -   A <- f(B, C, D)
    ;;  | ->  B <- f(B, C, D)
    ;; 2. If (car reads) is duplicated in the graph, allocates tmpvar e.g.:
    ;;  | -   A1 <- f(A, B)
    ;;  | -   A2 <- f(A, C)
    ;;  | -   O1 <- f(A1, A2)
    ;;   ->
    ;;  | -   At1 <- f(A, B)
    ;;  | -   At2 <- f(A, C)
    ;;  | -   O1 <-  f(At1, At2)
    ;;  Where At1, At2 is a scalar value, being rendered `float _val_0_0` in clang.
    (loop for key in `(,@alloc-ids ,@pipeline-ids)
	  for refcount = (create-ref-count pipeline key)
	  for graph = (gethash key pipeline) do
	    (labels ((refcount (x) (or (gethash x refcount) 0))
		     (no-alias-p (x) (<= (refcount x) 2))
		     (maybe-tmp (x default) (or (gethash x tmpvar-table) default))
		     (maybe-tmp1 (x &aux (x (reveal-buffer x)))
		       (let ((aliased (gethash x alias-map x)))
			 (maybe-tmp aliased aliased)))
		     (->scalar (x) (make-buffer 0 nil nil (buffer-dtype x) nil))
		     (->alloc (name type)
		       (make-node :Buffer :Allocate (list name) nil :nrank 0
				  :dtype (buffer-dtype (car (relay-writes type))) :_tmp t
				  :_type_relay (make-inferred-type (list (->scalar (car (relay-writes type)))) nil)))
		     (->store (tgt load-what type)
		       (make-node :Buffer :Load (list tgt) (list tgt) :value load-what
				  :_type_relay (make-inferred-type (list (car (relay-writes type))) (list (car (relay-writes type))))))
		     (update-buffer (buffer)
		       (setf (buffer-shape buffer) (map 'list #'maybe-tmp1 (buffer-shape buffer))
			     (buffer-stride buffer) (map 'list #'maybe-tmp1 (buffer-stride buffer))
			     (buffer-views buffer)
			     (loop for v in (buffer-views buffer)
				   if v
				     collect (list (maybe-tmp1 (nth 0 v)) (maybe-tmp1 (nth 1 v)) (maybe-tmp1 (nth 2 v)) (nth 3 v))
				   else
				     collect v)))
		     (update-relay-reference(type)
		       (mapc #'update-buffer (relay-writes type))
		       (mapc #'update-buffer (relay-reads type)))	       
		     (updt-tmp (node type r-old)
		       (setf
			(relay-reads type) (map 'list #'(lambda (x y) (if (gethash x tmpvar-table) (->scalar y) y)) r-old (relay-reads type))
			(node-reads node) (map 'list #'maybe-tmp r-old (node-reads node)))))
	      (let ((allocs) (stores (make-hash-table)))
		(loop for node in (graph-nodes graph)
		      for type = (read-type-relay node)
		      for old-reads  = (getattr node :_reads)
		      for old-writes = (getattr node :_writes)
		      unless (eql (node-type node) :Allocate) do
			(when (not (no-alias-p (car (node-writes node))))
			  ;; Create a new tmpvar:
			  (let ((tmpvar-name (intern (format nil "_~(~a~)_~a" (car old-writes) key))))
			    (push (->alloc tmpvar-name type) allocs)
			    (setf (gethash (car (node-writes node)) stores)
				  (->store (car (node-writes node)) tmpvar-name type))				  
			    (setf (gethash (car old-writes) tmpvar-table) tmpvar-name
				  (car (node-writes node)) tmpvar-name
				  (car (relay-writes type)) (->scalar (car (relay-writes type))))))
			(update-relay-reference type)
			(updt-tmp node type old-reads))
		(when (find key alloc-ids)
		  (dolist (node (graph-nodes graph))
		    (when (eql (node-type node) :Allocate)
		      (when (not (alloc-args-p node))
			(setf (graph-nodes t-1) (append (graph-nodes t-1) (list node)))))))
		(assert (every #'(lambda (x) (eql (node-type x) :Allocate)) allocs))
		(setf (graph-nodes graph) (append (reverse allocs) (graph-nodes graph) (hash-table-values stores)))
		;; Tmpvar cannot be used across the different timestamp
		(setf tmpvar-table (make-hash-table)))))
    (flet ((load-from-map (x) (or (gethash x alias-map) x)))
      (loop for g in (graph-nodes schedule-graph)
	    if (eql (node-type g) :FOR) do
	      (let ((below (getattr g :below)))
		(expr-recursive-replace below alias-map)))
      (setf (graph-nodes t-1) (remove-duplicates (graph-nodes t-1) :key (compose #'car #'node-writes)))
      (setf (graph-nodes t-1)
	    (loop with seen = (%seen-ids-for-unused-scalar-nodes pipeline pipeline-ids)
		  for n in (graph-nodes t-1)
		  if (find (car (node-writes n)) seen)
		    collect n))
      (when (graph-nodes t-1)
	(setf (gethash -1 pipeline) t-1)
	(setf (graph-nodes schedule-graph) (append (list (r/funcall "T-1" nil)) (graph-nodes schedule-graph))))
      (setf (avm-fw-outputs avm) (map 'list #'load-from-map (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'load-from-map (avm-bw-outputs avm)))
      (let ((new-id2tensor (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (load-from-map k) new-id2tensor) v))
	 (avm-id2tensor avm))
	(setf (avm-id2tensor avm) new-id2tensor))
      (let ((new-variables (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (load-from-map k) new-variables) v))
	 (avm-variables avm))
	(setf (avm-variables avm) new-variables)))))

(defun %seen-ids-for-unused-scalar-nodes (pipeline keys)
  (declare (type hash-table pipeline))
  (remove-duplicates
   (loop for key in keys
	 for graph = (gethash key pipeline)
	 append
	 (loop for node in (graph-nodes graph)
	       for type = (read-type-relay node)
	       append (remove-duplicates
		       (append (unless (eql (node-type node) :EXPR) (loop for r in (node-reads node) if (symbolp r) collect r))
			       (flatten
				`(,@(map 'list #'buffer-reconstruct-view-args (relay-writes type))
				  ,@(map 'list #'buffer-reconstruct-view-args (relay-reads type))))))))))


