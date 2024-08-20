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
		       collect (progn (push (node-id node) seen) node))))
     pipeline)
    (maphash
     #'(lambda (k graph)
	 (when (every #'(lambda (x) (or (eql (node-type x) :FOR) (eql (node-type x) :ENDFOR))) (graph-nodes graph))
	   (remhash k pipeline)))
     pipeline)))

;; ~~ Step3, Before Rendering Process (pipeline w/o DOMAIN)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Reference-Counter
	    (:conc-name refcount-))
  (alias (make-hash-table) :type hash-table)
  (refcount (make-hash-table) :type hash-table)
  (refcount-by (make-hash-table) :type hash-table))
(defun node-output-position (node) (case (node-type node) (:WHERE (second (node-reads node))) (otherwise (car (node-reads node)))))
(defun create-reference-count (graph)
  (declare (type graph graph))
  (let ((refcount (make-hash-table))
	(refby (make-hash-table)))
    (labels ((relevant-graph (pos) (apply #'make-graph (subseq (graph-nodes graph) pos)))
	     (inplace (node pos)
	       ;; (in-place-p . users)
	       (multiple-value-bind (count users) (id->memwrites (relevant-graph pos) (car (node-writes node)))
		 (cons count users))))	     
      (loop for node in (graph-nodes graph)
	    for pos upfrom 0
	    for (refc . refb) = (inplace node pos)
	    do (setf (gethash (car (node-writes node)) refcount) refc
		     (gethash (car (node-writes node)) refby) refb)))
    (make-reference-counter :refcount refcount :refcount-by refby)))

(defun refcount/refalias (count id)
  (let ((val (gethash id (refcount-alias count))))
    (if val
	(if (eql id val)
	    val
	    (refcount/refalias count val))
	id)))

(defun refcount/make-alias (count node in-place)
  (if (eql (node-type node) :Allocate)
      (setf (gethash (car (node-writes node)) (refcount-alias count)) (car (node-writes node)))
      (if in-place
	  (setf (gethash (car (node-writes node)) (refcount-alias count)) (node-output-position node))
	  (setf (gethash (car (node-writes node)) (refcount-alias count)) (car (node-writes node))))))

(defun refcount/update-buffer (count buffer)
  (flet ((new (x) (refcount/refalias count (reveal-buffer x))))
    (setf (buffer-shape buffer) (map 'list #'new (buffer-shape buffer))
	  (buffer-stride buffer) (map 'list #'new (buffer-stride buffer))
	  (buffer-views buffer)
	  (loop for v in (buffer-views buffer)
		if v
		  collect (list (new (nth 0 v)) (new (nth 1 v)) (new (nth 2 v)) (nth 3 v))
		else
		  collect v))))

(defun refcount/update-node (node refcount)
  (declare (type node node))
  (flet ((ref (x) (refcount/refalias refcount x)))
    (when (eql (node-type node) :EXPR)
      (let ((buffers (getattr node :buffers)))
	(assert (every #'(lambda (x) (eql :AREF (expr-op x))) buffers))
	(mapc
	 #'(lambda (aref)
	     (setf (expr-x aref) (ref (expr-x aref))))
	 buffers)))
    ;;(assert (null (getattr node :_reads)))
    ;;(assert (null (getattr node :_writes)))
    (map 'list #'(lambda (x) (when x (refcount/update-buffer refcount x))) `(,@(relay-writes (read-type-relay node)) ,@(relay-reads (read-type-relay node))))
    (setf (getattr node :_loop_bound_nodes) (map 'list #'ref (getattr node :_loop_bound_nodes))
	  (getattr node :_reads)  (node-reads node)
	  (getattr node :_writes) (node-writes node)
	  (node-reads node) (map 'list #'ref (node-reads node))
	  (node-writes node) (map 'list #'ref (node-writes node)))))

(defun id->memwrites (graph id)
  "Counts how many times id was read in the graph, and by who?"
  (let ((count 0)
	(nodes))
    (loop for node in (graph-nodes graph)
	  for reads = (node-reads node) do
	    (loop for r in reads
		  if (eql r id) do (incf count) (push node nodes)))
    (values count (remove-duplicates nodes :key #'node-id))))

(defun apply-memory-planner (refcount polyhedron avm schedule-graph)
  (declare (type Reference-counter refcount) (type Polyhedral polyhedron) (type avm avm))
  (let* ((pipeline (poly-pipeline polyhedron))
	 (pipeline-ids (loop for r in (graph-nodes schedule-graph) if (eql (node-type r) :FUNCALL) collect (getattr r :idx)))
	 (pipeline-ids-by-loop (render-graph/sort-by-time schedule-graph))
	 (allocated-items
	   (loop for time in `(,@pipeline-ids)
		 append
		 (loop for node in (graph-nodes (gethash time pipeline))
		       if (eql (node-type node) :Allocate)
			 collect (car (node-writes node)))))
	 (allocated-items (append allocated-items
				  (poly-vm-inputs polyhedron)
				  (loop for o in (poly-vm-outputs polyhedron)
					if (poly/io-scalar-p polyhedron o) collect o))))
    (assert (equal (flatten pipeline-ids-by-loop) pipeline-ids))
    (labels ((inplace-p (node time)
	       ;; (in-place-p . intersects-with-current-pipeline?)
	       (when (eql (node-type node) :Allocate) (return-from inplace-p (cons t t)))
	       (let* ((id (node-output-position node))
		      (refcount-n (gethash id (refcount-refcount refcount)))
		      (refdom     (gethash id (refcount-refcount-by refcount))))
		 (dolist (r (node-reads node)) (when (symbolp r) (decf (gethash r (refcount-refcount refcount)))))
		 ;;(assert (>= refcount-n -1))
		 (cons
		  (or (<= refcount-n 1) (print (depends-across-pipeline? id)))
		  (every #'(lambda (node) (find (node-id node) (graph-nodes (gethash time pipeline)) :key #'node-id)) refdom))))
	     (depends-across-pipeline? (id)
	       (find id (poly-deps-across-group polyhedron)))
	     (newid (x) (refcount/refalias refcount x)))
      ;; O(nlogn) * the cost of id->users ...
      (loop for loop-ids in pipeline-ids-by-loop do
	(loop
	  for time in loop-ids
	  for graph = (gethash time pipeline) do
	    (loop
	      for node in (graph-nodes graph)
	      for (inplace-p . all-exists-in-the-same-pipeline) = (inplace-p node time) do
		(assert (= 1 (length (node-writes node))) ())
		(refcount/make-alias refcount node inplace-p)
		;; If write-to area is not going to be used by any other ops, let's make it in-place
		;; otherwise:
		;;  - If write-to-user exists in the same schedule -> create a tmpvar.
		;;  - If write-to-user exists in the another schedule -> they are save-for-backwards, lets keep them copying
		
		(when (and (null inplace-p) all-exists-in-the-same-pipeline)
		  ;; [TODO]
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
		  )
		(refcount/update-node node refcount))))

      ;; Update allocated-items
      (setf allocated-items (map 'list #'newid allocated-items))
      (loop for time in `(,@pipeline-ids)
	    for graph = (gethash time pipeline) do
	      (loop for node in (graph-nodes graph)
		    for type = (read-type-relay node) do
		      (loop for id in `(,@(node-writes node) ,@(node-reads node))
			    for typ in `(,@(relay-writes type) ,@(relay-reads type))
			    if (and (symbolp id) (null (find id allocated-items))) do
			      (push id allocated-items)
			      (setf (graph-nodes (gethash time pipeline))
				    (append
				     (list (make-node :Buffer :Allocate
						      (list id) (map 'list #'reveal-buffer `(,@(buffer-shape typ) ,@(buffer-stride typ)))
						      :nrank (buffer-nrank typ)
						      :dtype (buffer-dtype typ)
						      :_type_relay (make-inferred-type nil (list typ))
						      :_not_a_input t))
				     (graph-nodes (gethash time pipeline)))))))
      
      (flet ((replacer (x) (refcount/refalias refcount x)))
	(loop for g in (graph-nodes schedule-graph)
	      if (eql (node-type g) :FOR) do
		(expr-recursive-replace (getattr g :below) #'replacer)
		(expr-recursive-replace (getattr g :upfrom) #'replacer)
		(expr-recursive-replace (getattr g :by) #'replacer)))
      
      (setf (avm-fw-outputs avm) (map 'list #'newid (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'newid (avm-bw-outputs avm))
	    ;; vm-inputs are fixed (they are dynamic shapes)
	    (poly-vm-outputs polyhedron) (map 'list #'newid (poly-vm-outputs polyhedron)))

      (macrolet ((renew (accessor)
		   `(let ((new-table (make-hash-table)))
		      (maphash
		       #'(lambda (k v)
			   (setf (gethash (newid k) new-table) v))
		       ,accessor)
		      (setf ,accessor new-table))))
	(renew (avm-id2tensor avm))
	(renew (poly-vm-io-types polyhedron))
	(renew (avm-variables avm))))))
