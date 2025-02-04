(in-package :caten/air)

(defclass Graph ()
  ((nodes :initarg :nodes :type list :accessor %graph-nodes)
   (seen :initarg :seen :initform nil :type list :accessor graph-seen)
   (outputs :initarg :output :initform nil :type list :accessor graph-outputs))
  (:documentation "A Graph is a data structure used to handle a collection of nodes.

Graph has a list of nodes (nodes), node inputs (seen), and node outputs (outputs).

Unlike `FastGraph`, Graph does not assume that the nodes form a DAG. Additionally, it guarantees that the nodes will not be sorted during each process of the Pattern Matcher. Graph structures, such as Let-Binding, are represented using Graph.

Note: Using Graph may lead to performance degradation if the graph is a DAG. Please use FastGraph instead."))

(defmethod print-object ((graph Graph) stream)
  (format stream "
Graph[seen=~a, outputs=~a] {
~a}
"
	  (graph-seen graph)
	  (graph-outputs graph)
	  (with-output-to-string (out)
	    (dolist (node (graph-nodes graph))
	      (format out "    ~a~%" node)))))

(defclass FastGraph (Graph)
  ((node-table :initform (make-hash-table :test 'eq) :type hash-table :accessor %graph-nodes-table))
  (:documentation "
`FastGraph` is a subclass of `Graph` that implements faster node searches based on the assumption that the nodes form a DAG. (It is approximately 20 times faster than Graph, in the larger scale.)
Since `FastGraph` stores nodes as a hash-table, there are some constraints on node operations. If necessary, converting between `(->fast-graph graph)` and `(->graph graph)` can be done frequently with minimal impact on performance."))

(defmethod print-object ((graph FastGraph) stream)
  (format stream "
FastGraph[seen=~a, outputs=~a] {
~a}
"
	  (graph-seen graph)
	  (graph-outputs graph)
	  (with-output-to-string (out)
	    (dolist (node (graph-nodes (->graph graph)))
	      (format out "    ~a~%" node)))))

(defun make-graph (&rest nodes)
  "
```
(make-graph &rest nodes)
```
Creates a new Graph object with the given nodes.
"
  (make-instance 'Graph :nodes nodes))

(defgeneric copy-graph (graph) (:documentation "
```
(copy-graph graph)
```
Creates a copy of the given graph.
"))

(defmethod copy-graph ((graph Graph))
  "Creates a copy of the given graph."
  (let ((g (apply #'make-graph (graph-nodes graph))))
    (setf (graph-seen g) (copy-list (graph-seen graph))
	  (graph-outputs g) (copy-list (graph-outputs graph)))
    g))

(defun graph-p (graph) (typep graph 'Graph))

(defmethod graph-nodes ((graph Graph)) (%graph-nodes graph))

(defmethod graph-nodes ((graph FastGraph)) (remove-duplicates (hash-table-values (%graph-nodes-table graph)) :key #'node-id))

(defmethod (setf graph-nodes) (nodes (graph Graph)) (setf (%graph-nodes graph) nodes))

(defmethod (setf graph-nodes) (nodes (graph FastGraph)) (error "graph-nodes for FastGraph is immutable!"))

(defgeneric id->value (graph id) (:documentation "
```
(id->value graph id)
```
Returns a node whose node-writes includes the given id."))

(defgeneric id->users (graph id) (:documentation "
```
(id->users graph id)
```
Returns a list of nodes whose node-reads includes the given id."))

(defmethod id->users ((graph Graph) id)
  (declare (optimize (speed 3)))
  (if (not (symbolp id))
      nil
      (loop for node in (graph-nodes graph)
	    if (find id (node-reads node) :test #'eql)
	      collect node)))

(defmethod id->value ((graph Graph) id)
  (declare (type graph graph) (optimize (speed 3)))
  (if (not (symbolp id))
      nil
      (loop for node in (graph-nodes graph)
	    if (find id (node-writes node) :test #'eql)
	      do (return-from id->value node))))

(defmethod id->value ((graph FastGraph) id)
  (if (symbolp id)
      (gethash id (%graph-nodes-table graph))
      nil))

(defmethod id->node ((graph Graph) id)
  (declare (type graph graph) (optimize (speed 3)))
  (find id (graph-nodes graph) :test #'eql :key #'node-id))

(defgeneric remnode (graph id) (:documentation "
```
(remnode graph id)
```
Removes all node where node-id = `id` from the given graph."))
(defmethod remnode ((graph Graph) id)
  (declare (type graph graph)
	   (type symbol id)
	   (optimize (speed 3)))
  (setf (graph-nodes graph)
	(loop for node in (graph-nodes graph)
	      unless (eql id (node-id node)) collect node)))

(defmethod remnode ((graph FastGraph) id) (remhash id (%graph-nodes-table graph)))

(defgeneric insert-nodes (graph nodes) (:documentation "
```
(insert-nodes graph nodes)
```
Inserts the given nodes (list) into the graph.
"))

(defmethod insert-nodes ((graph Graph) nodes) (nconc (graph-nodes graph) nodes))

(defmethod insert-nodes ((graph FastGraph) nodes)
  (declare (optimize (speed 3)))
  (dolist (node nodes)
    (dolist (w (node-writes node))
      (setf (gethash w (%graph-nodes-table graph)) node))))

(defun ->fast-graph (graph)
  "
```
(->fast-graph graph)
```

Creates a FastGraph object from the given graph."
  (declare (type graph graph))
  (assert (graph-outputs graph) () "Cannot create a fast graph because the graph does not have a `outputs`.")
  (let ((fast-graph (make-instance 'FastGraph :output (graph-outputs graph) :seen (graph-seen graph))))
    (insert-nodes fast-graph (graph-nodes graph))
    fast-graph))

(defgeneric ->graph (graph) (:documentation "
```
(->graph graph)
```
Converts the given graph to a fast graph.
"))

(defmethod ->graph ((graph Graph)) graph)

(defmethod ->graph ((fast-graph FastGraph))
  (declare (type FastGraph fast-graph) (optimize (speed 3)))
  (assert (graph-outputs fast-graph) () "Cannot create a graph from the given fast graph because it does not have `outputs`.")
  (let ((result) (seen nil))
    (declare (type list result seen))
    (flet ((get-parents (top-id &aux (result nil))
	     (labels
                 ((explore (id)
		    (declare (type symbol id))
		    (let ((node (id->value fast-graph id)))
		      (when (and node (null (find (node-id node) seen)))
                        (push (node-id node) seen)
			(setf result (nconc (list node) result))
                        (dolist (r (node-reads node))
                          (when (symbolp r)
                            (explore r)))))))
	       (explore top-id)
	       result)))
      (dolist (out (graph-outputs fast-graph))
	(setf result (nconc result (get-parents out)))))
    (make-instance 'Graph :output (graph-outputs fast-graph) :seen (graph-seen fast-graph) :nodes result)))

(defmethod tpsort-graph ((graph Graph))
  (declare (type Graph graph) (optimize (speed 3)))
  (let ((sorted-nodes) (queue) (in-degrees (make-hash-table)) (out-degrees (make-hash-table)))
    (declare (type list sorted-nodes queue) (type hash-table in-degrees out-degrees))
    (flet ((butseen (list)
             (loop for l in list
                   for v = (id->value graph l)
                   if (and v (symbolp l) (not (find l (the list (graph-seen graph)))))
                     collect v)))
      (loop for node in (graph-nodes graph) do
        (setf (gethash (node-id node) in-degrees) (butseen (node-reads node)))
        (dolist (r (butseen (node-reads node)))
          (when (null (find (the symbol (node-id node)) (the list (gethash (node-id r) out-degrees)) :key #'node-id))
            (push node (gethash (node-id r) out-degrees)))))
      (let* ((graph (->graph graph))
             (backward (find :PAUSE/BACKWARD (the list (graph-nodes graph)) :key #'node-type))
             (backward-pos (position :PAUSE/BACKWARD (the list (graph-nodes graph)) :key #'node-type)))
        (when backward
          ;; nodes located after :PAUSE/BACKWARD should be placed after :PAUSE/BACKWARD.
          (dolist (node (nthcdr (1+ backward-pos) (graph-nodes graph)))
            (when (null (find (the symbol (node-id node)) (the list (gethash (node-id backward) out-degrees)) :key #'node-id))
              (push node (gethash (node-id backward) out-degrees)))
            (when (null (find (the symbol (node-id backward)) (the list (gethash (node-id node) in-degrees)) :key #'node-id))
              (push backward (gethash (node-id node) in-degrees))))))
      (loop for node in (graph-nodes graph)
            if (null (gethash (node-id node) in-degrees))
              do (push node queue))
      (loop while queue
            for node = (pop queue) do
              (push node sorted-nodes)
              (dolist (adj (gethash (node-id node) out-degrees))
                (setf (gethash (node-id adj) in-degrees) (remove (node-id node) (gethash (node-id adj) in-degrees) :key #'node-id))
                (when (null (gethash (node-id adj) in-degrees))
                  (push adj queue)))
              (remhash (node-id node) out-degrees))
      (assert (= 0 (hash-table-count out-degrees)) () "tpsort-graph: the graph is not a DAG, or there's undefined variables in the graph~%use (verify-graph graph) for the detailed report.~%~a" graph)
      (nreverse sorted-nodes))))

(defmethod ->graph-with-tpsort ((fast-graph FastGraph))
  (make-instance 'Graph :output (graph-outputs fast-graph) :seen (graph-seen fast-graph) :nodes (tpsort-graph fast-graph)))

(defgeneric verify-graph (graph &key no-purge) (:documentation "
```
(verify-graph graph &key no-purge)
```
Verify the consistency of the graphs and simplify them by doing following things:
- Checks if all variables are immutable
- All variables appeared in read, are also defined in writes.
- Purge all isolated graph
- Sort nodes by time.
- Set no-purge=T to ignore purge isolated graph step. This has no effect on FastGraph.
"))

(defmethod verify-graph ((graph Graph) &key (no-purge nil))
  (declare (type graph graph)
	   (optimize (speed 3)))
  (setf (graph-nodes graph)
	(nreverse
	 (loop with seen = nil
	       for node in (reverse (graph-nodes graph))
	       if (null (find (the symbol (car (node-writes node))) seen))
		 collect (progn (push (car (node-writes node)) seen) node))))
  (resolve-isolated-nodes graph)
  ;; very heavy computation cost, should be called once if the caller is recursive.
  (unless no-purge (purge-isolated-graph graph))
  t)

(defmethod verify-graph ((graph FastGraph) &key (no-purge nil))
  (declare (ignore no-purge) (optimize (speed 3)))
  (let ((keys-initial (hash-table-keys (%graph-nodes-table graph))))
    (declare (type list keys-initial))
    (setf (%graph-nodes-table graph) (%graph-nodes-table (->fast-graph (->graph graph))))
    (let ((keys (hash-table-keys (%graph-nodes-table graph))))
      (declare (type list keys))
      (dolist (out (graph-outputs graph))
	(when (and (null (find (the symbol out) keys :test #'eq)) (find (the symbol out) keys-initial :test #'eq))
	  (error "verify-graph: Detected ~a was removed during verification process." out))))
    t))

(defun special-p (kw) (declare (optimize (speed 3))) (search "SPECIAL/" (format nil "~a" kw)))

(defmethod resolve-isolated-nodes ((graph graph))
  (declare (optimize (speed 3)))
  (let ((new-nodes) (seen (graph-seen graph)) (stashed))
    (declare (type list new-nodes seen stashed))
    (flet ((seen-p (reads) (every #'(lambda (x) (or (numberp x) (find x seen :test #'eql))) reads)))
      (loop for node in (graph-nodes graph)
	    for position fixnum upfrom 0
	    for reads = (node-reads node)
	    for writes = (node-writes node)
	    if (seen-p reads) do
	      (dolist (w writes) (push w seen))
	      (push node new-nodes)
	    else do
	      (push (cons reads node) stashed)
	    end
	    do (loop with finish-p = nil
		     with changed-p = nil
		     while (not finish-p)
		     do (setf changed-p nil)
			(loop for (reads-old . node-old) in stashed
			      if (seen-p reads-old) do
				(push node-old new-nodes)
				(setf changed-p t)
				(dolist (w (node-writes node-old)) (push w seen))
				(setf stashed (remove node-old stashed :key #'cdr :test #'equal)))
			(setf finish-p (not changed-p)))))
    (let ((initial-write-set (apply #'append (map 'list #'node-writes (graph-nodes graph))))
          (write-set (apply #'append (map 'list #'node-writes new-nodes))))
      (declare (type list write-set initial-write-set))
      (declare (type list write-set initial-write-set))
      (assert (every #'(lambda (x) (or (find (the symbol x) write-set) (null (find x initial-write-set)))) (graph-outputs graph))
              ()
              "graph-outputs ~a was removed during verification process.
To sort the graph properly, resolve the following isolated graph dependencies.
~a"
	      (graph-outputs graph)
	      (with-output-to-string (out)
		(dolist (id (graph-outputs graph))
		  (let ((seen nil))
		    (format out "~%== [Report: backtrace on ~a] ===============~%" id)
		    (labels ((find-stashed (id) (find id stashed :key #'(lambda (x) (node-writes (cdr x))) :test #'find))
			     (indent (indent) (with-output-to-string (out) (dotimes (i (the fixnum indent)) (princ " " out))))
			     (explore (id indent &key (stop nil) (parent nil))
			       (declare (type fixnum indent))
			       (let ((seen-p (find (the (or number symbol) id) seen))
				     (deps (find-stashed id)))
				 (push id seen)
				 (if deps
				     (progn
				       (format out "~a[*NG*: ~a was stashed because ~a is not defined]~%" (indent indent) id (car deps))
				       (if stop
					   (format out "~a<Omitting ~a>~%" (indent indent) id)
					   (mapc #'(lambda (x) (explore x (+ 2 indent) :stop seen-p :parent (cdr deps))) (car deps))))
				     (if (and (symbolp id) (null (find id initial-write-set)))
					 (progn
					   (format out "~a[**FIXME**: ~a is not defined in the original graph.]~%" (indent indent) id)
					   (when parent
					     (format out "~a-> In ~a~%" (indent (+ 2 indent)) parent)))
					 (format out "~a[OK: ~a satisfies all requirements]~%" (indent indent) id))))))
		      (explore id 0)))))))
    (setf (graph-nodes graph) (reverse new-nodes))
    graph))

(defmethod purge-isolated-graph ((Graph graph))
  (declare (type graph graph) (optimize (speed 3)))
  (when (graph-nodes graph)
    (let* ((output (or (graph-outputs graph) (node-writes (car (last (graph-nodes graph))))))
	   (valid-write-ids))
      (labels ((helper (x &aux (value (id->value graph x)))
		 (when (and value (null (find (node-id value) valid-write-ids)))
		   (push (node-id value) valid-write-ids)
		   (mapc #'helper (node-reads value)))))
	(mapc #'helper output))
      (setf (graph-nodes graph)
	    (loop for node in (graph-nodes graph) 
		  if (or (find (node-id node) valid-write-ids) ;; node exists in a valid path
			 (special-p (node-class node)))
		    collect node)))))

(defmethod graph-weakly-connected-p ((Graph graph) from to &aux (seen))
  "Returns T if exploring the graph from `from`, and `to` was found. (i.e.: from is wealky depends on to)"
  (labels ((explore (id &aux (node (id->value graph id)))
	     (when (and node (null (find id seen)))
	       (push id seen)
	       (when (find to (node-reads node)) (return-from graph-weakly-connected-p t))
	       (mapc #'explore (node-reads node)))))
    (explore from)
    nil))
