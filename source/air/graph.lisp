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
FastGraph is a subclass of Graph that implements faster node searches based on the assumption that the nodes form a DAG. (It is approximately 20 times faster than Graph, in the larger scale.)
Since FastGraph stores nodes as a hash-table, there are some constraints on node operations. If necessary, converting between ->fast-graph and ->graph can be done frequently with minimal impact on performance."))

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

(defun make-graph (&rest nodes) (make-instance 'Graph :nodes nodes))
(defmethod copy-graph ((graph Graph))
  (let ((g (apply #'make-graph (graph-nodes graph))))
    (setf (graph-seen graph) (copy-list (graph-seen graph))
	  (graph-outputs graph) (copy-list (graph-outputs graph)))
    g))
(defun graph-p (graph) (typep graph 'Graph))
(defmethod graph-nodes ((graph Graph)) (%graph-nodes graph))
(defmethod graph-nodes ((graph FastGraph)) (hash-table-values (%graph-nodes-table graph)))
(defmethod (setf graph-nodes) (nodes (graph Graph)) (setf (%graph-nodes graph) nodes))
(defmethod (setf graph-nodes) (nodes (graph FastGraph)) (error "graph-nodes for FastGraph is immutable!"))

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

(defmethod remnode ((graph Graph) id)
  (declare (type graph graph)
	   (type symbol id)
	   (optimize (speed 3)))
  (setf (graph-nodes graph)
	(loop for node in (graph-nodes graph)
	      unless (eql id (node-id node)) collect node)))

(defmethod remnode ((graph FastGraph) id) (remhash id (%graph-nodes-table graph)))
(defmethod insert-nodes ((graph Graph) nodes) (nconc (graph-nodes graph) nodes))
(defmethod insert-nodes ((graph FastGraph) nodes)
  (declare (optimize (speed 3)))
  (dolist (node nodes)
    (dolist (w (node-writes node))
      (setf (gethash w (%graph-nodes-table graph)) node))))

(defun ->fast-graph (graph)
  (declare (type graph graph))
  (when (= 1 (ctx:getenv :SAFETY)) (return-from ->fast-graph graph))
  (assert (graph-outputs graph) () "Cannot create a fast graph because the graph does not have a `outputs`.")
  (let ((fast-graph (make-instance 'FastGraph :output (graph-outputs graph) :seen (graph-seen graph))))
    (insert-nodes fast-graph (graph-nodes graph))
    fast-graph))

(defmethod ->graph ((graph Graph)) graph)

(defmethod ->graph ((fast-graph FastGraph))
  (declare (type FastGraph fast-graph) (optimize (speed 3)))
  (assert (graph-outputs fast-graph) () "Cannot create a graph from the given fast graph because it does not provide a `outputs`.")
  (let ((result) (seen nil))
    (declare (type list result seen))
    (flet ((get-parents (top-id &aux (result nil))
	     (labels
		 ((explore (id)
		    (declare (type symbol id))
		    (when (null (find id seen))
		      (push id seen)
		      (let ((node (id->value fast-graph id)))
			(when node
			  (setf result (nconc (list node) result))
			  (dolist (r (node-reads node))
			    (when (symbolp r)
			      (explore r))))))))
	       (explore top-id)
	       result)))
      (dolist (out (graph-outputs fast-graph))
	(setf result (nconc result (get-parents out)))))
    (make-instance 'Graph :output (graph-outputs fast-graph) :seen (graph-seen fast-graph) :nodes result)))

(defmethod verify-graph ((graph Graph) &key (no-purge nil))
  "Verify the consistency of the graphs and simplify them by operating following:
- Checks if all variables are immutable
- All read dependencies are appearedin writes.
- Purge all isolated graph
- Sort by the time
- TODO: verify-graph is called multiple times during compilation, needs optimized more.
- Nodes whose class are start with special/ cannot be purged even if they are isolated."
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
		    (labels ((find-stashed (id) (find (the symbol id) stashed :key #'(lambda (x) (node-writes (cdr x))) :test #'find))
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
