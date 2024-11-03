(in-package :caten/ajit)

;; ~~ From AVM Into Polyhedral Model Compilation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; polyhedral compilation to determine the parallelization strategy
;; If we do; compile from avm into ISL, optimizng
;; This is the toplevel of all optimization stuff
;; Set no-writes=t to skip O(N) search algorithm.
(defstruct (Group
	    (:constructor make-group (nodes realize-on-vm &key (no-writes nil)
				      &aux
					(args (when (null no-writes) (nodes-depends-on nodes)))
					(shapes (when (null no-writes) (nodes-gather-args nodes))))))
  (graph (apply #'make-graph nodes) :type graph)
  (sched nil :type list)
  (realize-on-vm realize-on-vm :type boolean)
  (polyhedron nil :type (or null Polyhedral))
  (render-graph nil :type (or null Graph))
  (across-time-deps nil :type list)
  (args args :type list)
  (shapes shapes :type list)
  (writes (when (null no-writes) (nodes-output-ids nodes)) :type list)
  (id (gensym)))

(defmethod max-dimension-in-group ((group group))
  (apply
   #'max
   (or
    (loop for node in (graph-nodes (group-graph group))
	  append
	  (loop for var in `(,@(relay-reads (read-type-relay node)) ,@(relay-writes (read-type-relay node)))
		if var
		  collect
		  (buffer-nrank var)))
    (list 0))))

(defun relocate-independent-allocations! (graph)
  "
  X   A+B
  |    |
  Y  Alloc <- If A+B is an independant operatin, Alloc and its subgraph
   \  /       can be relocated into the top of graph.
     Z"
  (declare (type graph graph))
  (let ((alloc-candidates
	  (loop for node in (graph-nodes graph)
		if (eql (node-type node) :Allocate)
		  collect node)))
    (labels ((subgraph (alloc)
	       (loop for r in (node-reads alloc)
		     if (symbolp r)
		       append (get-subgraph r graph)))
	     (alloc-p (node alloc subgraph)
	       (or (find (node-id node) subgraph :key #'node-id)
		   (and
		    (eql (node-type node) :Allocate)
		    (eql (node-id node) (node-id alloc)))))
	     (relocate (alloc subgraph)
	       (setf (graph-nodes graph)
		     (append
		      subgraph
		      (list alloc)
		      (loop for node in (graph-nodes graph)
			    unless (alloc-p node alloc subgraph)
			      collect node))))
	     (isolated-p (alloc subgraph)
	       (when subgraph
		 (loop with writes = (apply #'append (map 'list #'node-writes subgraph))
		       for node in (graph-nodes graph)
		       unless (alloc-p node alloc subgraph)
			 do (when (intersection (node-reads node) writes) (return-from isolated-p nil))))
	       t))
      (loop for alloc in alloc-candidates
	    for subgraph = (subgraph alloc)
	    if (isolated-p alloc subgraph)
	      do (relocate alloc subgraph)))))

(defun relocate-independent-loop-bound-computation! (graph)
  "Applies the same relocation as relocate-independent-allocation! against views, simplifying the scheduling for dynamic-shaped kernels."
  (declare (type graph graph))
  (let ((view-candidates
	  (loop for node in (graph-nodes graph)
		if (eql (node-type node) :View)
		  collect node)))
    (labels ((subgraph (view)
	       (loop for r in (node-reads view)
		     if (symbolp r)
		       append (get-subgraph r graph)))
	     (view-p (node view subgraph)
	       (or (find (node-id node) subgraph :key #'node-id)
		   (and
		    (eql (node-type node) :View)
		    (eql (node-id node) (node-id view)))))
	     (relocate (view subgraph)
	       (setf (graph-nodes graph)
		     (append
		      subgraph
		      (list view)
		      (loop for node in (graph-nodes graph)
			    unless (view-p node view subgraph)
			      collect node))))
	     (isolated-p (view subgraph)
	       (when subgraph
		 (loop with writes = (apply #'append (map 'list #'node-writes subgraph))
		       for node in (graph-nodes graph)
		       unless (view-p node view subgraph)
			 do (when (intersection (node-reads node) writes) (return-from isolated-p nil))))
	       t))
      (loop for view in view-candidates
	    for subgraph = (subgraph view)
	    if (isolated-p view subgraph)
	      do (relocate view subgraph)))))

(defun recursive-split-into-subgroups (group)
  (declare (type group group))
  (let ((graph (group-graph group))
	(stashed-path)
	(seen))
    (labels ((finalize-group (group)
	       ;; Infers group-writes
	       (make-group (graph-nodes (group-graph group)) (group-realize-on-vm group)))
	     (force-realize-on-vm (node)
	       (or
		(eql (node-type node) :pause/backward)
		(eql (node-type node) :Allocate)
		(and (eql (node-type node) :LOAD)
		     (symbolp (getattr node :value))
		     (= 0 (buffer-nrank (car (relay-writes (read-type-relay node))))))))
	     (explore (id)
	       (declare (type symbol id))
	       (let ((node (id->value graph id)))
		 (when (and node (null (find (node-id node) seen :key #'node-id)))
		   ;; dynamic shapes are stashed and excluded from the graph, or exists in the toplevel?
		   (push node seen)
		   (if (force-realize-on-vm node)
		       (progn
			 (push node stashed-path)
			 nil)
		       (make-group
			(append
			 (loop for read in (node-reads node)
			       for parent = (when (symbolp read) (explore read))
			       if parent
				 append (graph-nodes (group-graph parent)))
			 (list node))
			nil
			:no-writes t)))))
	     (restart-from-stashed-node (node)
	       (list
		(make-group
		 (list node)
		 t
		 :no-writes t)
		(make-group
		 (loop for read in (node-reads node)
		       for parent = (when (symbolp read) (explore read))
		       if parent
			 append (graph-nodes (group-graph parent)))
		 nil
		 :no-writes t))))
      (let ((new-groups (map 'list #'explore (group-writes group))))
	;; TODO: Remove duplicated LOAD! they are in stashed-path
	(loop while stashed-path
	      do (setf new-groups (append (restart-from-stashed-node (pop stashed-path)) new-groups)))
	(loop for g in new-groups
	      ;; Empty group can be removed
	      if (and g (graph-nodes (group-graph g))) collect (finalize-group g))))))

(defun split-into-subgroups (graph)
  "Graphs are first breaked into subgroups only after:
- Tensor is shaped by a tensor
- :PAUSE/BACKWARD
Input: graph (AVM Graph)"
  (declare (type graph graph))
  (let ((groups))
    (labels ((force-realize-on-vm (node) (or (eql (node-type node) :pause/backward))))
      (apply
       #'append
       (map
	'list
	#'recursive-split-into-subgroups
	`(,@(loop for node in (graph-nodes graph)
		  if (force-realize-on-vm node)
		    collect (make-group (nreverse groups) nil)
		    and collect (make-group (list node) t)
		    and do (setf groups nil)
		  else
		    do (push node groups))
	  ,(make-group (nreverse groups) nil)))))))
