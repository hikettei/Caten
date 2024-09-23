(in-package :caten/ajit)
;; ~~ JIT Specific IRs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Group{Submodule} will use %for and %endfor to schedule `pre-fusion`.
(defun %for (gid size &key (scalar-p nil))
  (declare (type (or number symbol) size))
  (emit (make-node :IR :IR/FOR (list gid) (list 0 size 1) :_scalar_p (and scalar-p (eql size 1)))))
(defun %endfor (gid) (emit (make-node :IR :IR/ENDFOR nil (list gid))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Scheduled-Items
	    (:conc-name si-)
	    (:constructor make-scheduled-items (top)))
  "Top ... (list top-node, top-buffer, top-id-in-nodes)
A helper object to handle a group of nodes that scheduled to the same task."
  (nodes (list (first top)) :type list)
  (latest (second top) :type Buffer)
  (latest-id (third top) :type symbol)
  (name (intern (symbol-name (gensym "S")) "KEYWORD") :type keyword))

(defun si/append-item (scheduled-items node)
  (declare (type scheduled-items scheduled-items)
	   (type node node))
  (when (null (find (node-id node) (si-nodes scheduled-items) :test #'eql :key #'node-id))
    (push node (si-nodes scheduled-items))))

(defun buffer-intersect-p (a b)
  "Returns T if two buffers a and b are mergeable.
Further op-fusion optimization are done by the polyhedral-compiler."
  (declare (type Buffer a b))
  ;; either of buffers are scalar -> merge them
  (symbol-macrolet ((->ok (return-from buffer-intersect-p t))
		    (->ng (return-from buffer-intersect-p nil)))
    (when (or (= (buffer-nrank a) 0) (= 0 (buffer-nrank b)))->ok)
    ;; Contiguous and the same-shaped buffer -> merge them
    (when (and
           (every #'null (buffer-views a))
           (every #'null (buffer-views b))
           (equal (map 'list #'reveal-buffer (buffer-shape a)) (map 'list #'reveal-buffer (buffer-shape b))))
      ->ok)
    ;; They still have a chance to be merged by the polyhedral compiler.
    ->ng))

(defparameter *recursive-find-seen* nil)
(defun recursive-find-group (graph scheduled-items)
  "Return -> (list scheduled-items ...)"
  (declare (type graph graph) (type scheduled-items scheduled-items))
  (flet ((explore (x) (when x (recursive-find-group graph x)))
	 (mergeable-p (x latest x-type)
	   (or
	    (numberp x)
	    (and (id->value graph x) (not (eql (node-type (id->value graph x)) :Allocate)) (buffer-intersect-p latest x-type)))))
    (with-slots ((latest latest) (latest-id latest-id)) scheduled-items
      (let* ((node (or (id->value graph latest-id) (return-from recursive-find-group)))
	     ;; Allocation is done at the (Exported) VM Level
	     ;; - (1.) dynamic shapes are computed under VM Level
	     ;; - (2.) dynamic strides are computed under JIT Level
	     ;; It is unknown whether :Allocate is moved to args or body; so it should be scheduled standalone.
	     ;; i.e.: Allocate cannot be merged with any other nodes!
	     (schedule-standalone-p (eql (node-type node) :Allocate))
	     (children (node-reads node)) (children-type (relay-reads (read-type-relay node)))
	     ;; Views are purged from the graph!
	     ;; That is, the node will never connected to the nodes which compute stride/shape/view(upfrom, below, by)
	     ;; (buffer-reconstruct-view-args buffer) will reconstruct the view args, lets id->writing it to connect w/ them.
	     (loop-bound-reads (loop with type = (read-type-relay node)
				     for s in (remove-duplicates
					       (flatten
						`(,@(map 'list #'buffer-reconstruct-view-args (relay-writes type))
						  ,@(map 'list #'buffer-reconstruct-view-args (relay-reads type)))))
				     if (and (null (find s *recursive-find-seen*)) (id->value graph s)) collect s))
	     (loop-bound-types (map 'list #'(lambda (x) (car (relay-writes (read-type-relay (id->value graph x))))) loop-bound-reads))
	     (children `(,@children ,@loop-bound-reads))
	     (children-type `(,@children-type ,@loop-bound-types))
	     (mergeable-list (map 'list #'(lambda (x x-type) (mergeable-p x latest x-type)) children children-type)))
	(when (find (node-id node) *recursive-find-seen*) (return-from recursive-find-group))
	(assert (eql latest-id (car (node-writes node))) () "~a" node)
	(when loop-bound-reads
	  (let* ((already-defined (or (getattr node :_loop_bound_nodes) (getattr node :_loop_bound_nodes_type)))
		 (equal? (equal (getattr node :_loop_bound_nodes) loop-bound-reads)))
	    (assert (or (null already-defined) equal?)
		    ()
		    ""))
	  (setf (getattr node :_loop_bound_nodes) loop-bound-reads
		(getattr node :_loop_bound_nodes_type) loop-bound-types))
	(push (node-id node) *recursive-find-seen*)
	;; node_id <- F(Children[0], Children[1], ...)
	;;              mergeable[0] mergeable[1], ...
	;; All mergeable -> merge them!
	;; if the index relationships are too complicated to judge mergeablity, leave it to the polyhedron.
	(if (and (not schedule-standalone-p) (every #'identity mergeable-list))
	    (let ((parent-groups))
	      (dolist (c children)
		(when (not (numberp c))
		  (let ((node (id->value graph c)))
		    (when (and node (null (find (node-id node) *recursive-find-seen*)))
		      (si/append-item scheduled-items node)))))
	      (loop for c in children
		    for ct in children-type do
		      (when (not (numberp c))
			(setf (si-latest scheduled-items) ct
			      (si-latest-id scheduled-items) c)
			(setf parent-groups (append parent-groups (cdr (explore scheduled-items))))))
	      (append (list scheduled-items) parent-groups))
	    ;; Create new and independent schedule item for each args.
	    (let ((new-groups
		    (map
		     'list
		     #'(lambda (x x-type)
			 (when (not (numberp x))
			   (let ((node (id->value graph x)))
			     (when (and node (null (find (node-id node) *recursive-find-seen*)))
			       (make-scheduled-items (list node x-type x))))))
		     children children-type)))
	      (append
	       (list scheduled-items)
	       (loop for n in (map 'list #'explore new-groups) if n collect n))))))))

(defun buffer->loop-size (dim nrank &rest buffers)
  "Determines the bound of loops based on buffers and dim, nrank."
  (declare (type fixnum dim))
  (let* ((buffers (loop for b in buffers
			if (= (buffer-nrank b) nrank)
			  collect b))
	 (shapes
	   (map 'list #'(lambda (x &aux (v (nth dim (buffer-views x))))
			  (if (and v (fourth v))
			      1
			      (nth dim (buffer-shape x))))
		buffers)))
    (reveal-buffer
     (or
      (when (every #'numberp shapes) (apply #'max shapes))
      (when (every #'(lambda (x) (eql x 1)) shapes) 1)
      (when (some #'(lambda (x) (eql x 1)) shapes) (find 1 shapes :test-not #'eql))
      (car shapes)))))

(defun no-offsets-p (buffers dim) (every #'(lambda (x) (null (nth dim (buffer-views x)))) buffers))
(defmethod schedule->submodule ((sched Scheduled-Items) &aux (nrank 0) (args nil) (deps (schedule-depends-on sched)))
  "Lowers the scheduled-items into Loops based on VM Instruction.
i.e.:
```
ScheduledItem{
  <IR> [EXPR]:  a = sin(cos(x)) where x = (10, 10) Tensor
}
```
=>
```
Graph{Submodule}<
  %for _gid0 = 0..10
    %for _gid1 = 0..10
     <IR> [EXPR]:  a = sin(cos(x)) where x = (10, 10) Tensor
    %endfor
  %endfor
>
```
"
  (declare (type scheduled-items sched))
  (loop for node in (si-nodes sched)
	for reads = (relay-reads (read-type-relay node))
	for max-rank = (apply #'max (map 'list #'buffer-nrank reads))
	if (vm-instruction-p node) do
	  (assert (every #'(lambda (x) (or (null x) (= 0 (buffer-nrank x)) (= (buffer-nrank x) max-rank))) reads)
		  ()
		  "Inconsistency in the inferred tensor shape. (This is a bug of Caten, not users as long as ShapeTracker is enabled.)
All tensors appeared in `node-reads`, must have the same ranks or be scalars.~%
Node: ~a
Butgot: ~a
Buffers: ~a
"
		  node
		  (map 'list #'buffer-shape reads)
		  reads)
	  (setf nrank (max nrank (apply #'max (map 'list #'(lambda (x) (if x (buffer-nrank x) 0)) reads))))
	  (mapc #'(lambda (r type) (when (find r deps) (push type args))) (node-reads node) reads))
  (let* ((index-components (map 'list #'gid (range 0 nrank)))
	 (loopsizes (map 'list #'(lambda (x) (apply #'buffer->loop-size x nrank args)) (range 0 nrank))))
    (let ((g
	    (with-context
	      (start-loop (loop for i in index-components for s in loopsizes
				for dim upfrom 0 do (%for i s :scalar-p (no-offsets-p args dim))))
	      (_ (dolist (node (si-nodes sched)) (emit node)))
	      (end-loop (dolist (i index-components) (%endfor i))))))
      (setf (graph-seen g) (schedule-depends-on sched))
      g)))

(defmethod schedule-depends-on ((sched Scheduled-Items))
  "Enumerates the unsolved buffer ids from the sched graph."
  (declare (type scheduled-items sched))
  (nodes-depends-on (si-nodes sched)))

(defun graph->loop-factors (graph &key (scalar-mutation nil))
  "Graph = Graph{Submodule}"
  (declare (type graph graph))
  ;; Applies if only rank is one.
  (let ((scalar-mutation (when scalar-mutation (= 1 (length (graph->loop-factors graph :scalar-mutation nil))))))
    (remove-duplicates
     (loop for node in (graph-nodes graph)
	   if (and (eql (node-type node) :IR/FOR) (or (null scalar-mutation) (null (getattr node :_scalar_p))))
	     collect (car (node-writes node))))))

(defun graph-loop-scalar-mutated-p (graph)
  (not
   (= (length (graph->loop-factors graph :scalar-mutation nil))
      (length (graph->loop-factors graph :scalar-mutation t)))))

(defun graph->loop-size (graph)
  "Graph = Graph{Submodule}"
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (and (eql (node-type node) :IR/FOR)
		 (symbolp (second (node-reads node))))
	   collect (second (node-reads node)))))
