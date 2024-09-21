(in-package :caten/ajit)
;; First, Nodes determined to be necessarily Fused (e.g.: non-viewed and same shaped tensors)
;; are combined into a single SubGraph and converted into an ISL AST.
;; Refenreces: (Good to read first)
;; - https://pliss2019.github.io/albert_cohen_slides.pdf
;; - https://www.slideshare.net/slideshow/introduction-to-polyhedral-compilation/70482946
;; - https://www.researchgate.net/publication/273651704_Schedule_Trees
;; - https://www.researchgate.net/publication/317826152_Scheduling_for_PPCG
;; - https://groups-google-com.translate.goog/g/isl-development/c/2bgepkLQBhY/m/BmiDq1nDAAAJ?_x_tr_sl=en&_x_tr_tl=ja&_x_tr_hl=ja&_x_tr_pto=sc
;; - https://libisl.sourceforge.io/tutorial.pdf
;; - https://libisl.sourceforge.io/manual.pdf
;; - https://medium.com/@zhen8838/hands-on-polyherdal-affine-loop-fusion-ffb398b0ae60
;; - https://github.com/zhen8838/isl_learn/blob/main/12_schedule_program.ipynb
;; ~~~~ Subgraph initializers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Scheduled-Items
	    (:conc-name si-)
	    (:constructor make-scheduled-items (top)))
  "Top ... (top-node, top-buffer, top-id-in-nodes)"
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
;; ~~ JIT-Specific IRs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %for (gid size &key (scalar-p nil))
  (declare (type (or number symbol) size))
  (emit (make-node :IR :IR/FOR (list gid) (list 0 size 1) :_scalar_p (and scalar-p (eql size 1)))))
(defun %endfor (gid) (emit (make-node :IR :IR/ENDFOR nil (list gid))))
;; ~~~~~ subgraph helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun buffer->loop-size (dim nrank &rest buffers)
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
;; TODO: Flatten the loop if the access pattern is contiguous
(defun schedule->submodule (sched &aux (nrank 0) (args nil) (deps (schedule-depends-on sched)))
  "Lowers the grouped scheduled-items into the graph."
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

(defun schedule-depends-on (sched)
  "Enumerates the unsolved buffer ids from the sched graph."
  (declare (type scheduled-items sched))
  (nodes-depends-on (si-nodes sched)))

(defun graph->loop-factors (graph &key (scalar-mutation nil))
  (declare (type graph graph))
  ;; Applies if only rank is one.
  (let ((scalar-mutation (when scalar-mutation (= 1 (length (graph->loop-factors graph :scalar-mutation nil))))))
    (remove-duplicates
     (loop for node in (graph-nodes graph)
	   if (and (eql (node-type node) :IR/FOR) (or (null scalar-mutation) (null (getattr node :_scalar_p))))
	     collect (car (node-writes node))))))

(defun graph->loop-factors1 (graph)
  (declare (type graph graph))
  ;; Applies if only rank is one.
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (and (eql (node-type node) :IR/FOR) (null (getattr node :_scalar_p)))
	   collect (car (node-writes node))
	 else if (and (eql (node-type node) :IR/FOR) (getattr node :_scalar_p))
		collect (car (node-writes node)))
   :test #'(lambda (x y) (and (symbolp x) (symbolp y) (eql x y)))))

;; [Fix] is it necessary?
(defun graph-loop-scalar-mutated-p (graph)
  (not
   (= (length (graph->loop-factors graph :scalar-mutation nil))
      (length (graph->loop-factors graph :scalar-mutation t)))))

(defun graph->loop-size (graph)
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (and (eql (node-type node) :IR/FOR)
		 (symbolp (second (node-reads node))))
	   collect (second (node-reads node)))))
;; ~~ ISL Renderers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun vm-instruction-p (node)
  "Add more classes here if you have a certain node that do not desired to be involved."
  ;; :IR = :FOR :ENDFOR
  (and
   (not (eql (node-class node) :IR))
   (not (eql (node-type node) :Allocate))))
;; TODO: generate Expr
(defun one-dimensional-renderer (gid stride upfrom by broadcast-p)
  (if broadcast-p
      (format nil "0")
      (format nil "~a~a~a"
	      (if (eql by 1)
		  (if (and (numberp stride) (= stride 1))
		      ""
		      (format nil "~a*" stride))
		  (if (and (numberp stride) (= stride 1))
		      (format nil "~a*" by)
		      (if (and (numberp stride) (numberp by))
			  (format nil "~a*" (* stride by))
			  (format nil "~a*~a*" by stride))))
	      gid
	      (if (eql upfrom 0)
		  ""
		  (format nil "+(~a*~a)" upfrom stride)))))

(defun isl-access-renderer (gid stride upfrom by broadcast-p)
  (declare (ignore stride))
  (assert (numberp by) () "by is expected to be a constant to create an affine schedule! (TODO: Fix)")
  ;;(when (symbolp by) (setf by 2))
  (if broadcast-p
      "0"
      (format nil "~a~a~a"
	      (if (eql by 1)
		  ""
		  (format nil "~a*" by))
	      gid
	      (if (eql upfrom 0)
		  ""
		  (format nil "+~a" upfrom)))))

(defun render-isl-aref (buffer &key (genid #'gid) (indexing #'one-dimensional-renderer) (split "+") (strides nil) (use-permute nil) (upper nil) (mutate-scalar nil) &aux (c 0))
  "Renders the stride computation for ISL:
```
A[stride1 * view_info1 * index_component_0 + bias1 + stride2 * view_info2 * index_component_1 + bias2 + ...]
```
"
  (declare (type buffer buffer))
  (apply
   #'concatenate
   'string
   (butlast
    (append
     (loop with order = (if (and use-permute (buffer-inferred-permute buffer))
			    (buffer-inferred-permute buffer)
			    (range 0 (buffer-nrank buffer)))
	   for nth in order
	   for stride-nth in (or strides (buffer-stride buffer))
	   for size   = (nth nth (buffer-shape buffer))
	   for view   = (nth nth (buffer-views buffer))
	   for stride = (reveal-buffer stride-nth)
	   for upfrom = (reveal-buffer (or (nth 0 view) 0))
	   for by     = (reveal-buffer (or (nth 2 view) 1))
	   for broadcast-p = (nth 3 view)
	   for gid = (funcall genid nth)
	   do (incf c)
	   if (and mutate-scalar (eql size 1))
	     append (list "0" split)
	   else
	     append
	     (list
	      (funcall indexing gid stride upfrom by broadcast-p)
	      split))
     (when upper (loop repeat (- upper c) append (list "0" split)))))))

(defun render-domain (pipeline &key (depends-on nil))
  "Render the domain notation from the scheduled subgraphs
```
Domain [depends-on] -> {
  Sched_0_ID(loop_factors_0) : IConstraint_0;
  Sched_1_ID(loop_factors_1) : IConstraint_1;
                 ...
}
```
Pipeline: A hash-table where keys and values are: {T_ID[Fixnum] -> Scheduled_Subgrpah[Graph]}"
  (declare (type list depends-on)
	   (type hash-table pipeline))
  (with-output-to-string (out)
    ;; renders depends-on
    (format out "[~(~a~)] -> {~%" (render-list depends-on))
    (maphash1
     #'(lambda (timestamp subgraph)
	 (let* ((loop-factors (graph->loop-factors subgraph :scalar-mutation t))
		(mutated-p (graph-loop-scalar-mutated-p subgraph))
		(constraints
		  (loop for node in (graph-nodes subgraph)
			if (and (eql (node-type node) :IR/FOR) (or (null mutated-p) (null (getattr node :_scalar_p))))
			  collect
			  (progn
			    (assert (= 1 (nth 2 (node-reads node))) () "Loop steps should be optimized by the polyhedral compiler. Set=1.")
			    (make-iconstraint (car (node-writes node)) (nth 0 (node-reads node)) (nth 1 (node-reads node)))))))
	   (if loop-factors
	       (progn
		 (format out "  T~a[~(~a~)]" timestamp
			 (render-list
			  (loop for lf in loop-factors
				for c in constraints
				for scal-p = (iconstraint-scalar-p c)
				if scal-p
				  collect (format nil "~a = 0" lf)
				else
				  collect lf)))
		 (format out " : ")
		 (let ((c (apply #'concatenate 'string (butlast (loop for c in constraints unless (iconstraint-scalar-p c) append (list (form c) " and "))))))
		   (format out "~a" (if (string= c "") "true" c)))
		 (format out ";~%"))
	       (format out "  T~a[];~%" timestamp))))
     pipeline)
    (format out "}")))

(defun render-access (mode pipeline &key (depends-on nil) &aux (kernel-rank (pipeline/upper-nrank pipeline)))
  "Render the read/write accessing relation ship in the following notation:
```
[depends-on] -> {
    Sched_0_ID[read_index] -> Tensor_ID_N[strided_access_idx];
        ...
}
```
"
  (declare (type list depends-on)
	   (type (member :read :write) mode)
	   (type hash-table pipeline))
  (with-output-to-string (out)
    (format out "[~(~a~)] -> {~%" (render-list depends-on))
    (maphash1
     #'(lambda (timestamp subgraph)
	 (let* ((lf (graph->loop-factors subgraph :scalar-mutation t))
		(lf-orig (graph->loop-factors subgraph))
		(occur-from
		  (format nil "T~a[~(~a~)]" ;; = 0
			  timestamp (render-list lf)))
		(scalar (apply #'concatenate 'string (butlast (loop repeat kernel-rank append (list "0" ", "))))))
	   (flet ((pad ()
		    (if (= kernel-rank (length lf-orig))
			""
			(format nil ", ~a"
				(apply #'concatenate 'string
				       (butlast
					(loop repeat (- kernel-rank (length lf-orig)) append (list "0" ", "))))))))
	     (dolist (node (graph-nodes subgraph))
	       (when (not (eql (node-class node) :IR))
		 ;; When reduction is T, the first argument becomes the dependency
		 ;; e.g.: Tn[...]: A <- ADD(X, Y, reduction=t) is the equivalent to
		 ;; Tn[...]: A = (X += Y),  i.e.: Tn[...]: A = (X = X + Y)
		 ;; Here, X depends on X.
		 (when (and (eql mode :read) (getattr node :reduction))
		   (let ((reduce-to (car (node-writes node)))
			 (rt        (car (relay-writes (read-type-relay node)))))
		     (when (symbolp reduce-to)
		       (if (vm-instruction-p node)
			   (format out "  ~a -> ~(~a~)[~(~a~)~a];~%" occur-from reduce-to (render-isl-aref rt :mutate-scalar t :indexing #'isl-access-renderer :split ", " :use-permute t) (pad))
			   (error ":reduction for the op ~a is invaild." node)))))
		 (loop for r in (funcall (if (eql mode :read) #'node-reads #'node-writes) node)
		       for rt in (funcall (if (eql mode :read) #'relay-reads #'relay-writes) (read-type-relay node)) do
			 ;; When node has a :reduction
			 (when (symbolp r)
			   (if (null lf)
			       (format out "  ~a -> ~(~a~)[~a];~%" occur-from r scalar)
			       (when (vm-instruction-p node)
				 (let ((access (render-isl-aref rt :mutate-scalar t :indexing #'isl-access-renderer :split ", " :use-permute t)))
				   (if (string= access "")
				       (format out "  ~a -> ~(~a~)[~a];~%" occur-from r scalar)
				       (format out "  ~a -> ~(~a~)[~(~a~)~a];~%" occur-from r access (pad))))))))
		 ;; Symbols for computing the stride
		 (when (and node (eql mode :read))
		   (let* ((symbols
			    (loop for buff in `(,@(relay-reads (read-type-relay node)) ,@(relay-writes (read-type-relay node)))
				  if buff
				    append (append (buffer-shape buff) (buffer-stride buff) (apply #'append (buffer-views buff)))))
			  (symbols
			    (loop for s1 in symbols
				  for s = (reveal-buffer s1)
				  if (and (symbolp s) (not (eql s t)) (not (eql s nil)))
				    collect s)))
		     (dolist (s symbols) (format out "  ~a -> ~(~a~)[~a];~%" occur-from s scalar)))))))))
     pipeline)
    (format out "}")))

(defun pipeline->timestamp (pipeline)
  (declare (type hash-table pipeline))
  (maphash
   #'(lambda (ts graph)
       (declare (ignore ts))
       (setf (graph-outputs graph) (nodes-output-ids (graph-nodes graph))))
   pipeline)
  (let ((graph
	  (apply
	   #'make-graph
	   (loop for time in (hash-table-keys pipeline)
		 for graph = (gethash time pipeline)
		 collect (make-node :TIME :GRAPH (graph-outputs graph) (graph-seen graph) :id time))))
	(lex (make-hash-table))
	(seen))
    ;; TODO: Relocate Isolated nodes w/ the end of nodes. (when debugging, should produce a warning)
    (labels ((explore (id &key (time 0) &aux (val (id->value graph id)))
	       (when (and val (null (find `(,time ,(node-id val)) seen :test #'equal)))
		 (push (list time (node-id val)) seen)
		 (let* ((key (getattr val :id)))
		   (mapc #'(lambda (x) (explore x :time (1+ time))) (remove-duplicates (node-reads val)))
		   (if (gethash key lex)
		       (push time (gethash key lex))
		       (setf (gethash key lex) (list time)))))))
      ;; Labelling the schedule dependency w/ lexicographical order
      ;; [TODO] that should look like below, not starting with `time`?
      ;; wanna consider this when optimizing backward process; it usually has multiple outputs.
      ;; 2  2    2    ...
      ;; \  /   /      |
      ;;   1   1       4
      ;;    \ /        |
      ;;     0         3
      ;; 
      (loop for time upfrom 0
	    for id in (nodes-output-ids (graph-nodes graph))
	    do (explore id :time time))
      (assert (every #'(lambda (x) (find x seen :key #'second)) (map 'list #'node-id (graph-nodes graph))))
      (let ((tree-max-depth (apply #'max (apply #'append (hash-table-values lex)))))
	(maphash
	 #'(lambda (x y)
	     (setf (gethash x lex) (apply #'min (map 'list #'(lambda (n) (- tree-max-depth n)) y))))
	 lex)
	lex))))

(defun isl-initial-schedule (pipeline &key (depends-on nil))
  "
Optional order fusing softmax in a single kernel is:
[val_1, val_0, val_7, val_11] -> {
  T0[_gid0, _gid1] -> [3, _gid0, _gid1];
  T1[_gid0, _gid1] -> [1, _gid0, _gid1];
  T2[_gid0, _gid1] -> [2, _gid0, _gid1];
  T3[_gid0, _gid1] -> [3, _gid0, _gid1];
  T4[_gid0, _gid1] -> [4, _gid0, _gid1];
  T5[_gid0, _gid1] -> [4, _gid0, _gid1];
  T6[_gid0, _gid1] -> [5, _gid0, _gid1];
  T7[_gid0, _gid1] -> [6, _gid0, _gid1];
}
"
  (let ((lex (pipeline->timestamp pipeline))
	(prev-rank 0))
    (values
     (union-map-from-str
      (with-output-to-string (out)
	(format out "[~(~a~)] -> " (render-list depends-on))
	(format out "{~%")
	(maphash1 ;; ts comes in order of 0, 1, 2, ..., max regardless of Common Lisp Implementation.
	 #'(lambda (ts graph)
	     (let* ((loop-factors (graph->loop-factors1 graph))
		    (dom (format nil
				 "  T~a[~(~a~)] -> [~(~a~)]"
				 ts
				 (render-list loop-factors)
				 (render-list (padding-list `(,(gethash ts lex) ,@loop-factors) (1+ prev-rank))))))
	       (setf prev-rank (length loop-factors))
	       (format out "~a;~%" dom)))
	 pipeline)
	(format out "}")))
     lex)))

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
     Z
"
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
- :PAUSE/BACKWARD"
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

(declaim (ftype (function (AVM &key (:verbose boolean)) (values list)) create-schedules-from-avm))
(defun create-schedules-from-avm (avm &key (verbose nil))
  "Step1, Creates an initial schedule"
  (declare (type avm avm) (type boolean verbose))
  ;; Trace the view and dtype information.
  (let* ((type-map (run-type-infer avm)) (*recursive-find-seen* nil) (seen nil))
    (when verbose
      (format t "Verbose: Initial Computation Graph[Forward/Backward]~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~~ JIT Specific Graph rewriting Processes ~~~~~~~~~~~~~~~~~~~~
    (deploy-type-infer-results avm type-map) ;; Move buffer/view nodes into :_type_relay attribtutes
    (relocate-independent-loop-bound-computation! (avm-graph avm)) ;; for (...;by+=a*b) is equivalent to for(...;by+=val_xx)
    (apply-jit-specific-simplifiers avm)     ;; Purge :view nodes, WMMA Accumlation, contiguous elimination etc...
    (when verbose
      (format t "Verbose: Simplified Graph[Forward/Backward]~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ;; (The comment below is out-of-date. we will create more than 2 groups)
    ;; Creating a Polyhedral Compilation Group: (Group1 = Forward, Group2=Backward)
    ;; Assume there's only two groups for simplicity:
    ;;  - Forward Computation
    ;;  - Backward Computation
    ;; If we are in the mood of implementing second-order derivatives, create another group here.
    ;; So, in the early stage, we will create a list of save-for-backward, and apply multi-grouping optimization.
    ;; We also assumed that all custom kernels are scheduled as a scalar function having vector array on ISL,
    ;; there is no need to consider the situation that where a complete array used in forward, is required by another kernel
    ;; except for backward. (that's why we only create a list of save-for-backward)
    (labels ((id->buffer (graph)
	       #'(lambda (id)
		   (assert (symbolp id) () "Graph should not return a number!")
		   (let ((node (id->value graph id)))
		     (list node (car (relay-writes (read-type-relay node))) id))))
	     (make-top-schedule (group) (map 'list (compose #'make-scheduled-items (id->buffer (group-graph group))) (group-writes group)))
	     (schedule (group schedules)
	       (multiple-value-bind (sorted seen-new)
		   (schedule/resolve-isolated-ops
		    (reverse (flatten (map 'list #'(lambda (x) (recursive-find-group (group-graph group) x)) schedules)))
		    seen)
		 (setf seen (append seen-new (group-writes group)))
		 sorted))
	     (seen-in-groups (group &aux (seen-in-groups nil))
	       (if (group-sched group)
		   (loop for nth upfrom 0
			 for s in (group-sched group)
			 do (dolist (node (si-nodes s))
			      (unless (eql (node-type node) :Allocate)
				(setf seen-in-groups (append seen-in-groups (node-writes node)))))
			    (setf (si-name s) (intern (format nil "T~a" nth) "KEYWORD")))
		   (loop for node in (graph-nodes (group-graph group))
			 unless (eql (node-type node) :Allocate)
			   do (setf seen-in-groups (append seen-in-groups (node-writes node)))))
	       (remove-duplicates seen-in-groups))
	     (read-in-groups (group &aux (read-in-groups nil))
	       (if (group-sched group)
		   (loop for s in (group-sched group) do
		     (dolist (node (si-nodes s))
		       (dolist (r `(,@(node-reads node) ,@(getattr node :_loop_bound_nodes))) (when (symbolp r) (push r read-in-groups)))))
		   (loop for node in (graph-nodes (group-graph group)) do
		     (dolist (r `(,@(node-reads node) ,@(getattr node :_loop_bound_nodes))) (when (symbolp r) (push r read-in-groups)))))
	       (remove-duplicates read-in-groups)))
      (relocate-independent-allocations! (avm-graph avm))
      (let* ((groups (loop for g in (group/resolve-dependencies (split-into-subgroups (avm-graph avm)))
			   if (graph-nodes (group-graph g))
			     collect g)))
	(loop for group in groups
	      if (group-realize-on-vm group)
		do (setf seen (append seen (group-writes group)))
	      else
		collect (setf (group-sched group) (schedule group (make-top-schedule group))))
	(loop with write-deps = (map 'list #'seen-in-groups groups)
	      with read-deps = (map 'list #'read-in-groups groups)
	      for nth upfrom 1
	      for group in groups
	      for writing in write-deps
	      collect (setf (group-across-time-deps group)
			    (intersection writing `(,@(avm-fw-outputs avm)
						    ,@(avm-bw-outputs avm)
						    ,@(apply #'append (nthcdr nth read-deps))))))
	(mapc
	 #'(lambda (x)
	     (unless (group-realize-on-vm x)
	       (apply-multiexpr-grouping (group-sched x) (group-across-time-deps x))))
	 groups)

	(when verbose
	  (loop for group in groups
		for nth upfrom 0 do
		  (format t "~%= Verbose: ~ath group === ~%" nth)
		  (if (group-realize-on-vm group)
		      (print group)
		      (print-schedules (group-sched group)))))
	groups))))

(declaim (ftype (function (AVM group &key (:verbose boolean) (:verbose-auto boolean)) (values Polyhedral)) create-polyhedron-from-group))
(defun create-polyhedron-from-group (avm group &key (verbose nil) (verbose-auto nil))
  "Step2, create a polyhedron from the scheduled items."
  (declare (type group group) (type boolean verbose))
  (let* ((submodule (map 'list #'schedule->submodule (group-sched group))) ;; Rendering :FOR and :ENDFOR
	 (pipeline (make-hash-table)))
    (loop for nth upfrom 0
	  for s in submodule
	  do (setf (gethash nth pipeline) s))
    (when verbose
      (format t "== [Final Graph Before Applying Polyhedral Compiler] ======~%")
      (print-pipeline pipeline))
    (let* ((vm-inputs (avm-gather-args avm))
	   ;;(vm-input-tensors (nodes-depends-on (graph-nodes (group-graph group))))
	   (loop-sizes (loop for value being the hash-values of pipeline
			     collect (graph->loop-size value)))
	   (loop-size (apply #'append loop-sizes))
	   (dynamic-shapes (remove-duplicates `(,@vm-inputs ,@loop-size)))
	   (domain         (render-domain pipeline :depends-on dynamic-shapes))
	   ;;(dynamic-shapes (remove-duplicates `(,@dynamic-shapes ,@vm-input-tensors)))
	   (read-access  (render-access :read pipeline :depends-on dynamic-shapes))
	   (write-access (render-access :write pipeline :depends-on dynamic-shapes)))
      (multiple-value-bind (schedule lex-table) (isl-initial-schedule pipeline :depends-on dynamic-shapes)
	(when verbose-auto
	  (format t "== [Domain] ===========")
	  (format t "~%~a~%" domain)
	  (format t "== [Read Accesses] =======")
	  (format t "~%~a~%" read-access)
	  (format t "== [Write Accesses] ======")
	  (format t "~%~a~%" write-access)
	  (format t "== [Initial Scheduling domain (=domain)] ======")
	  (format t "~%~a~%" schedule))
	(make-polyhedral avm pipeline domain read-access write-access schedule vm-inputs (group-writes group) lex-table)))))

(declaim (ftype (function (Polyhedral &key (:verbose boolean) (:serialize boolean)) Polyhedral) auto-schedule!))
(defun auto-schedule! (polyhedral &key (verbose nil) (serialize nil))
  "
Step3, autoschedule polyhedron model.
Options:
- debug[boolean]:  If this option is set, this function prints the Polyhedron Model for each step of the optimization.
- serialize[boolean]: If this option is set, then all strongly connected components in the dependence
  graph are serialized as soon as they are detected. This means in particular that
  instances of statements will only appear in the same band node if these statements belong to
  the same strongly connected component at the point where the band node is constructed."
  (declare (type Polyhedral polyhedral)
	   (type boolean verbose serialize))
  (macrolet ((debug-print (step-name) `(when verbose (format t "~%[~a]~%~a~%" ,step-name (print-polyhedral polyhedral nil)))))
    (debug-print "Initial")
    ;; Loop Fusion
    (poly/schedule polyhedral :serialize serialize)
    (debug-print "Scheduled")
    polyhedral))

(declaim (ftype (function (Group keyword) graph) finalize-and-retrive-graph))
(defun finalize-and-retrive-render-graph (group backend)
  "Step4, Extract the schedule from ISL."
  (declare (type group group))
  (multiple-value-bind (ast bands) (finalize-schedule (group-polyhedron group))
    (create-rendering-graph ast bands backend (max-dimension-in-group group))))

(defstruct (Compiled-Kernel
	    (:conc-name ck-)
	    (:constructor make-compiled-kernel (name args code fcaller-list group)))
  (group group :type group)
  (name name :type keyword)
  (args args :type list)
  (code code :type string)
  (fcaller-list fcaller-list :type list))

(defun render-to-string (backend group name-prefix avm debug kernels &aux (base-name (avm-name avm)))
  "Step5, rendering the graph.
(values cffi-name body foreign-function-caller compile-function-lambda)"
  (when (group-realize-on-vm group) (return-from render-to-string (values (list group) "")))
  (assert (listp kernels))
  (let ((code ""))
    (values
     (loop for kernel in kernels
	   for nth upfrom 0
	   for name = (setf (avm-name avm) (intern (format nil "~a_~a_k~a" base-name name-prefix (kernel-renderer-nth kernel)) "KEYWORD"))
	   for body = (%render-body backend backend (apply #'make-graph (kernel-renderer-nodes kernel))
				    (group-polyhedron group) 1 (kernel-renderer-args kernel))
	   for function = (%render-function backend avm (kernel-renderer-args kernel) body)
	   collect
	   (progn
	     (setf code (format nil "~a~%~a~%" code function))
	     (make-compiled-kernel name (kernel-renderer-args kernel)
				   function (%render-function-caller backend avm (kernel-renderer-args kernel)) group)))
     (progn
       (when (>= debug 1) (format t "Compiled[~a]:~%~a" name-prefix code))
       (setf (avm-name avm) base-name)
       code))))

(defun jit->vm (backend compiled-kernels)
  "Step5, collects the related nodes."
  (loop for kernel in compiled-kernels
	append
	(etypecase kernel
	  (Compiled-Kernel
	   (list
	    (make-fused-kernel-caller (ck-name kernel) (ck-args kernel) (compile nil (ck-fcaller-list kernel))
				      (ck-fcaller-list kernel)
				      (ck-code kernel) backend (count-n-kernels (group-render-graph (ck-group kernel))))))
	  (Group (graph-nodes (group-graph kernel))))))

(defun %jit (avm
	     &key
	       (debug (ctx:getenv :JIT_DEBUG))
	       (serialize (= 1 (ctx:getenv :SERIALIZE)))
	       (backend (or (ctx:getenv :JIT_BACKEND) :clang))
	       (compile-later nil)
	       (dir nil)
	     &aux
	       (backend (if (keywordp backend)
			    (default-device backend)
			    backend))
	       (verbose-schedule (or (= debug 2) (= debug 4)))
	       (verbose-auto (or (= debug 4) (= debug 3))))
  "Applies the jit, returning the compiled code.
DEBUG=1 to see the compiled code
DEBUG=2 to debug the scheduling process
DEBUG=3 to debug the ISL process
DEBUG=4 to debug both DEBUG=3 and DEBUG=4."
  (declare (type avm avm)
	   (type (integer 0 4) debug)
	   (type boolean serialize))
  (with-isl-context
    (let* ((groups (create-schedules-from-avm avm :verbose verbose-schedule))
	   (groups (loop for group in groups
			 if (group-realize-on-vm group) collect group
			   else if (group-sched group) do
			     (setf (group-polyhedron group) (create-polyhedron-from-group avm group :verbose verbose-schedule :verbose-auto verbose-auto))
				and collect group)))
      (mapc
       #'(lambda (x)
	   (when (group-polyhedron x)
	     (auto-schedule! (group-polyhedron x) :verbose verbose-auto :serialize serialize)
	     (funcall (compose #'remove-iteration-ir #'poly-pipeline #'group-polyhedron) x)
	     (setf (group-render-graph x) (finalize-and-retrive-render-graph x backend))))
       groups)
      (let* ((mp (make-instance 'MemoryPlanner :avm avm :groups groups :debug debug :device backend))
	     (_ (memory-plan mp))
	     (kernels (retrive-kernels mp))
	     (blueprints/codes
	       (loop for group in groups
		     for kernel in kernels
		     for nth upfrom 0
		     collect
		     (multiple-value-list (render-to-string backend group (format nil "e~a" nth) avm debug kernel))))
	     (final-code (%render-program-toplevel backend (with-output-to-string (out) (dolist (c blueprints/codes) (princ (second c) out))))))
	(declare (ignore _))
	(when (>= (ctx:getenv :JIT_DEBUG) 2)
	  (format t "Final JIT Schedule:~%")
	  (loop for nth upfrom 0
		for kr in kernels do
		  (format t "~%=== nth=~a ======" nth)
		  (if (group-p kr)
		      ;;(print (group-graph kr))
		      nil
		      (dolist (k kr) (print (kernel-renderer-nodes k))))))
	(unless compile-later (%render-compile backend avm final-code dir))
	(list
	 (map 'list #'car blueprints/codes) final-code mp
	 (loop for kr in kernels
	       if (listp kr)
		 append
		 (loop for k in kr append (kernel-renderer-args k))))))))

(defun jit (base-avm
	    &key
	      (debug (ctx:getenv :JIT_DEBUG))
	      (serialize (= 1 (ctx:getenv :SERIALIZE)))
	      (backend (or (ctx:getenv :JIT_BACKEND) :clang))
	      (dir nil)
	    &aux
	      (_ (apply-static-gensym base-avm))
	      (backend (if (keywordp backend)
			   (default-device backend)
			   backend))
	      (avm (deepcopy-avm base-avm)))
  "Applies the jit"
  (declare (type avm avm)
	   (type (integer 0 4) debug)
	   (type boolean serialize)
	   (ignore  _))
  (multiple-value-bind (compiled-kernels code mp kernel-args)
      (apply #'values (%jit avm :debug debug :serialize serialize :backend backend :compile-later nil :dir dir))
    (declare (ignore code))
    (make-avm
     (clean-up-attrs
      (optimize-non-in-place-buffers
       base-avm avm mp
       (remove-unused-allocs
	(apply
	 #'make-graph
	 (apply #'append (map 'list #'(lambda (x) (jit->vm backend x)) compiled-kernels))))
       (nodes-gather-args (graph-nodes (avm-graph avm)))
       (or (= debug 2) (= debug 4))
       kernel-args))
     (avm-name avm)
     (avm-id2tensor avm)
     (avm-fw-outputs avm)
     (avm-bw-outputs avm))))

(defun compile-isl (&key domain read write schedule (ast-option :separate))
  (let ((poly (make-polyhedral (make-avm (make-graph) :x (make-hash-table) nil nil)
			       (make-hash-table)
			       domain
			       read
			       write
			       (union-map-from-str schedule)
			       nil
			       nil
			       (make-hash-table)
			       :ast-option ast-option)))
    (auto-schedule! poly)
    (print (schedule-get-root (poly-schedule poly)))
    (print (debug/render-c poly))))

;; Loop Collapse https://github.com/zhen8838/isl_learn/blob/main/10_loop_transformation.ipynb
;; (union-set-apply domain xxx)
;; [TODO] Test w/
;; Including Test
;; !softmax
;; !matmul !matmul
;; !sin (!matmul)
;; Embedding
