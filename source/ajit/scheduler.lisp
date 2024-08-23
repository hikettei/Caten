(in-package :caten/ajit)
;; First, Nodes determined to be necessarily Fused (e.g.: non-viewed and same shaped tensors)
;; are combined into a single SubGraph and converted into an ISL AST.
;; Refenreces: https://pliss2019.github.io/albert_cohen_slides.pdf
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
           (equal (buffer-shape a) (buffer-shape b)))
      ->ok)
    ;; They still have a chance to be merged by the polyhedral compiler.
    ->ng))

(defparameter *recursive-find-seen* nil)
(defun recursive-find-group (graph scheduled-items)
  "Return -> (list scheduled-items ...)"
  (declare (type graph graph) (type scheduled-items scheduled-items))
  (flet ((explore (x) (when x (recursive-find-group graph x)))
	 (mergeable-p (x latest x-type) (or (numberp x) (and (id->value graph x) (not (eql (node-type (id->value graph x)) :Allocate)) (buffer-intersect-p latest x-type)))))
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
	;;(when (getattr node :_loop_bound_nodes) (return-from recursive-find-group))
	(when loop-bound-reads
	  (let* ((already-defined (or (getattr node :_loop_bound_nodes) (getattr node :_loop_bound_nodes_type)))
		 (equal? (equal (getattr node :_loop_bound_nodes) loop-bound-reads)))
	    (assert (or (null already-defined) equal?)
		    ()
		    ""))
	  (setf (node-attrs node)
		(append (node-attrs node)
			`(:_loop_bound_nodes ,loop-bound-reads :_loop_bound_nodes_type ,loop-bound-types))))
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
(defun %for (gid size)
  (declare (type (or number symbol) size))
  (emit (make-node :IR :for (list gid) (list 0 size 1))))
(defun %endfor (gid) (emit (make-node :IR :endfor nil (list gid))))
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
;; TODO: Flatten the loop if the access pattern is contiguous
(defun schedule->submodule (sched &aux (nrank 0) (args nil) (deps (schedule-depends-on sched)))
  "Lowers the grouped scheduled-items into the graph."
  (declare (type scheduled-items sched))
  (loop for node in (si-nodes sched)
	for reads = (relay-reads (read-type-relay node))
	if (vm-instruction-p node) do
	  (assert (every #'(lambda (x) (or (null x) (= 0 (buffer-nrank x)) (= (buffer-nrank x) (buffer-nrank (car reads))))) reads)
		  ()
		  "Tensors are not broadcasted properly: ~a" reads)
	  (setf nrank (max nrank (apply #'max (map 'list #'(lambda (x) (if x (buffer-nrank x) 0)) reads))))
	  (mapc #'(lambda (r type) (when (find r deps) (push type args))) (node-reads node) reads))
  (let* ((index-components (map 'list #'gid (range 0 nrank)))
	 (loopsizes (map 'list #'(lambda (x) (apply #'buffer->loop-size x nrank args)) (range 0 nrank))))
    (let ((g
	    (with-context
	      (start-loop (loop for i in index-components for s in loopsizes do (%for i s)))
	      (_ (dolist (node (si-nodes sched)) (emit node)))
	      (end-loop (dolist (i index-components) (%endfor i))))))
      (setf (graph-seen g) (schedule-depends-on sched))
      g)))

(defun schedule-depends-on (sched)
  "Enumerates the unsolved buffer ids from the sched graph."
  (declare (type scheduled-items sched))
  (nodes-depends-on (si-nodes sched)))

(defun graph->loop-factors (graph)
  (declare (type graph graph))
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (eql (node-type node) :FOR)
	   collect (car (node-writes node)))))

(defun graph->loop-size (graph)
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (and (eql (node-type node) :FOR)
		 (symbolp (second (node-reads node))))
	   collect (second (node-reads node)))))
;; ~~ ISL Renderers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun vm-instruction-p (node)
  "Add more classes here if you have a certain node that do not desired to be involved."
  ;; :IR = :FOR :ENDFOR
  (and
   (not (eql (node-class node) :IR))
   (not (eql (node-type node) :Allocate))))

(defun render-isl-aref (buffer &key (genid #'gid) (access-rep nil) (strides nil))
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
    (loop for nth upfrom 0
	  for stride-nth in (or strides (buffer-stride buffer))
	  for view in (buffer-views buffer)
	  for stride = (reveal-buffer stride-nth)
	  for upfrom = (reveal-buffer (or (nth 0 view) 0))
	  for by     = (reveal-buffer (or (nth 2 view) 1))
	  for broadcast-p = (nth 3 view)
	  for gid = (funcall genid nth)
	  append
	  (list
	   (progn
	     ;; Ugly solution... should be temporary...
	     ;; ISL assumes the domain to be an affine function.
	     ;; [TODO] FIX This (when `if` is scheduled by ISL, there's no way to update the index) to make mean working
	     (when (and (not (numberp stride)) access-rep) (setf stride 1))
	     (when (and (not (numberp by)) access-rep) (setf by 2))
	     ;; [TODO] upfrom can be a symbol? stride * (index + offset)
	     (when (and (not (numberp upfrom)) access-rep) (setf upfrom 1))
	     (if broadcast-p
		 (format nil "~a" upfrom)
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
	   "+")))))

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
    (maphash
     #'(lambda (timestamp subgraph)
	 (let* ((loop-factors (graph->loop-factors subgraph))
		(constraints
		  (loop for node in (graph-nodes subgraph)
			if (eql (node-type node) :FOR)
			  collect
			  (progn
			    (assert (= 1 (nth 2 (node-reads node))) () "Loop steps should be optimized by the polyhedral compiler. Set=1.")
			    (make-iconstraint (car (node-writes node)) (nth 0 (node-reads node)) (nth 1 (node-reads node)))))))
	   (if loop-factors
	       (progn
		 (format out "  T~a[~(~a~)]" timestamp (render-list loop-factors))
		 (format out " : ")
		 (format out "~a" (apply #'concatenate 'string (butlast (loop for c in constraints append (list (form c) " and ")))))
		 (format out ";~%"))
	       ;; It is asserted that :Allocation is always alone in the schedule.
	       (when (not
		      (and (= 1 (length (graph-nodes subgraph)))
			   (eql :Allocate (node-type (car (graph-nodes subgraph)))) ))
		 (format out "  T~a[];~%" timestamp)))))
     pipeline)
    (format out "}")))

(defun render-access (mode pipeline &key (depends-on nil))
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
    (maphash
     #'(lambda (timestamp subgraph)
	 (let* ((lf (graph->loop-factors subgraph))
		(occur-from
		  (format nil "T~a[~(~a~)]"
			  timestamp (render-list lf))))
	   (dolist (node (graph-nodes subgraph))
	     (when (not (eql (node-class node) :IR))
	       ;; When reduction is T, the first argument becomes the dependency
	       ;; e.g.: Tn[...]: A <- ADD(X, Y, reduction=t) is the equivalent to
	       ;; Tn[...]: A = (X += Y),  i.e.: Tn[...]: A = (X = X + Y)
	       ;; Here, X depends on X.
	       (when (getattr node :reduction)
		 (let ((reduce-to (car (node-reads node)))
		       (rt        (car (relay-reads (read-type-relay node)))))
		   (when (symbolp reduce-to)
		     (if (vm-instruction-p node)
			 (format out "  ~a -> ~(~a~)[~(~a~)];~%" occur-from reduce-to (render-isl-aref rt :access-rep t))
			 (error ":reduction for the op ~a is invaild." node)))))
	       (loop for r in (funcall (if (eql mode :read) #'node-reads #'node-writes) node)
		     for rt in (funcall (if (eql mode :read) #'relay-reads #'relay-writes) (read-type-relay node)) do
		       ;; When node has a :reduction
		       (when (symbolp r)
			 (if (null lf)
			     (format out "  ~a -> ~(~a~)[_total] : _total >= 0;~%" occur-from r)
			     (when (vm-instruction-p node)
			       (let ((access (render-isl-aref rt :access-rep t)))
				 (if (string= access "")
				     (format out "  ~a -> ~(~a~)[0];~%" occur-from r)
				     (format out "  ~a -> ~(~a~)[~(~a~)];~%" occur-from r access)))))))
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
		   (dolist (s symbols) (format out "  ~a -> ~(~a~)[0];~%" occur-from s))))))))
     pipeline)
    (format out "}")))

(defun isl-initial-schedule (pipeline &key depends-on)
  (let ((schedule :nothing))
    (maphash
     #'(lambda (ts graph)
	 (let* ((loop-factors (graph->loop-factors graph))
		(constraints
		  (loop for node in (graph-nodes graph)
			if (eql (node-type node) :FOR)
			  collect
			  (progn
			    (assert (= 1 (nth 2 (node-reads node))) () "Loop steps should be optimized by the polyhedral compiler. Set=1.")
			    (make-iconstraint (car (node-writes node)) (nth 0 (node-reads node)) (nth 1 (node-reads node))))))
		(dom (isl-union-set-read-from-str
		      (format nil
			      "[~(~a~)] -> { T~a[~(~a~)] : ~a }"
			      (render-list depends-on)
			      ts
			      (render-list loop-factors)
			      (apply #'concatenate 'string (butlast (loop for c in constraints append (list (form c) " and ")))))))
		(sched (isl-schedule-from-domain dom)))
	   (if (eql schedule :nothing)
	       (setf schedule sched)
	       (setf schedule (isl-schedule-sequence schedule sched)))))
     pipeline)
    schedule))
;; ~~ From AVM Into Polyhedral Model Compilation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; polyhedral compilation to determine the parallelization strategy
;; If we do; compile from avm into ISL, optimizng
;; This is the toplevel of all optimization stuff
;; TODO: コンパイルされた関数のデータ構造をきれいにしたい (defstruct Compiled-Function
;; (!rand (!softmax する時に，AoTしたrandを用いてx <- AUTOGEN_CUSTOM/RAND(x)みたいなのをできるようにしたい。
(defstruct (Group
	    (:constructor make-group (nodes realize-on-vm &aux (args (nodes-depends-on nodes)) (shapes (nodes-gather-args nodes)))))
  (graph (apply #'make-graph nodes) :type graph)
  (sched nil :type list)
  (realize-on-vm realize-on-vm :type boolean)
  (across-time-deps nil :type list)
  (args args :type list)
  (shapes shapes :type list)
  (writes (nodes-output-ids nodes) :type list))

(defun subgraph-scalar-load-p (graph id &aux (seen (make-hash-table)))
  (declare (type graph graph))
  (labels ((explore (x)
	     (or (numberp x)
		 (if (gethash x seen)
		     (= 0 (gethash x seen))
		     (let* ((node (id->value graph x))
			    (buff (car (relay-writes (read-type-relay node)))))
		       (setf (gethash x seen) (buffer-nrank buff))
		       (and
			(= 0 (buffer-nrank buff))
			(case (node-type node)
			  (:Load t)
			  (:Allocate (null (node-reads node)))
			  (otherwise nil))
			(every #'explore (node-reads node))))))))
    (explore id)))

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

(defun split-into-subgroups (graph)
  "Graphs are first breaked into subgroups only after:
- Tensor is shaped by a tensor
- :PAUSE/BACKWARD"
  (declare (type graph graph))
  (let ((groups))
    (labels ((scalar-load-p (id) (subgraph-scalar-load-p graph id))
	     (force-realize-on-vm (node)
	       (or
		(eql (node-type node) :pause/backward)
		(when (eql (node-type node) :Allocate)
		  (some #'null (map 'list #'scalar-load-p (node-reads node)))))))
      `(,@(loop for node in (graph-nodes graph)
		if (force-realize-on-vm node)
		  collect (make-group (nreverse groups) nil)
		  and collect (make-group (list node) t)
		  and do (setf groups nil)
		else
		  do (push node groups))
	,(make-group (nreverse groups) nil)))))
;; TODO: Relocate Alloc to the top of the nodes
;; TODO: Allocateと，そこから伸びるSubgraphの判定
(declaim (ftype (function (AVM &key (:verbose boolean)) (values list)) create-schedules-from-avm))
(defun create-schedules-from-avm (avm &key (verbose nil) &aux (backward-mode-p (not (null (avm-bw-outputs avm)))))
  "Step1, Creates an initial schedule"
  (declare (type avm avm) (type boolean verbose))
  ;; Trace the view and dtype information.
  (let* ((type-map (run-type-infer avm)) (*recursive-find-seen* nil) (seen nil))
    (when (and (not backward-mode-p)
	       (graph-nodes (avm-graph avm))
	       (eql :PAUSE/BACKWARD (node-type (car (last (graph-nodes (avm-graph avm)))))))
      ;; When no backward graph are compiled, remove :PAUSE/BACKWARD to make more chances of in-place computation.
      (setf (graph-nodes (avm-graph avm)) (butlast (graph-nodes (avm-graph avm)))))
    (when verbose
      (format t "Verbose: Initial Computation Graph[Forward/Backward]~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~~ JIT Specific Graph rewriting Processes ~~~~~~~~~~~~~~~~~~~~
    (deploy-type-infer-results avm type-map) ;; Move buffer/view nodes into :_type_relay attribtutes
    (relocate-independent-loop-bound-computation! (avm-graph avm))
    (apply-jit-specific-simplifiers avm)     ;; Purge :view nodes, WMMA Accumlation, contiguous elimination etc...
    (when verbose
      (format t "Verbose: Simplified Graph[Forward/Backward]~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
	       (nconc seen (group-writes group))
	       (multiple-value-bind (sorted seen-new)
		   (schedule/resolve-isolated-ops
		    (reverse (flatten (map 'list #'(lambda (x) (recursive-find-group (group-graph group) x)) schedules)))
		    seen)
		 (setf seen seen-new)
		 sorted))
	     (seen-in-groups (group &aux (seen-in-groups nil))
	       (if (group-sched group)
		   (loop for nth upfrom 0
			 for s in (group-sched group)
			 do (dolist (node (si-nodes s))
			      (unless (eql (node-type node) :Allocate)
				(nconc seen-in-groups (node-writes node))))
			    (setf (si-name s) (intern (format nil "T~a" nth) "KEYWORD")))
		   (loop for node in (graph-nodes (group-graph group))
			 unless (eql (node-type node) :Allocate)
			   do (nconc seen-in-groups (node-writes node))))
	       (remove-duplicates seen-in-groups))
	     (read-in-groups (group &aux (read-in-groups nil))
	       (if (group-sched group)
		   (loop for s in (group-sched group) do
		     (dolist (node (si-nodes s))
		       (dolist (r (node-reads node)) (when (symbolp r) (push r read-in-groups)))))
		   (loop for node in (graph-nodes (group-graph group)) do
		     (dolist (r (node-reads node)) (when (symbolp r) (push r read-in-groups)))))
	       (remove-duplicates read-in-groups)))
      (relocate-independent-allocations! (avm-graph avm))
      (print (avm-graph avm))
      (let* ((groups (split-into-subgroups (avm-graph avm))))
	(loop for group in groups
	      if (group-realize-on-vm group)
		do (nconc seen (group-writes group))
	      else
		collect (setf (group-sched group) (schedule group (make-top-schedule group))))
	(loop with write-deps = (map 'list #'seen-in-groups groups)
	      with read-deps = (map 'list #'read-in-groups groups)
	      for nth upfrom 1
	      for group in groups
	      for writing in write-deps
	      collect (setf (group-across-time-deps group) (intersection writing (apply #'append (nthcdr nth read-deps)))))
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
	groups
	(error "STOP")
	))))

(declaim (ftype (function (AVM group list &key (:verbose boolean)) (values Polyhedral)) create-polyhedron-from-schedule))
(defun create-polyhedron-from-schedule (avm group recursive-top-ids &key (verbose nil))
  "Step2, create a polyhedron from the scheduled items."
  (declare (type group group) (type boolean verbose))
  (let* ((submodule (map 'list #'schedule->submodule schedules)) ;; Rendering :FOR and :ENDFOR
	 (pipeline (make-hash-table)))
    (loop for nth upfrom 0
	  for s in submodule
	  do (setf (gethash nth pipeline) s))
    (when verbose
      (format t "== [Final Graph Before Applying Polyhedral Compiler] ======~%")
      (print-pipeline pipeline))
    
    (let* ((vm-inputs (avm-gather-args avm))
	   (loop-size (loop for value being the hash-values of pipeline
			    append (graph->loop-size value)))
	   (dynamic-shapes (remove-duplicates `(,@vm-inputs ,@loop-size)))
	   (domain       (render-domain pipeline :depends-on dynamic-shapes))
	   (read-access  (render-access :read pipeline :depends-on dynamic-shapes))
	   (write-access (render-access :write pipeline :depends-on dynamic-shapes))
	   (schedule     (isl-initial-schedule pipeline :depends-on dynamic-shapes)))
      (when verbose
	(format t "== [Domain] ===========")
	(format t "~%~a~%" domain)
	(format t "== [Read Accesses] =======")
	(format t "~%~a~%" read-access)
	(format t "== [Write Accesses] ======")
	(format t "~%~a~%" write-access)
	(format t "== [Initial Scheduling domain (=domain)] ======")
	(format t "~%~a~%" schedule)
	(isl-schedule-dump schedule))
      (make-polyhedral avm pipeline domain read-access write-access schedule vm-inputs recursive-top-ids))))

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
    (poly/reschedule polyhedral :serialize serialize)
    (debug-print "Reschedule")
    polyhedral))

(declaim (ftype (function (Polyhedral) graph) finalize-and-get-graph))
(defun finalize-and-get-render-graph (polyhedral)
  "Step4, Extract the schedule from ISL."
  (declare (type Polyhedral polyhedral))
  (create-rendering-graph polyhedral (finalize-schedule polyhedral)))

(defun render-to-string (backend name avm polyhedron rendering-graph debug compile-later &aux (base-name (avm-name avm)))
  "Step5, rendering the graph.
(values cffi-name body foreign-function-caller compile-function-lambda)"
  (setf (avm-name avm) (intern (string-upcase (format nil "~a_~a" (avm-name avm) name)) "KEYWORD"))
  (let* ((outputs (loop for o in (poly-vm-outputs polyhedron) if (poly/io-scalar-p polyhedron o) collect o))
	 (allocs (purge-allocations polyhedron (poly-pipeline polyhedron) (append (poly-vm-inputs polyhedron) outputs) rendering-graph))
	 ;; Start Rendering
	 (body     (%render-body backend backend rendering-graph polyhedron 1 allocs))
	 (function (%render-function backend avm allocs body))
	 (function (%render-program-toplevel backend function))
	 (fcaller-body (%render-function-caller backend avm allocs))
	 (name (avm-name avm)))
    (when (>= debug 1) (format t "Compiled[~a]:~%~a" name function))
    (setf (avm-name avm) base-name)
    (unless compile-later (%render-compile backend avm allocs function))
    (values
     name
     function
     fcaller-body
     allocs
     #'(lambda () (%render-compile backend avm allocs function)))))

(defun jit->vm (base-avm compiled-result polyhedron rendering-graph backend seen avm default-outs)
  "Step5, collects the related nodes."
  (multiple-value-bind (fname compiled-code fcaller-body allocs) (apply #'values compiled-result)
    (let* ((subgraph
	     (apply
	      #'append
	      (map
	       'list
	       #'(lambda (x &aux (x-in-base (or (id->value (avm-graph base-avm) (car (node-writes x))) x)))
		   (when (null (find (car (node-writes x)) seen))
		     (push (car (node-writes x)) seen)
		     (cond
		       ((eql (node-type x-in-base) :Allocate)
			(get-subgraph-recursively x-in-base (avm-graph base-avm) (poly-vm-inputs polyhedron) (getattr x :dtype)))
		       ((null (getattr x :_type_relay))
			(get-subgraph-recursively x-in-base (avm-graph base-avm) (poly-vm-inputs polyhedron) (getattr x :dtype)))
		       (T;;(find (car (node-writes x)) (poly-seen-in-groups polyhedron))
			;; Initialized in the jit graph
			;; -> Mutate them :Allocate (Also, they are labelled as TemporaryNode)
			(let* ((buffer (car (relay-writes (read-type-relay x))))
			       (args (map 'list #'reveal-buffer (append (buffer-shape buffer) (buffer-stride buffer))))
			       (args (map 'list
					  #'(lambda (id &aux (x (id->value (avm-graph base-avm) id)))
					      (if (null x)
						  id
						  (get-subgraph-recursively
						   x (avm-graph base-avm) (poly-vm-inputs polyhedron) (buffer-dtype buffer))))
					  args))
			       (args-list (loop for arg in args
						if (listp arg)
						  collect (car (node-writes (car (last arg))))
						else
						  collect arg)))
		          (append
			   (loop for n in (flatten args) if (node-p n) collect n)
			   (list
			    (make-node :Buffer :Allocate (node-writes x) args-list :dtype (buffer-dtype buffer) :nrank (buffer-nrank buffer) :_tmp t))))))))
	       (or
		allocs
		;; If no allocations occured:
		(loop for o in default-outs
		      collect
		      (find o (graph-nodes (avm-graph avm)) :key (compose #'car #'node-writes)))))))
	   (jit-kernel (make-fused-kernel-caller
			fname allocs (compile nil fcaller-body) fcaller-body
			compiled-code backend (count-n-kernels rendering-graph))))
      (values
       (apply #'make-graph (append subgraph (list jit-kernel)))
       (append seen (node-writes jit-kernel))))))

(defun %jit (avm
	     &key
	       (debug (ctx:getenv :JIT_DEBUG))
	       (serialize (= 1 (ctx:getenv :SERIALIZE)))
	       (static-gensym (= 1 (ctx:getenv :STATIC_GENSYM)))
	       (backend (or (ctx:getenv :JIT_BACKEND) :clang))
	       (compile-later nil)
	     &aux
	       (_ (when static-gensym (apply-static-gensym avm)))
	       (base-avm avm)
	       (avm (deepcopy-avm avm))
	       (*isl-context* (isl-ctx-alloc))
	       (verbose-schedule (or (= debug 2) (= debug 4)))
	       (verbose-auto (or (= debug 4) (= debug 3))))
  "Applies the jit, returning the compiled code.
DEBUG=1 to see the compiled code
DEBUG=2 to debug the scheduling process
DEBUG=3 to debug the ISL process
DEBUG=4 to debug both DEBUG=3 and DEBUG=4."
  (declare (type avm avm)
	   (type (integer 0 4) debug)
	   (type boolean serialize)
	   (ignore _))
  (let ((groups (create-schedules-from-avm avm :verbose verbose-schedule))
	(fw-polyhedron (create-polyhedron-from-schedule avm fw-schedule (avm-fw-outputs avm) :verbose verbose-schedule))
	(bw-polyhedron (when bw-schedule (create-polyhedron-from-schedule avm bw-schedule (avm-bw-outputs avm) :verbose verbose-schedule))))
    ;; Doing auto-schedule
    (auto-schedule! fw-polyhedron :verbose verbose-auto :serialize serialize)
    (when bw-polyhedron (auto-schedule! bw-polyhedron :verbose verbose-auto :serialize serialize))
    ;; Remove :FOR :ENDFOR
    (funcall (compose #'remove-iteration-ir #'poly-pipeline) fw-polyhedron)
    (when bw-polyhedron (funcall (compose #'remove-iteration-ir #'poly-pipeline) bw-polyhedron))
    ;; Finalize-schedule
    (let* ((fw-render-graph (finalize-and-get-render-graph fw-polyhedron))
	   (bw-render-graph (when bw-polyhedron (finalize-and-get-render-graph bw-polyhedron)))
	   (refcount (create-reference-counter fw-polyhedron fw-render-graph bw-polyhedron bw-render-graph)))
      ;; Create a reference count and apply memory-planner
      (apply-memory-planner! avm fw-polyhedron refcount fw-render-graph save-for-backwards)
      (when bw-polyhedron (apply-memory-planner! avm bw-polyhedron refcount bw-render-graph nil))
      ;; Compilation process was finished
      ;; Rendering the graph
      ;; (values fname compiled-code kernel-caller invoke-compile-f
      (let ((forward
	      (multiple-value-list
	       (render-to-string backend "forward" avm fw-polyhedron fw-render-graph debug compile-later)))
	    (backward
	      (when bw-polyhedron
		(multiple-value-list
		 (render-to-string backend "backward" avm bw-polyhedron bw-render-graph debug compile-later)))))
	(multiple-value-bind (graphf seen) (jit->vm base-avm forward fw-polyhedron fw-render-graph backend nil avm (avm-fw-outputs avm))
	  (multiple-value-bind (graphb seen) (when bw-polyhedron (jit->vm base-avm backward bw-polyhedron bw-render-graph backend seen avm (avm-bw-outputs avm)))
	    (declare (ignore seen))
	    (values avm forward graphf backward graphb)))))))	      

(defun jit (avm
	    &key
	      (debug (ctx:getenv :JIT_DEBUG))
	      (serialize (= 1 (ctx:getenv :SERIALIZE)))
	      (static-gensym (= 1 (ctx:getenv :STATIC_GENSYM)))
	      (backend (or (ctx:getenv :JIT_BACKEND) :clang)))
  "Applies the jit"
  (declare (type avm avm)
	   (type (integer 0 4) debug)
	   (type boolean serialize))

  (multiple-value-bind (avm fw-result fw-graph bw-result bw-graph)
      (%jit avm :debug debug :serialize serialize :static-gensym static-gensym
		:backend backend :compile-later nil)
    (declare (ignore fw-result bw-result))
    (make-avm
     (apply
      #'make-graph
      (append
       (graph-nodes fw-graph)
       (when bw-graph
	 (list (make-node :Special/VM :Pause/Backward nil nil)))
       (when bw-graph
	 (graph-nodes bw-graph))))
     (avm-name avm)
     (avm-id2tensor avm)
     (avm-fw-outputs avm)
     (avm-bw-outputs avm))))
