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

(defun recursive-find-group (avm scheduled-items &key (seen nil))
  "Return -> (list scheduled-items ...)"
  (declare (type avm avm) (type scheduled-items scheduled-items))
  (flet ((explore (x) (when x (recursive-find-group avm x :seen seen)))
	 (mergeable-p (x latest x-type) (or (numberp x) (and (id->value (avm-graph avm) x) (not (eql (node-type (id->value (avm-graph avm) x)) :Allocate)) (buffer-intersect-p latest x-type)))))
    (with-slots ((latest latest) (latest-id latest-id)) scheduled-items
      (when (find latest-id seen) (return-from recursive-find-group))
      (let* ((node (or (id->value (avm-graph avm) latest-id) (return-from recursive-find-group)))
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
				     if (and (null (find s seen)) (id->value (avm-graph avm) s)) collect s))
	     (loop-bound-types (map 'list #'(lambda (x) (car (relay-writes (read-type-relay (id->value (avm-graph avm) x))))) loop-bound-reads))
	     (children `(,@children ,@loop-bound-reads))
	     (children-type `(,@children-type ,@loop-bound-types))
	     (mergeable-list (map 'list #'(lambda (x x-type) (mergeable-p x latest x-type)) children children-type)))
	(when loop-bound-reads
	  (assert (null (getattr node :_loop_bound_nodes)))
	  (assert (null (getattr node :_loop_bound_nodes_type)))
	  (setf (node-attrs node)
		(append (node-attrs node)
			`(:_loop_bound_nodes ,loop-bound-reads :_loop_bound_nodes_type ,loop-bound-types))))
	(setf seen (append seen (node-writes node)))
	;; node_id <- F(Children[0], Children[1], ...)
	;;              mergeable[0] mergeable[1], ...
	;; All mergeable -> merge them!
	;; if the index relationships are too complicated to judge mergeablity, leave it to the polyhedron.
	(if (and (not schedule-standalone-p) (every #'identity mergeable-list))
	    (let ((parent-groups))
	      (dolist (c children)
		(when (not (numberp c))
		  (si/append-item scheduled-items (id->value (avm-graph avm) c))))
	      (loop for c in children
		    for ct in children-type do
		      (when (not (numberp c))
			(setf (si-latest scheduled-items) ct
			      (si-latest-id scheduled-items) c)
			(setf parent-groups (append parent-groups (cdr (explore scheduled-items))))))
	      (values (append (list scheduled-items) parent-groups) seen))
	    ;; Create new and independent schedule item for each args.
	    (let ((new-groups
		    (map
		     'list
		     #'(lambda (x x-type)
			 (when (not (numberp x))
			   (make-scheduled-items (list (id->value (avm-graph avm) x) x-type x))))
		     children children-type)))
	      (values
	       (append
		(list scheduled-items)
		(loop for n in (map 'list #'explore new-groups) if n collect n))
	       seen)))))))
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
	     ;; [TODO] FIX This (when `if` is scheduled by ISL, there's no way to update the index) to make mean working
	     (when (and (not (numberp stride)) access-rep) (setf stride 1))
	     (when (and (not (numberp by)) access-rep) (setf by 2))
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
	       ;; fails to render :Allocate to purge the allocation in the schedule.
	       ;; It is asserted that :Allocation is always alone in the schedule.
	       (when (not (and (= 1 (length (graph-nodes subgraph)))
			       (eql :Allocate (node-type (car (graph-nodes subgraph))))
			       (> (getattr (car (graph-nodes subgraph)) :nrank) 0)))
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
(declaim (ftype (function (AVM list &key (:verbose boolean) (:seen list)) (values Polyhedral list)) create-polyhedral-model-from-top-ids))
(defun create-polyhedral-model-from-top-ids (avm recursive-top-ids &key (verbose nil) (seen nil))
  "Input: AVM: JIT-Specific Optimization Applied AVM"
  (declare (type avm avm) (type list recursive-top-ids) (type boolean) (type list seen))
  (when verbose (format t "[Creating a polyhedron for group: ~a]~%" recursive-top-ids))
  (flet ((id->buffer (id)
	   (assert (symbolp id) () "Graph should not return a number!")
	   (let ((node (id->value (avm-graph avm) id)))
	     (list node (car (relay-writes (read-type-relay node))) id))))
    ;; Creating a JIT groups for this recursive-top-ids
    (let* ((schedules (map 'list (compose #'make-scheduled-items #'id->buffer) recursive-top-ids))
	   (scheduled (reverse
		       (flatten
			(map
			 'list
			 #'(lambda (x)
			     (multiple-value-bind (out seen-new) (recursive-find-group avm x :seen seen)
			       (setf seen seen-new)
			       out))
			 schedules)))))
      ;; verify-graph assets no duplication in branches from recursive-top-ids
      (loop for nth upfrom 0
	    for s in scheduled
	    do (setf (si-name s) (intern (format nil "T~a" nth) "KEYWORD")))
      (when verbose
	(format t "== [Graph after applying an initial scheduler process] ==~%")
	(print-schedules scheduled))
      ;; Creating :FOR :ENDFOR (From Schedule -> Pipeline)
      (let* ((graphs (map 'list #'schedule->submodule scheduled))
	     (pipeline (make-hash-table)))
	;; Pipeline is a hash table where key and values are: T_ID -> Submodule_Graph
	(loop for nth upfrom 0
	      for g in graphs
	      do (setf (gethash nth pipeline) g))
	(%simplify-pipeline pipeline recursive-top-ids)
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
	  (values (make-polyhedral avm pipeline domain read-access write-access schedule vm-inputs recursive-top-ids nil) seen))))))
;; polyhedral compilation to determine the parallelization strategy
;; If we do; compile from avm into ISL, optimizng
;; This is the toplevel of all optimization stuff
(declaim (ftype (function (AVM &key (:verbose boolean) (:more-groups list)) list) create-polyhedral-model))
(defun create-polyhedral-model (avm &key (verbose nil) (more-groups nil) &aux (seen nil))
  "Creates the polyhedral model given the avm."
  (declare (type avm avm) (type boolean verbose))
  (let* ((type-map (run-type-infer avm)))
    (flet ((%create-polyhedral-model-from-top-ids (&rest args)
	     (multiple-value-bind (out seen-new) (apply #'create-polyhedral-model-from-top-ids args)
	       (setf seen seen-new)
	       out)))
      (when (and (null (avm-bw-outputs avm))
		 (graph-nodes (avm-graph avm))
		 (eql :PAUSE/BACKWARD (node-type (car (last (graph-nodes (avm-graph avm)))))))
	(setf (graph-nodes (avm-graph avm)) (butlast (graph-nodes (avm-graph avm)))))
      (when verbose
	(format t "== [Initial Graph] ==~%")
	(uiop:symbol-call (find-package :caten) :print-avm avm))
      ;; ~ JIT-Specific Optimizations ~~
      ;; Do not verify the graph; nodes used to compute views may lost.
      (deploy-type-infer-results avm type-map) ;; Let them include :VIEW node inside :_type_relay attrs
      (apply-jit-specific-simplifiers avm)     ;; WMMA Accumlation etc
      (append
       ;; Forward Group
       (list (%create-polyhedral-model-from-top-ids avm (avm-fw-outputs avm) :verbose verbose :seen seen))
       ;; Backward Group
       (when (avm-bw-outputs avm)
	 (list (%create-polyhedral-model-from-top-ids avm (avm-bw-outputs avm) :verbose verbose :seen seen)))
       (map 'list #'(lambda (x) (%create-polyhedral-model-from-top-ids avm x :verbose verbose :seen seen)) more-groups)))))

(declaim (ftype (function (Polyhedral &key (:verbose boolean) (:serialize boolean)) Polyhedral) auto-schedule!))
(defun auto-schedule! (polyhedral &key (verbose nil) (serialize nil))
  "
Options:
- debug[boolean]:  If this option is set, this function prints the Polyhedron Model for each step of the optimization.
- serialize[boolean]: If this option is set, then all strongly connected components in the dependence
  graph are serialized as soon as they are detected. This means in particular that
  instances of statements will only appear in the same band node if these statements belong to
  the same strongly connected component at the point where the band node is constructed."
  (declare (type Polyhedral polyhedral)
	   (type boolean verbose serialize))
  (macrolet ((debug-print (step-name) `(when verbose (format t "~%[~a]~%~a~%" ,step-name polyhedral))))
    (debug-print "Initial")
    ;; Loop Fusion
    (poly/reschedule polyhedral :serialize serialize)
    (debug-print "Reschedule")
    polyhedral))
;; ~~ Fused Kernel Objects ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (JIT-Info)
  (caller #'(lambda ()) :type (or null function))
  (caller-body nil :type list)
  (lang :nil :type keyword)
  (code "" :type string)
  (n-kernels 0 :type fixnum)
  (load-p t :type boolean)) ;; the count of outermost loops
(defmethod make-load-form ((jit Jit-Info) &optional env)
  (declare (ignore env))
  `(make-jit-info :caller ,(jit-info-caller-body jit) :caller-body nil :lang ,(jit-info-lang jit) :code ,(jit-info-code jit) :n-kernels ,(jit-info-n-kernels jit) :load-p nil))
(defmethod print-object ((s jit-info) stream) (format stream "<~a Code [~a kernels]>" (jit-info-lang s) (jit-info-n-kernels s)))
(defun make-fused-kernel-caller (allocs lambda fcaller-body code lang n-kernels)
  (make-node :IR :JIT_KERNEL
	     (apply #'append (map 'list #'node-writes allocs))
	     (apply #'append (map 'list #'node-writes allocs))
	     :jit-info (make-jit-info :caller lambda :caller-body fcaller-body :lang lang :code code :n-kernels n-kernels)))
(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((jit (getattr node :jit-info)))
    (assert (jit-info-p jit) () "~a is not a jit kernel. :jit-info=~a" node jit)
    (apply (jit-info-caller jit) args))
  (apply #'values args))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun compile/polyhedron (refcount base-avm avm polyhedron &key (debug 0) (name nil) (backend nil) (seen nil) &aux (base-name (avm-name avm)))
  (declare (type polyhedral polyhedron))
  (setf (avm-name avm) (intern (string-upcase (format nil "~a_~a" (avm-name avm) name)) "KEYWORD"))
  ;; Minimizing the number of allocation by creating an alias
  ;; After applying memory-planner, it breaks write-must-be-exist-once rule of aIR graph
  ;; so you cannot verify the graph!
  ;; Preprocessing
  (let* ((extracted-schedule (finalize-schedule polyhedron))
	 (rendering-graph (create-rendering-graph polyhedron extracted-schedule))
	 (_      (apply-memory-planner refcount polyhedron avm rendering-graph))
	 (outputs (loop for o in (poly-vm-outputs polyhedron) if (poly/io-scalar-p polyhedron o) collect o))
	 (allocs (purge-allocations polyhedron (poly-pipeline polyhedron) (append (poly-vm-inputs polyhedron) outputs)))
	 ;; Start Rendering
	 (body     (%render-body backend backend rendering-graph polyhedron 1 allocs))
	 (function (%render-function backend avm allocs body))
	 (function (%render-program-toplevel backend function))
	 (fcaller-body (%render-function-caller backend avm allocs))
	 (f (compile nil fcaller-body)))
    (declare (ignore _))
    (when (>= debug 1) (format t "Compiled[~a]:~%~a" name function))
    (restart-case (%render-compile backend avm allocs function)
      (zenity/hand-rewrite-code ()
	:report "Calling a GUI Editor, update the code manually. (SHOULD ONLY BE USED FOR DEBUGGING)"
	(%render-compile backend avm allocs (zenity/prompt-new-value function))))
    (let* ((subgraph
	     (apply
	      #'append
	      (map
	       'list
	       #'(lambda (x &aux (x-in-base (or (id->value (avm-graph base-avm) (car (node-writes x))) x)))
		   (when (null (find x seen))
		     (get-subgraph-recursively x-in-base (avm-graph base-avm) (poly-vm-inputs polyhedron) (getattr x :dtype))))
	       allocs))))
      (setf (avm-name avm) base-name)
      (values (apply #'make-graph (append subgraph (list (make-fused-kernel-caller allocs f fcaller-body function backend (count-n-kernels rendering-graph))))) seen))))

(defun jit (avm
	    &key
	      (more-groups nil)
	      (debug (ctx:getenv :JIT_DEBUG))
	      (serialize (= 1 (ctx:getenv :SERIALIZE)))
	      (static-gensym (= 1 (ctx:getenv :STATIC_GENSYM)))
	      (backend (or (ctx:getenv :JIT_BACKEND) :clang))
	    &aux
	      (_ (when static-gensym (apply-static-gensym avm)))
	      (base-avm avm)
	      (avm (deepcopy-avm avm))
	      (*isl-context* (isl-ctx-alloc)))
  "Applies the jit"
  (declare (type avm avm)
	   (type (integer 0 4) debug)
	   (type boolean serialize)
	   (ignore _))
  (multiple-value-bind (verbose-schedule verbose-auto)
      (values (or (= debug 4) (= debug 3)) (or (= debug 4) (= debug 2)))
    (let* ((refcount (create-reference-count (avm-graph avm)))
	   (polyhedrons (create-polyhedral-model avm :verbose verbose-schedule :more-groups more-groups)))
      (mapc
       #'(lambda (x)
	   (auto-schedule! x :verbose verbose-auto :serialize serialize)
	   (when (>= debug 2) (format t "~% == [Final Polyhedron] ====~%~a~%" x)))
       polyhedrons)
      ;; Polyhedron supercedes :FOR/:ENDFOR, and we dont need it anymroe, remove them.
      (mapc (compose #'remove-iteration-ir #'poly-pipeline) polyhedrons)
      (poly/solve-group-deps polyhedrons)
      (let ((vars (apply #'append (map 'list #'apply-multiexpr-grouping polyhedrons))))
	(when (>= debug 1)
	  (format t "~%[JIT] Removed ~a tensors by multiexpr-grouping.~a~%" (length vars)
		  (if (>= debug 4) (format nil ":~%~a" vars) ""))))
      ;; Remove extra allocations
      ;; [TODO] 全部EXPRにする
      ;; No-need-to-allocationが発生する
      ;; そうしたらAllocを削除
      ;;(when (and (= (length polyhedrons) 1) multiexpr)
      ;;  (mapc #'(lambda (x) (apply-multiexpr-grouping (poly-pipeline x))) polyhedrons))
      (let* ((seen)
	     (jit-graphs
	       (map 'list
		    #'(lambda (x name)
			(multiple-value-bind (out seen-new)
			    (compile/polyhedron refcount base-avm avm x :backend backend :debug debug :name name :seen seen)
			  (setf seen seen-new)
			  out))
		    polyhedrons
		    (append (list "Forward" "Backward") (map 'list #'(lambda (x) (format nil "SUBGRAPH_~a" x)) (range 0 (length more-groups)))))))
	;; TODO: (isl-free-ctx *isl-context*)
	;; TODO: Tracing the jit-compiled AVM (including IfNode/MapNode etc)
	;; we can export the entire vm to clang
	(make-avm
	 (apply
	  #'make-graph
	  (append
	   (graph-nodes (car jit-graphs))
	   (when (second jit-graphs)
	     (list (make-node :Special/VM :Pause/Backward nil nil)))
	   (apply #'append (map 'list #'graph-nodes (cdr jit-graphs)))))
	 (avm-name avm)
	 (avm-id2tensor avm)
	 (avm-fw-outputs avm)
	 (avm-bw-outputs avm))))))
