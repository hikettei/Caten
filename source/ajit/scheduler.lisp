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
Further op-fusion optimization are done by the polyhedral-compiler"
  (declare (type Buffer a b))
  ;; either of buffers are scalar -> merge them
  (symbol-macrolet ((->ok (return-from buffer-intersect-p t))
		    (->ng (return-from buffer-intersect-p nil)))
    ;; Either of args is a scalar -> merge them
    (when (or (= (buffer-nrank a) 0) (= 0 (buffer-nrank b)))->ok)
    ;; Contiguous and same-shaped buffer -> merge them
    (when (and
	   (every #'null (buffer-views a))
	   (every #'null (buffer-views b))
	   (equal (buffer-shape a) (buffer-shape b)))
      ->ok)
    ;; Otherwise, leave it to out polyhedral compiler.
    ->ng))

(defun recursive-find-group (avm type-map scheduled-items)
  "Return -> (list scheduled-items ...)"
  (declare (type avm avm)
	   (type type-reporter type-map)
	   (type scheduled-items scheduled-items))
  (flet ((id->type (x) (map/type-of type-map x))
	 (explore (x) (when x (recursive-find-group avm type-map x))))
    (with-slots ((latest latest) (latest-id latest-id)) scheduled-items
      (when (find latest-id (rp-seen type-map)) (return-from recursive-find-group))
      (let* ((node (id->value (avm-graph avm) latest-id))
	     (children (node-reads node))
	     (mergeable-list
	       (map 'list
		    #'(lambda (x)
			(or (numberp x)
			    (and
			     (not (eql (node-type (id->value (avm-graph avm) x)) :Allocate))
			     (buffer-intersect-p latest (id->type x)))))
		    children)))
	(setf (rp-seen type-map) (append (rp-seen type-map) (node-writes node)))
	;; Top_ID <- F(Children[0], Children[1], ...)
	;;             mergeable[0] mergeable[1], ...
	(if (every #'identity mergeable-list)
	    (let ((parent-groups))
	      (dolist (c children)
		(when (not (numberp c))
		  (si/append-item scheduled-items (id->value (avm-graph avm) c))))
	      (dolist (c children)
		(when (not (numberp c))
		  (setf (si-latest scheduled-items) (id->type c)
			(si-latest-id scheduled-items) c)
		  (setf parent-groups (append parent-groups (cdr (explore scheduled-items))))))
	      (append (list scheduled-items) parent-groups))
	    (let ((new-groups
		    (map
		     'list
		     #'(lambda (x)
			 (when (not (numberp x))
			   (make-scheduled-items (list (id->value (avm-graph avm) x) (id->type x) x))))
		     children)))
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
	 (shapes (map 'list #'(lambda (x) (nth dim (buffer-shape x))) buffers)))
    (reveal-buffer
     (or
      (when (every #'numberp shapes) (apply #'max shapes))
      (when (every #'(lambda (x) (eql x 1)) shapes) 1)
      (car shapes)))))

(defun schedule->submodule (sched type-map)
  "Lowers the grouped scheduled-items into the graph."
  (declare (type scheduled-items sched))
  (flet ((id->buffer (x) (map/type-of type-map x)))
    (let* ((args (append
		  (map 'list #'id->buffer (schedule-depends-on sched))
		  (loop for node in (si-nodes sched)
			if (vm-instruction-p node)
			  append (map 'list #'id->buffer (node-writes node)))))
	   (nrank (if args (apply #'max (map 'list #'buffer-nrank args)) 0))
	   (index-components (map 'list #'gid (range 0 nrank)))
	   (loopsizes (map 'list #'(lambda (x) (apply #'buffer->loop-size x nrank args)) (range 0 nrank))))
      (let ((g
	      (with-context
		(start-loop (loop for i in index-components for s in loopsizes do (%for i s)))
		(_ (dolist (node (si-nodes sched)) (emit node)))
		(end-loop (dolist (i index-components) (%endfor i))))))
	(setf (graph-seen g) (schedule-depends-on sched))
	g))))

(defun schedule-depends-on (sched)
  "Enumerates the unsolved buffer ids from the sched graph."
  (declare (type scheduled-items sched))
  (let ((seen) (depends-on))
    (loop for node in (si-nodes sched) do
      (dolist (r (node-reads node))
	(when (null (find r seen))
	  (when (symbolp r)
	    (push r depends-on))
	  (push r seen)))
      (dolist (w (node-writes node))
	(push w seen)))
    (reverse depends-on)))

(defun graph->loop-factors (graph)
  (declare (type graph graph))
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (eql (node-type node) :FOR)
	   collect (car (node-writes node)))))
;; ~~ ISL Renderers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun vm-instruction-p (node)
  "Add more classes here if you have a certain node that do not desired to be involved."
  ;; :IR = :FOR :ENDFOR
  (and
   (not (eql (node-class node) :IR))
   (not (eql (node-type node) :Allocate))))

(defun render-isl-aref (id type-map &key (genid #'gid))
  "Renders the stride computation for ISL:
```
A[stride1 * view_info1 * index_component_0 + bias1 + stride2 * view_info2 * index_component_1 + bias2 + ...]
```
"
  (declare (type symbol id)
	   (type type-reporter type-map))
  (let ((buffer (map/type-of type-map id)))
    (apply
     #'concatenate
     'string
     (butlast
      (loop for nth upfrom 0
	    for stride-nth in (buffer-stride buffer)
	    for view in (buffer-views buffer)
	    for stride = (reveal-buffer stride-nth)
	    for upfrom = (reveal-buffer (or (nth 0 view) 0))
	    for by     = (reveal-buffer (or (nth 2 view) 1))
	    for broadcast-p = (nth 3 view)
	    for gid = (funcall genid nth)
	    append
	    (list
	     (progn
	       (assert (typep stride 'integer-t) () "(A bug of caten/ajit) Resolve this ~a" stride)
	       (assert (typep upfrom 'integer-t) () "(A bug of caten/ajit) Resolve this ~a" upfrom)
	       (assert (typep by 'integer-t)     () "(A bug of caten/ajit) Resolve this ~a" by)
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
			   (if (eql upfrom 0) "" (format nil "+~a" upfrom)))))
	     "+"))))))

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
	       (progn
		 (format out "  T~a[];~%" timestamp)))))
     pipeline)
    (format out "}")))

(defun render-access (mode pipeline type-map &key (depends-on nil))
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
		 (let ((reduce-to (car (node-reads node))))
		   (when (symbolp reduce-to)
		     (if (vm-instruction-p node)
			 (format out "  ~a -> ~(~a~)[~(~a~)];~%" occur-from reduce-to (render-isl-aref reduce-to type-map))
			 (error ":reduction for the op ~a is invaild." node)))))
	       (dolist (r (remove-duplicates (funcall (if (eql mode :read) #'node-reads #'node-writes) node)))
		 ;; When node has a :reduction
		 (when (symbolp r)
		   (if (null lf)
		       (format out "  ~a -> ~(~a~)[_total] : _total >= 0;~%" occur-from r)
		       (when (vm-instruction-p node)
			 (let ((access (render-isl-aref r type-map)))
			   (if (string= access "")
			       (format out "  ~a -> ~(~a~)[0];~%" occur-from r)
			       (format out "  ~a -> ~(~a~)[~(~a~)];~%" occur-from r access)))))))))))
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
;; polyhedral compilation to determine the parallelization strategy
;; If we do; compile from avm into ISL, optimizng
;; This is the toplevel of all optimization stuff
(declaim (ftype (function (AVM &key (:verbose boolean)) (values Polyhedral Type-Reporter)) create-polyhedral-model))
(defun create-polyhedral-model (avm &key (verbose nil))
  "Creates the polyhedral model given the avm."
  (declare (type avm avm) (type boolean verbose))
  (let* ((type-map (run-type-infer avm))
	 (recursive-top-ids (append (avm-fw-outputs avm) (avm-bw-outputs avm)))
	 (dynamic-shapes (avm-gather-args avm)))
    (when verbose
      (format t "== [Initial Graph] ==~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~ Optimizations ~~
    ;; Do not verify the graph; nodes used to compute views may lost.
    (apply-jit-specific-simplifiers avm)
    (when verbose
      (format t "== [Graph after applying jit-specific simplifiers] ==~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    (flet ((id->buffer (id)
	     (assert (symbolp id) () "Graph should not return a number!")
	     (list (id->value (avm-graph avm) id) (map/type-of type-map id) id)))
      (let* ((schedules (map 'list (compose #'make-scheduled-items #'id->buffer) recursive-top-ids))
	     (scheduled (reverse (flatten (map 'list #'(lambda (x) (recursive-find-group avm type-map x)) schedules)))))
	;; verify-graph assets no duplication in branches from recursive-top-ids
	(loop for nth upfrom 0
	      for s in scheduled
	      do (setf (si-name s) (intern (format nil "T~a" nth) "KEYWORD")))
	(when verbose
	  (format t "== [Graph after applying an initial scheduler process] ==~%")
	  (print-schedules scheduled))
	
	(let* ((graphs (map 'list #'(lambda (x) (schedule->submodule x type-map)) scheduled))
	       (pipeline (make-hash-table)))
	  ;; Pipeline: T_ID -> Submodule_Graph
	  (loop for nth upfrom 0
		for g in graphs
		do (setf (gethash nth pipeline) g))
	  ;; [TODO]: When graph is fused to single?
	  ;; -> SKip the polyhedral compilation
	  ;; Creates the initial problem:
	  (let* ((domain       (render-domain pipeline :depends-on dynamic-shapes))
		 (read-access  (render-access :read pipeline type-map :depends-on dynamic-shapes))
		 (write-access (render-access :write pipeline type-map :depends-on dynamic-shapes))
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
	    (values (make-polyhedral avm pipeline domain read-access write-access schedule) type-map)))))))

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

(defun remove-iteration-ir (pipeline)
  (loop for nth being each hash-keys of pipeline
	  using (hash-value graph)
	do (setf (graph-nodes graph)
		 (loop for node in (graph-nodes graph)
		       unless (or (eql (node-type node) :FOR) (eql (node-type node) :ENDFOR))
			 collect node))))
;; TODO: making isl objects gc-reachable
;; TODO: dynamic shapes
;; TODO: Gemmがab bc -> acするようにしたい
(defun jit (avm
	    &key
	      (debug (ctx:getenv :JIT_DEBUG))
	      (serialize (= 1 (ctx:getenv :SERIALIZE)))
	      (static-gensym (= 1 (ctx:getenv :STATIC_GENSYM)))
	      (backend (or (ctx:getenv :JIT_BACKEND) :clang)))
  "Applies the jit"
  (declare (type avm avm)
	   (type (integer 0 3) debug)
	   (type boolean serialize))
  (when static-gensym (apply-static-gensym avm))
  (multiple-value-bind (verbose-schedule verbose-auto)
      (values (or (= debug 3) (= debug 1)) (or (= debug 3) (= debug 2)))
    (multiple-value-bind (polyhedron type-map)
	(create-polyhedral-model avm :verbose verbose-schedule)
      (auto-schedule! polyhedron :verbose verbose-auto :serialize serialize)
      (when (>= debug 1)
	(format t "~% == [Final Polyhedron] ====~%~a~%" polyhedron))
      polyhedron
      (remove-iteration-ir (poly-pipeline polyhedron))
      (apply-alias-for-rendering-graph (poly-pipeline polyhedron))
      (let* ((extracted-schedule (finalize-schedule polyhedron))
	     (r-graph (create-rendering-graph polyhedron extracted-schedule))
	     (render (%render-subroutine backend backend r-graph polyhedron 0 type-map)))
	render))))
