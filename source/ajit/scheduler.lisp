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
    ;; Either of args is a scalar -> merge them
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
  (declare (type avm avm)
	   (type scheduled-items scheduled-items))
  (flet ((explore (x) (when x (recursive-find-group avm x :seen seen)))
	 (mergeable-p (x latest x-type) (or (numberp x) (and (not (eql (node-type (id->value (avm-graph avm) x)) :Allocate)) (buffer-intersect-p latest x-type)))))
    (with-slots ((latest latest) (latest-id latest-id)) scheduled-items
      (when (find latest-id seen) (return-from recursive-find-group))
      (let* ((node (id->value (avm-graph avm) latest-id))
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
	      (append (list scheduled-items) parent-groups))
	    ;; Create new and independent schedule item for each args.
	    (let ((new-groups
		    (map
		     'list
		     #'(lambda (x x-type)
			 (when (not (numberp x))
			   (make-scheduled-items (list (id->value (avm-graph avm) x) x-type x))))
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
	 (shapes (map 'list #'(lambda (x) (nth dim (buffer-shape x))) buffers)))
    (reveal-buffer
     (or
      (when (every #'numberp shapes) (apply #'max shapes))
      (when (every #'(lambda (x) (eql x 1)) shapes) 1)
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
	  (setf nrank (max nrank (apply #'max (map 'list #'buffer-nrank reads))))
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

(defun render-isl-aref (buffer &key (genid #'gid) (access-rep nil))
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
	     ;; Ugly solution... should be temporary...
	     (when (and (not (numberp stride)) access-rep) (setf stride 1))
	     (when (and (not (numberp by)) access-rep) (setf by 2))
	     (when (and (not (numberp upfrom)) access-rep) (setf upfrom 1))
	     (if broadcast-p
		 (format nil "~a" upfrom)
		 (format nil "~a(~a~a)"
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
	       (when (not (and (= 1 (length (graph-nodes subgraph))) (eql :Allocate (node-type (car (graph-nodes subgraph))))))			  
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
;; polyhedral compilation to determine the parallelization strategy
;; If we do; compile from avm into ISL, optimizng
;; This is the toplevel of all optimization stuff
(declaim (ftype (function (AVM &key (:verbose boolean)) (values Polyhedral)) create-polyhedral-model))
(defun create-polyhedral-model (avm &key (verbose nil))
  "Creates the polyhedral model given the avm."
  (declare (type avm avm) (type boolean verbose))
  (let* ((type-map (run-type-infer avm))
	 (recursive-top-ids (append (avm-fw-outputs avm))));; (avm-bw-outputs avm)))
    (when verbose
      (format t "== [Initial Graph] ==~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~ Optimizations ~~
    ;; Do not verify the graph; nodes used to compute views may lost.
    (deploy-type-infer-results avm type-map) ;; Let them include :VIEW node inside :_type_relay attrs
    (apply-jit-specific-simplifiers avm)     ;; WMMA Accumlation etc
    (when verbose
      (format t "== [Graph after applying jit-specific simplifiers] ==~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    (flet ((id->buffer (id)
	     (assert (symbolp id) () "Graph should not return a number!")
	     (list (id->value (avm-graph avm) id) (map/type-of type-map id) id)))
      (let* ((schedules (map 'list (compose #'make-scheduled-items #'id->buffer) recursive-top-ids))
	     (scheduled (reverse (flatten (map 'list #'(lambda (x) (recursive-find-group avm x)) schedules)))))
	;; verify-graph assets no duplication in branches from recursive-top-ids
	(loop for nth upfrom 0
	      for s in scheduled
	      do (setf (si-name s) (intern (format nil "T~a" nth) "KEYWORD")))
	(when verbose
	  (format t "== [Graph after applying an initial scheduler process] ==~%")
	  (print-schedules scheduled))
	
	(let* ((graphs (map 'list #'schedule->submodule scheduled))
	       (pipeline (make-hash-table)))
	  ;; Pipeline: T_ID -> Submodule_Graph
	  (loop for nth upfrom 0
		for g in graphs
		do (setf (gethash nth pipeline) g))
	  (%simplify-pipeline pipeline recursive-top-ids)
	  (when verbose
	    (format t "== [Final Graph Before Applying Polyhedral Compiler] ======~%")
	    (print-pipeline pipeline))
	  ;; pipeline has all infos including
	  ;; Creates the initial problem:
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
	    (values (make-polyhedral avm pipeline domain read-access write-access schedule) vm-inputs)))))))

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
  (caller #'(lambda ()) :type function)
  (lang :nil :type keyword)
  (code "" :type string))
(defmethod print-object ((s jit-info) stream) (format stream "<~a Code>" (jit-info-lang s)))
(defun make-fused-kernel-caller (allocs lambda code lang)
  (make-node :IR :JIT_KERNEL
	     (apply #'append (map 'list #'node-writes allocs))
	     (apply #'append (map 'list #'node-writes allocs))
	     :jit-info (make-jit-info :caller lambda :lang lang :code code)))
(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((jit (getattr node :jit-info)))
    (assert (jit-info-p jit) () "~a is not a jit kernel. :jit-info=~a" node jit)
    (apply (jit-info-caller jit) args))
  (apply #'values args))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO LIST:
;;  TODO: making isl objects gc-reachable (-> ctxに紐付けておく)
;;  TODO: Symbolic Graph Compilation
;;      - Step1 Numberと同じようにCompileできるようにする (if not appeared in strides)
;;              - Testing: Stride計算がTensorでもOK?
;;      - Step2 (Strideの計算は適当な整数値(素数)で置き換える)
;;  FIX:  Bugs (more symbolic deps needed)
;;  ADD: METAL/OMP, parallelize dependencies analysis
;;  ADD: If/For Node in the early stage!!!!
;; 正しいコンパイル:
;;   Step1, Dynamic Shapeと入力変数のみを受け入れる
;;   Step2, tmpvarの振る舞い...
;;   Step3, JIT-CompiledのArgsのテスト (axpy, symbolic meanで検証)
;;   Aref ga buffer no toki overwrite???
	     ;; TODO: View計算もExprに含めたい (OK)
	     ;; 今やってないこと:
	     ;; apply-multiexpr-grouping無しでも動作するべき (:MULTIEXPR=1, CI Testに含める)
	     ;; Backward?
	     ;; RendererをRefactorする。aRI GraphのRenderingを廃止する？
	     ;; MULTIEXPR=0でテストを通すべきだと思う
	     ;; TODO: Ternary Ops %where
	     ;; 一時領域の判定ができると思う = (Allocationに宣言されてないUndefined Variable)
	     ;; Pipelineを跨いでWriteに依存はない？
	     ;; Esp: when creating backwards
	     ;; Write-toのUpdateがおかしい
	     ;; やること
	     ;; 1. Tanを動かす (ok ) -> Undefined-Varの処理を追加 (ok)
	     ;; 2. In-place-mutationをapply-memory-plannerにする (ok)
	     ;; 3. MULTIEXPR=1 or 0をCIに含める (no)
	     ;; 4. JIT-Compilation Backwardを実装
	     ;; 5. ^ 途中でMoveが含まれる時，うまく分割する
	     ;; Backward実装したら，int xxx = x[...];を実装
	     ;; JIT=0 JIT=1 でBackwardが同じかTestする
	     ;; (!tan (!matmul ...))のScheduler修正
(defun jit (avm
	    &key
	      (debug (ctx:getenv :JIT_DEBUG))
	      (serialize (= 1 (ctx:getenv :SERIALIZE)))
	      (static-gensym (= 1 (ctx:getenv :STATIC_GENSYM)))
	      (backend (or (ctx:getenv :JIT_BACKEND) :clang))
	      (multiexpr (= 1 (ctx:getenv :MULTIEXPR)))
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
    (multiple-value-bind (polyhedron dynamic-shapes)
	(create-polyhedral-model avm :verbose verbose-schedule)
      (auto-schedule! polyhedron :verbose verbose-auto :serialize serialize)
      (when (>= debug 2)
	(format t "~% == [Final Polyhedron] ====~%~a~%" polyhedron))
      ;; Polyhedral -> Renderer
      ;; Polyhedron supercedes :FOR/:ENDFOR, remove them.
      (remove-iteration-ir (poly-pipeline polyhedron))
      ;; Minimizing the number of allocation by creating an alias
      ;; After applying memory-planner, it breaks write-must-be-exist-once rule of aIR graph
      ;; so you cannot verify the graph!
      (let* ((extracted-schedule (finalize-schedule polyhedron))
	     (r-graph (create-rendering-graph polyhedron extracted-schedule))	     
	     (_ (apply-memory-planner (poly-pipeline polyhedron) avm r-graph :multiexpr multiexpr))
	     (allocs (purge-allocations (poly-pipeline polyhedron) dynamic-shapes))
	     (body (%render-body backend backend r-graph polyhedron 1))
	     (function (%render-function backend avm allocs body))
	     (function (%render-program-toplevel backend function))
	     (f (%render-function-caller backend avm allocs function)))
	(declare (ignore _))
	(assert (functionp f) () "%render-function-caller should return a function!")
	(when (>= debug 1)
	  (format t "Compiled:~%~a" function))
	(%render-compile backend avm allocs function)
	;; (isl-free-ctx )
	;; TODO: Further Simplificatin, tracing the avm, generate the C-exported VM
	;; Keep the consistency: JIT(JIT(model))
	(let* ((subgraph (apply #'append (map 'list #'(lambda (x) (get-subgraph-recursively x (avm-graph base-avm) dynamic-shapes (getattr x :dtype))) allocs)))
	       (new-graph (apply #'make-graph (append subgraph (list (make-fused-kernel-caller allocs f function backend))))))
	  ;;(fold-constant new-graph)
	  ;; TODO: Tracing the jit-compiled AVM (including IfNode/MapNode etc)
	  ;; we can export the entire vm to clang.
	  (make-avm
	   new-graph
	   (avm-name avm)
	   (avm-id2tensor avm)
	   (avm-fw-outputs avm)
	   (avm-bw-outputs avm)))))))
