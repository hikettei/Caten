(in-package :caten/ajit)
;; memory-planner.lisp:
;; Lowers Group into Kernel-Renderer-Graph
;; Also, it has following optimizations:
;; - 1. Allocation Overlapping
;; - 2. Schedules overlapped allocation
;; - 3. Index Computation Scheduling
;; - 4. Duplicated computation elimination
;; - 5. Dead Code Elimination
(defun render-graph/get-timestamps (graph)
  (declare (type graph graph))
  (loop for node in (graph-nodes graph) if (eql (node-type node) :FUNCALL) collect (getattr node :idx)))
;; ~~ MemoryPlanner ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass MemoryPlanner ()
  ((groups :initarg :groups :initform nil :accessor mp-groups)
   (avm :initarg :avm :accessor mp-avm)
   (device :initarg :device :accessor mp-device)
   (graph  :initform nil :accessor mp-graph)
   (kernels :initform nil :accessor mp-kernels)
   (debug :initarg :debug :initform 0 :accessor mp-debug)
   (alias :initform (make-hash-table) :accessor mp-alias)
   (id2buffer :initform (make-hash-table) :accessor mp-id2buffer))
  (:documentation "
# [class] MemoryPlanner
Schedules given groups allocation and index computation.
Slots:
- groups[list] groups to schedule
- debug[fixnum] debug level
"))

(defmethod debug-log ((mp MemoryPlanner) stream str &rest more)
  (when (>= (mp-debug mp) 1)
    (format stream "MemoryPlanner : ~a~%" (apply #'format nil str more))))

(defun apply-group-attr (nodes)
  (dolist (n nodes)
    ;; Set :_no_group_realize_on_vm=t not to involve VM variables MemoryPlanner
    (setf (getattr n :_no_group_realize_on_vm) t))
  nodes)

(defun groups->graph (groups)
  (apply
   #'make-graph
   (loop for group in groups
	 if (group-realize-on-vm group)
	   append (apply-group-attr (graph-nodes (group-graph group)))
	 else
	   append
	   (loop for idx in (render-graph/get-timestamps (group-render-graph group))
		 append (graph-nodes (gethash idx (poly-pipeline (group-polyhedron group))))))))

(defmethod initialize-instance :after ((mp MemoryPlanner) &key (groups nil) &aux (groups (simplify-groups mp groups)))
  (setf (mp-groups mp) groups
	(mp-kernels mp) (map 'list #'->render-graph groups)
	(mp-graph mp) (groups->graph groups)))

(defun simplify-groups (mp groups)
  "Simplifies the given groups"
  (declare (type list groups))
  (mapc
   (compose
    #'(lambda (x) (group-apply-reduction mp x))
    #'group-apply-load-simplification)
   groups)
  groups)

(defmethod group-apply-reduction ((mp MemoryPlanner) (group Group))
  "Rewriting:
OUT <- f(x, y, reduction=t)
as
X <- f(x, y, reduction=t)"
  (loop for node in (graph-nodes (group-graph group))
	if (getattr node :reduction :allow-undefined t) do
	  (let ((from (car (node-writes node)))
		(to   (car (node-reads1 node))))
	    (setf (gethash from (mp-alias mp)) to
		  (car (node-writes node)) to)))
  group)

(defmethod group-apply-load-simplification ((group Group))
  "Removing: val_1 <- val_1"
  group)

(defmethod mp-newid ((mp MemoryPlanner) id)
  "ID -> {UPDATED_ID}"
  (assert (or (numberp id) (symbolp id)) () "mp-newid: id should be a number or symbol. but got ~a(~a)" id (type-of id))
  (or (gethash id (mp-alias mp)) id))

(defmethod mp-update-buffer ((mp MemoryPlanner) buffer)
  "Synchornizes the buffer with the MemoryPlanner"
  (flet ((new (x) (mp-newid mp (reveal-buffer x))))
    (when (null (buffer-shape-base buffer))
      ;; Avoid applying duplicated mp-newid
      (setf (buffer-shape-base buffer) (copy-list (buffer-shape buffer))
	    (buffer-stride-base buffer) (copy-list (buffer-stride buffer))
	    (buffer-views-base buffer) (copy-list (buffer-views buffer))))	    
    (setf (buffer-shape buffer) (map 'list #'new (buffer-shape-base buffer))
	  (buffer-stride buffer) (map 'list #'new (buffer-stride-base buffer))
	  (buffer-views buffer)
	  (loop for v in (buffer-views-base buffer)
		if v
		  collect (list (new (nth 0 v)) (new (nth 1 v)) (new (nth 2 v)) (nth 3 v))
		else
		  collect v))))

(defmethod mp-update-node ((mp MemoryPlanner) node)
  "Synchornizes the node with the MemoryPlanner"
  (flet ((ref (x) (mp-newid mp x)))
    (when (eql (node-type node) :EXPR)
      (flet ((replacer (x) (mp-newid mp x)))
	(expr-recursive-replace (getattr node :expr) #'replacer)))
    (when (getattr node :_type_relay)
      (map
       'list
       #'(lambda (x) (when x (mp-update-buffer mp x)))
       `(,@(relay-writes (read-type-relay node)) ,@(relay-reads (read-type-relay node)))))
    (setf (getattr node :_loop_bound_nodes) (map 'list #'ref (getattr node :_loop_bound_nodes))
	  (getattr node :_reads)  (node-reads node)
	  (getattr node :_writes) (node-writes node)
	  (node-reads node) (map 'list #'ref (node-reads node))
	  (node-writes node) (map 'list #'ref (node-writes node)))))

(defmethod mp-update-avm ((mp MemoryPlanner) avm)
  "Synchornizes the AVM with the MemoryPlanner"
  (flet ((replacer (x) (mp-newid mp x)))
    (loop for kernels in (mp-kernels mp)
	  if kernels do
	    (dolist (k kernels)
	      (dolist (g (kernel-renderer-nodes k))
		(case (node-type g)
		  (:FOR
		   (expr-recursive-replace (getattr g :below) #'replacer)
		   (expr-recursive-replace (getattr g :upfrom) #'replacer)
		   (expr-recursive-replace (getattr g :by) #'replacer))
		  (:IF
		   (expr-recursive-replace (getattr g :condition) #'replacer))
		  (:FUNCALL
		   (when (getattr g :args)
		     (dolist (x (getattr g :args))
		       (expr-recursive-replace x #'replacer))))))))
    (setf (avm-fw-outputs avm) (map 'list #'replacer (avm-fw-outputs avm))
	  (avm-bw-outputs avm) (map 'list #'replacer (avm-bw-outputs avm)))
    (dolist (g (mp-groups mp))
      (setf (group-args g) (map 'list #'replacer (group-args g))))
    (macrolet ((renew (accessor)
		 `(let ((new-table (make-hash-table)))
		    (maphash
		     #'(lambda (k v)
			 (setf (gethash (replacer k) new-table) v))
		     ,accessor)
		    (setf ,accessor new-table))))
      (renew (avm-id2tensor avm))
      (renew (avm-variables avm)))))

(defmethod renderer-get-nodes ((group group) (kr kernel-renderer))
  "Returns a list of nodes in the order of the given kernel-renderer.
e.g.:
```
for (...)
  T0(i)
  T1(i)
T0(i+3)
```
=>
```
(append T0.nodes T1.nodes T0.nodes)
```
"
  (apply
   #'append
   (map
    'list
    #'(lambda (time) (graph-nodes (gethash time (poly-pipeline (group-polyhedron group)))))
    (render-graph/get-timestamps (apply #'make-graph (kernel-renderer-nodes kr))))))

(defmethod renderer-get-irs ((kr kernel-renderer))
  "Returns a list of nodes whose type is :FOR or :IF"
  (loop for node in (kernel-renderer-nodes kr)
	if (find (node-type node) `(:FOR :IF))
	  collect node))

(defmethod is-sv4bw ((group group) (id symbol))
  "AcrossTimeDep: a list of ids which lifetime won't finish in the current nodes."
  (not (null (find id (group-across-time-deps group)))))
;; ID2Buffer: Pointers loaded as x* can be loaded as x[0+0] in other kenels.
(defmethod mp-reg-buffer-type ((mp MemoryPlanner) id buffer)
  (when (null (gethash id (mp-id2buffer mp)))
    (setf (gethash id (mp-id2buffer mp)) buffer)))
(defmethod mp-get-buffer-type ((mp MemoryPlanner) id) (gethash id (mp-id2buffer mp)))

(defmethod newid-from-str ((group group) obj)
  (if (stringp obj)
      (or
       (find obj (poly-vm-inputs (group-polyhedron group)) :test #'equalp :key #'symbol-name)
       (intern obj))
      obj))

(defmethod apply-current-plan ((mp MemoryPlanner) (group group) (kernel kernel-renderer))
  "Applying the current memory-plan, returning a list of arguments for the given group/kernel.
It finalizes the global argument of the kernel.
In Caten IR, there is a two way to allocate a variable.
 |   VAR TYPE    | How to declare
-|---------------|------------------------------
 | DEFINE_GLOBAL | Allocate nodes
 | DEFINE_LOCAL  | Set (getattr expr :decl-type)
"
  (let* ((nodes (renderer-get-nodes group kernel))
	 (irs   (renderer-get-irs kernel))
	 (index-components
	   (loop for ir in irs
		 if (eql (node-type ir) :FOR)
		   collect (newid-from-str group (getattr ir :idx))))
	 (meta-ids)
	 (out))
    (flet ((cleanup-buffer (buffer)
	     (setf (buffer-shape buffer) (map 'list #'reveal-buffer (buffer-shape buffer))
		   (buffer-shape buffer) (loop for s in (buffer-shape buffer)
					       for nth upfrom 0
					       for view = (nth nth (buffer-views buffer))
					       if (or (null view) (null (nth 3 view)))
						 collect s
					       else
						 collect 1))))
      ;; Tensors firstly appeared in the read
      (loop
	for (name . type) in (nodes-depends-on/buffers nodes)
	for sv4bw-p = (is-sv4bw group name)
	for written-deps = (find name nodes :key #'node-writes :test #'find)
	for read-deps    = (find name nodes :key #'node-reads1  :test #'find)
	do (cleanup-buffer type)
	   (push
	    (make-argument :name (progn
				   (mp-reg-buffer-type mp name type)
				   name)
			   :pointer-p (if (= (buffer-nrank type) 0)
					  (if written-deps t nil)
					  t)
			   :dtype (buffer-dtype type)
			   :type (if sv4bw-p :user :tmp)
			   :io (if (and written-deps read-deps)
				   :io
				   (if written-deps :output :input))
			   :metadata (or (mp-get-buffer-type mp name) type))
	    out))
      ;; Tensors firstly appeared in the write. (a.k.a: failed-in-place-tensors)
      (loop
	with read-set = (map 'list #'node-reads1 nodes)
	for node in nodes
	for nth upfrom 0 do
	  (loop for write in (node-writes node)
		for type in (relay-writes (read-type-relay node))
		if (null (find write (apply #'append (subseq read-set 0 nth)))) do
		  (cleanup-buffer type)
		  (push
		   (make-argument :name write
				  :pointer-p t
				  :dtype (buffer-dtype type)
				  :type :tmp
				  :io :output
				  :metadata (or (mp-get-buffer-type mp write) type))
		   out)))
      ;; Tensor firstly appeared in the IRs (For, IF)
      (loop for ir in irs do
	(ecase (node-type ir)
	  (:FOR
	   (let ((deps
		   (remove-duplicates
		    (append
		     (expr-recursive-deps (getattr ir :upfrom))
		     (expr-recursive-deps (getattr ir :below))
		     (expr-recursive-deps (getattr ir :by))))))
	     (loop for dep in deps
		   for name = (newid-from-str group dep)
		   unless (find name index-components)
		     ;; Indices are created as default-uint
		     do (push name meta-ids)
			(push
			 (make-argument :name name :pointer-p nil :dtype *default-uint* :type :shape :io :input
					:metadata (make-buffer 0 nil nil *default-uint* nil))
			 out))))
	  (:IF
	   (let ((deps
		   (remove-duplicates
		    (expr-recursive-deps (getattr ir :condition)))))
	     (loop for dep in deps
		   for name = (newid-from-str group dep)
		   unless (find name index-components)
		     do (push name meta-ids)
			(push
			 (make-argument :name name :pointer-p nil :dtype *default-uint* :type :shape :io :input
					:metadata (make-buffer 0 nil nil *default-uint* nil))
			 out))))))
      ;; Gathering symbols used in the view computation
      (dolist (node nodes)
	(dolist (r (relay-reads (read-type-relay node)))
	  (dolist (s (buffer-reconstruct-view-args r :except-for-shape t))
	    (when (symbolp s)
	      (push
	       (make-argument :name s :pointer-p nil :dtype *default-uint* :type :shape :io :input
			      :metadata (make-uconst-buffer))
	       out)
	      (push s meta-ids)))))
      (setf out (remove-duplicates out :key #'argument-name))
      (when (>= (mp-debug mp) 1)
	;; TODO: Display memory-size
	)
      (setf (kernel-renderer-args kernel) out)
      out)))

(defstruct (MemoryBlock
	    (:constructor make-memoryblock (id type create release &key (lock nil))))
  "An abstraction for memory_block
    |
 i  |  (create)  (release)
 d  |     |----------| 
    |
-------------------------
   t i m e
MemoryBlock(id) is allocated when t=create, preserved until t become `release`."
  (id id :type symbol)
  (answer nil :type symbol)
  (type type :type buffer)
  (create create :type fixnum)
  (release release :type fixnum)
  (lifetime (- release create) :type (integer 0))
  (lock lock :type boolean))

(defmethod print-object ((mb MemoryBlock) stream)
  (format stream "MemoryBlock(~(~a~) -> ~(~a~)) : (~a, ~a, ~a, lock=~a)~%" (memoryblock-id mb) (memoryblock-answer mb) (buffer-shape (memoryblock-type mb)) (memoryblock-create mb) (memoryblock-release mb) (memoryblock-lock mb)))

(defmethod allocate-p ((mb MemoryBlock) time)
  (= time (memoryblock-create mb)))

(defmethod created-p ((mb MemoryBlock) time)
  (>= time (memoryblock-create mb)))

(defmethod preserved-p ((mb MemoryBlock) time)
  (< time (memoryblock-release mb)))

(defmethod release-p ((mb MemoryBlock) time)
  (= time (memoryblock-release mb)))

(defmethod freed-p ((mb MemoryBlock) time)
  (and (created-p mb time) (>= time (memoryblock-release mb))))

(defun buffer-orig-shape (buffer)
  "Returns a shape of the buffer, which is not VIEWED."
  (declare (type buffer buffer))
  (or
   (buffer-orig-buffer-shape buffer)
   (buffer-shape buffer)))

;; Currently, our memory-planner has a room for the further optimization.
;; [TODO] Assuming the entire graph is "static", applying the `best-fit` schedule.
;; [TODO] If the entire graph is static, use BestFitHeuristicDSA, otherwise use GREEDY.
;; Env: GREEDY=1 to alywas use greedy solver.
;; Paper: Best-Fit Heuristic https://arxiv.org/pdf/1804.10001
(defun greedy-solve-dsa (I total-time)
  "A greedy solver for minimizing `peak_mem`"
  (declare (type list I))
  (let ((locked))
    (labels ((choose-from-fragments (mb time &aux (candidates nil))
	       (loop for candidate in I
		     if (and (null (find (memoryblock-id candidate) locked))
			     (freed-p candidate time)
			     (buffer-shape (memoryblock-type mb)) ;; <=> assure the memory-block is a tensor
			     (equal (buffer-orig-shape (memoryblock-type candidate))
				    (buffer-orig-shape (memoryblock-type mb)))
			     (equal (buffer-dtype (memoryblock-type candidate))
				    (buffer-dtype (memoryblock-type mb)))
			     ;; [TODO] This condition can be more simplified? (!randn `(100 100)) is good to test this behaviour.
			     (equal (buffer-views (memoryblock-type candidate))
				    (buffer-views (memoryblock-type mb))))
		       do (push candidate candidates))
	       (flet ((use (x)
			(push (memoryblock-id x) locked)
			(return-from choose-from-fragments x)))
		 (when candidates (use (car (sort candidates #'> :key #'memoryblock-lifetime))))))
	     (apply-creation (time)
	       (loop for mb in I
		     if (allocate-p mb time) do
		       (let ((buffer (and (null (memoryblock-lock mb)) (choose-from-fragments mb time))))
			 (if buffer
			     (setf (memoryblock-answer mb) (memoryblock-id buffer))
			     (setf (memoryblock-answer mb) (memoryblock-id mb))))))
	     (apply-release (time)
	       (loop for mb in I
		     if (and (release-p mb time) (memoryblock-answer mb)) do
		       (setf locked (remove (memoryblock-answer mb) locked)))))
      (dotimes (time total-time)
	(apply-release time)
	(apply-creation time))
      I)))

(defmethod memory-plan ((mp MemoryPlanner) &aux (avm (mp-avm mp)))
  "This is a toplevel for memory-oriented optimization techniques.
Resourses:
- https://arxiv.org/pdf/2203.00448
- https://arxiv.org/abs/1804.10001
Goal: overlapping all the lifespan of the memory allocation, e.g.:
Lifespan:
 |    T1
 |  a----b   T2
 |       c----d
 |
-------------------
- T1 exists from t=a to t=b.
- T2 exists from t=c to t=d.
- T1 and T2 are orthogonal, and can be overlapped.
"
  (flet ((sync ()
	   (dolist (node (graph-nodes (mp-graph mp))) (mp-update-node mp node))
	   (mp-update-avm mp avm)))
    (sync)
    (let* ((trace-table (make-hash-table))
	   (id2type (make-hash-table))
	   (lock-table (make-hash-table))
	   (total-time (length (graph-nodes (mp-graph mp))))
	   (outputs (avm-bw-outputs avm))
	   (constants))
      (loop for node in (graph-nodes (mp-graph mp))
	    for nth upfrom 0
	    for lock-p = (getattr node :_no_group_realize_on_vm) do
	      ;; [Note] Not going to make the dynamic shape in-place.
	      ;; beucase they are constant (e.g.: const uint_32 a)
	      (when (and (eql (node-type node) :Load) (symbolp (getattr node :value)))
                ;; [Note] If there's a variable that do not want to be destructed, add them to the constants.
		(push (getattr node :value) constants))
	      (loop for val in (node-reads1 node)
		    for typ in (relay-reads1 node)
		    for time = `(,nth ,@(gethash val trace-table))
		    if (and (symbolp val) (null (find val constants)))
		      do (setf (gethash val id2type) typ (gethash val trace-table) time))
	      (loop for val in (node-writes node)
		    for typ in (relay-writes (read-type-relay node))
		    if (and (symbolp val) (null (gethash val trace-table)))
                      ;; ID2Type    -> the variable name and its type
                      ;; TraceTable -> the variable name and timestamps of the variable (when it's used)
                      ;; LockTable  -> Set T to lock (never become in-place)
		      do (setf (gethash val id2type) typ
			       (gethash val trace-table) (list nth)
			       (gethash val lock-table) lock-p)))
      (let* ((memory-blocks
	       (loop for key in (hash-table-keys trace-table)
		     for typ = (gethash key id2type)
		     collect
                     ;; [Note] A memory block lives in the range of [min{t}, max{t})
                     ;; Plus, If the same task (e.g.: T0(x) -> T1(x) -> T0(x+1)) is scheduled, the memory block lives from 0 to 2.
		     (make-memoryblock
		      key typ
		      (apply #'min (gethash key trace-table))
                      ;; Set the longest time for the output variables (not to destruct it, and users can see the result)
		      (if (find key outputs)
			  total-time
			  (apply #'max (gethash key trace-table)))
		      :lock (gethash key lock-table))))
             ;; Minimize the peak memory usage
	     (solved (greedy-solve-dsa memory-blocks total-time))
             ;; Retrive the solution. A hash table of OLD_MEMORY_ID -> NEW_MEMORY_ID
	     (alias-map (mp-alias mp)))
	(loop for mb in solved
	      do (setf (gethash (memoryblock-id mb) alias-map) (or (memoryblock-answer mb) (memoryblock-id mb))))
	(setf (mp-alias mp) alias-map)))
    (sync)
    (loop for group in (mp-groups mp)
	  for kernel in (mp-kernels mp) ;; update arguments
	  if kernel do (dolist (k kernel) (apply-current-plan mp group k)))))

(defun dead-kernel-elimination (groups kernels meta-id
				&aux (static-read
				      (append
				       meta-id
				       (loop for g in groups if (group-realize-on-vm g) append (group-args g)))))
  (labels ((f (ks nth
	       &aux
		 (subsequent-reads
		  (map 'list #'argument-name
		       (apply #'append
			      (map 'list #'kernel-renderer-args (apply #'append (nthcdr (1+ nth) kernels))))))
		 (fix `(,@static-read ,@subsequent-reads)))
	     (loop for kernel in ks
		   for pos upfrom 1
		   for deps = (append fix (map 'list #'argument-name (apply #'append (map 'list #'kernel-renderer-args (nthcdr pos ks)))))
		   unless (every #'(lambda (x) (null (find (argument-name x) deps))) (kernel-renderer-args kernel))
		     collect kernel))
	   (r (&aux (changed-p nil))
	     (loop for k in kernels
		   for nth upfrom 0
		   if k do
		     (let ((new (f k nth)))
		       (when (not (= (length new) (length k))) (setf changed-p t))
		       (setf (nth nth kernels) new)))
	     changed-p))
    (loop while (r))
    kernels))

(defmethod mp-auto-schedule! ((mp MemoryPlanner))
  (let ((polyhedrons
          (loop for kernel in (mp-kernels mp)
                for group  in (mp-groups mp)
                collect
                (loop for kr in kernel
                      collect
                      (group->polyhedral-group group kr)))))
    (dolist (p polyhedrons)
      ;; Final Chance to apply Loop Fusion
      ;; Kernrels with complicated memory access relations, like Matmul+Transpose, Conv are first fused here
      ;; Polyhedral Group is a result of splitting a group -> multiple group?
      (when (and p (every #'(lambda (x) (typep x 'Polyhedral-Auto-Scheduler)) p))
        (affine-fusion p)))

    ;; Tiling, Vectorizing, Parallelizing(CPU/GPU), Loop Fission here
    ;; [TODO] Apply the changes to mp-kernerls, mp-groups
    ))

(defmethod retrive-kernels ((mp MemoryPlanner))
  "Finalizes the result of memory-planner, retriving the final rendering-graph"
  (flet ((prune ()
           "Applies the dead code elimination"
	   (setf (mp-kernels mp) (dead-kernel-elimination (mp-groups mp) (mp-kernels mp) (append (avm-fw-outputs (mp-avm mp)) (avm-bw-outputs (mp-avm mp)))))))
    (prune)

    ;; [Note] Auto_Scheduler is *work in progress*
    ;; (mp-auto-schedule! mp)
    ;; (prune)
    
    ;; 1. Mutate output buffers as a scalar
    (optimize-memory-load mp)
    ;; 2. Hide Latency Optimization
    ;; - The arrays should be loaded at once
    ;; - In the last, storing the result.
    ;; [TODO] Add dead graph.nodes elimination here. ^ maybe produce unused ops.
    (loop for group in (mp-groups mp)
	  for kernels in (mp-kernels mp)
	  if (group-realize-on-vm group)
	    collect group
	  else
	    collect
	    (loop for k in kernels
		  if (kernel-renderer-nodes k)
		    collect (pack-loop-funcall k (group-polyhedron group) (device-packed-by (mp-device mp)))))
    ;; TODO: Simplify the index load for unrolled buffer produced above.
    ))

(defun memory-access-local-p (render-nodes id pipeline)
  "Returns T if the given id is accessed locally in the rendering-graph."
  (let ((search-key
          ;; A subject to search is the tasks which are related to the given id.
	  (loop for key in (sort (hash-table-keys pipeline) #'<)
		for graph = (gethash key pipeline)
		if (find id (graph-nodes graph) :key #'(lambda (x) (append (node-reads x) (node-writes x))) :test #'find)
		  collect key)))
    (flet ((position-of (id)
             (or
              (position id render-nodes :key #'(lambda (x) (and (eql (node-type x) :FUNCALL) (getattr x :idx))))
              (return-from memory-access-local-p nil))))
      ;; [TODO] Is it confirmed that the schedule task is in the order of 0, 1, 2, ...?
      ;; Potentially should produce an bug.
      (loop with depth = 0
	    with nodes = render-nodes
            with start = (position-of (apply #'min search-key))
            with end   = (position-of (apply #'max search-key))
	    for nth upfrom (min start end) to (max start end)
	    for ir = (nth nth nodes)
	    if (find (node-type ir) `(:IF :FOR))
	      do (incf depth)
	    else if (find (node-type ir) `(:ENDIF :ENDFOR))
	           do (decf depth)
	    end
	    if (< depth 0) do
	      (return-from memory-access-local-p nil))))
  t)

(defmethod output->scalar-mutation ((mp MemoryPlanner) (group group) (kernel kernel-renderer) dependency-list)
  "
Rewrites the graph:
for (...) {
  out[...] = f(...)
}
->
for (...) {
  out = f(...)
}
If the tensor `out` is labelled as :output by the memory-planner, and not appeared in the `dependency-list`.
"
  (let ((scalars))
    (flet ((->scalar-p (id)
	     (and
              ;; Tensors labelled as a :output
	      (let ((out (find id (kernel-renderer-args kernel) :key #'argument-name)))
		(and out (eql :output (argument-io out))))
              ;; Tensors not appeared in the dependency-list
	      (null (find id dependency-list))
              ;; Judge if the use of the tensor is local, by reading the rendering-graph.
	      (memory-access-local-p (kernel-renderer-nodes kernel) id (poly-pipeline (group-polyhedron group))))))
      ;; Create a list of buffers that can be optimized in this function.
      (loop for node in (graph-nodes (group-render-graph group))
            if (eql (node-type node) :FUNCALL) do
              (loop for node in (graph-nodes (gethash (getattr node :idx) (poly-pipeline (group-polyhedron group)))) do
                (loop for w in (node-writes node)
                      for typ in (relay-writes (read-type-relay node))
                      if (and (not (= 0 (buffer-nrank typ))) (->scalar-p w)) do
                        (push w scalars))))
      ;; mutate all read dependencies
      (let ((seen) (suffix))
	(loop for node in (kernel-renderer-nodes kernel)
	      if (eql (node-type node) :FOR)
		do (push (cons (getattr node :idx) 1) suffix)
	      else
		if (eql (node-type node) :ENDFOR)
		  do (setf suffix (remove (getattr node :idx) suffix :key #'car :test #'equalp))
	      end
	      if (eql (node-type node) :FUNCALL)
		do (let ((domain-space (getattr node :args)))
		     (dolist (node (graph-nodes (gethash (getattr node :idx) (poly-pipeline (group-polyhedron group)))))
		       (update-buffer-as-scalar node scalars domain-space)
		       (setf (getattr node :declare-type)
			     (loop for w in (node-writes node)
				   for typ in (relay-writes (read-type-relay node))
				   for w-as-unrolled = (intern (format nil "~a~a" w (unroll-suffix typ suffix)))
                                   for nth upfrom 0
				   collect
				   (prog1
                                       (or
                                        ;; If the same task was appeared more than twise times -> extend the first result.
                                        (nth nth (getattr node :declare-type))
				        (and (null (find w-as-unrolled seen)) (find w scalars)))
				     (push w-as-unrolled seen))))))))
      ;; Remove from args
      (setf (kernel-renderer-args kernel)
	    (loop for arg in (kernel-renderer-args kernel)
		  if (null (find (argument-name arg) scalars))
		    collect arg)))))

(defmethod optimize-memory-load ((mp MemoryPlanner))
  "Optimizes the memory latency by caching LOAD/STORE, and removes LOAD for unnecessary Variables"
  (with-slots ((groups groups) (kernels kernels) (avm avm)) mp
    (assert (= (length groups) (length kernels)))
    (let ((args-by-time
	    (loop for g in groups
		  for k in kernels
		  if k
		    collect (map 'list #'(lambda (x) (map 'list #'argument-name x)) (map 'list #'kernel-renderer-args k))
		  else
		    collect (list (group-args g))))
	  (const-dependencies (append (avm-fw-outputs avm) (avm-bw-outputs avm))))
      ;; Removes the :OUTPUT buffer (except for const-dependencies)
      (loop for g in groups
	    for k in kernels
	    for nth upfrom 0
	    if k do
	      (loop for kernel-renderer in k
		    for ith upfrom 1
		    for deps = (flatten
				(append
				 const-dependencies
                                 ;; [Note] If the output buffer is used by another kernels, that should be a matrix.
				 (apply #'append (nthcdr (1+ nth) args-by-time))
				 (apply #'append (nthcdr ith (nth nth args-by-time)))))
		    do (output->scalar-mutation mp g kernel-renderer deps))))))
