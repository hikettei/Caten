(in-package :caten/ajit)
;; memory-planner.lisp:
;; Lowers Group into Kernel-Renderer-Graph
;; Also, it has following optimizations:
;; - 1. Allocation Overlapping
;; - 2. Schedules overlapped allocation
;; - 3. Index Computation Scheduling
;; - 4. Duplicated computation elimination
;; - 5. Dead Code Elimination
;; reading: (TODO: Githubから持ってくる)
;; - https://discuss.tvm.apache.org/t/discussion-alignment-memory-planning/9730
;; - https://dl.acm.org/doi/pdf/10.5555/314500.315082

;; TODO: Improve Memory_Planner (Support Statement and Symbolic) (OK)
;; TODO: 必要ないカーネルはCompileしない (OK)
;; TODO: out deps -> float mutation (カーネル間の依存を考えればOK)
;;     - カーネル間に依存がないOUTPUTはscalarにする
;; TODO: Index Computation Simplification
;;     - Latency Optimization
;;     - Minimize the load
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

(defmethod initialize-instance :after ((mp MemoryPlanner) &key (groups nil) &aux (groups (simplify-groups mp groups)))
  (setf (mp-groups mp) groups
	(mp-kernels mp) (map 'list #'->render-graph groups)
	(mp-graph mp)
	(apply
	 #'make-graph
	 (loop for group in groups
	       if (group-realize-on-vm group)
		 append (graph-nodes (group-graph group))
	       else
		 append
		 (loop for idx in (render-graph/get-timestamps (group-render-graph group))
		       append (graph-nodes (gethash idx (poly-pipeline (group-polyhedron group)))))))))

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
	if (getattr node :reduction) do
	  (let ((from (car (node-writes node)))
		(to   (car (node-reads1 node))))
	    (setf (gethash from (mp-alias mp)) to
		  (car (node-writes node)) to)))
  group)

(defmethod group-apply-load-simplification ((group Group))
  "Removing: val_1 <- val_1"
  group)

(defmethod mp-newid ((mp MemoryPlanner) id) (or (gethash id (mp-alias mp)) id))
(defmethod mp-update-buffer ((mp MemoryPlanner) buffer)
  (flet ((new (x) (mp-newid mp x)))
    (setf (buffer-shape buffer) (map 'list #'new (buffer-shape buffer))
	  (buffer-stride buffer) (map 'list #'new (buffer-stride buffer))
	  (buffer-views buffer)
	  (loop for v in (buffer-views buffer)
		if v
		  collect (list (new (nth 0 v)) (new (nth 1 v)) (new (nth 2 v)) (nth 3 v))
		else
		  collect v))))

(defmethod mp-update-node ((mp MemoryPlanner) node)
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
  (apply
   #'append
   (map
    'list
    #'(lambda (time) (graph-nodes (gethash time (poly-pipeline (group-polyhedron group)))))
    (render-graph/get-timestamps (apply #'make-graph (kernel-renderer-nodes kr))))))

(defmethod renderer-get-irs ((kr kernel-renderer))
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

(defun newid-from-str (obj) (if (stringp obj) (intern (string-upcase obj)) obj))
(defmethod apply-current-plan ((mp MemoryPlanner) (group group) (kernel kernel-renderer))
  "Applying the current memory-plan, returning a list of arguments for the given group/kernel"
  (let* ((nodes (renderer-get-nodes group kernel))
	 (irs   (renderer-get-irs kernel))
	 (index-components
	   (loop for ir in irs
		 if (eql (node-type ir) :FOR)
		   collect (newid-from-str (getattr ir :idx))))
	 (meta-ids)
	 (out))
    (flet ((cleanup-buffer (buffer)
	     (setf (buffer-shape buffer) (map 'list #'reveal-buffer (buffer-shape buffer))
		   (buffer-shape buffer) (loop for s in (buffer-shape buffer)
					       for nth upfrom 0
					       for view = (nth nth (buffer-views buffer))
					       if (or (null view) (null (fourth view)))
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
		   for name = (newid-from-str dep)
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
		   for name = (newid-from-str dep)
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
	    (:constructor make-memoryblock (id type create release)))
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
  (lifetime (- release create) :type (integer 0)))

(defmethod print-object ((mb MemoryBlock) stream)
  (format stream "MemoryBlock(~(~a~) -> ~(~a~)) : (~a, ~a, ~a)~%" (memoryblock-id mb) (memoryblock-answer mb) (buffer-shape (memoryblock-type mb)) (memoryblock-create mb) (memoryblock-release mb)))

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

;; [TODO] Assuming the entire graph is "static", applying the `best-fit` schedule
;; [TODO] If the entire graph is static, use BestFitHeuristicDSA, otherwise use GREEDY
;; Env: GREEDY=1 to alywas use greedy solver.
;; Paper: Best Heuristic https://arxiv.org/pdf/1804.10001
(defun GreedySolveDSA (I total-time)
  "A greedy solver for minimizing peak_mem"
  (declare (type list I))
  (let ((locked))
    (labels ((choose-from-fragments (mb time &aux (candidates nil))
	       (loop for candidate of-type MemoryBlock in I
		     if (and (null (find (memoryblock-id candidate) locked))
			     (freed-p candidate time)
			     ;; [TODO] Is the shape computed from Viewed or Original?
			     (equal (buffer-shape (memoryblock-type candidate))
				    (buffer-shape (memoryblock-type mb)))
			     (equal (buffer-dtype (memoryblock-type candidate))
				    (buffer-dtype (memoryblock-type mb))))
		       do (push candidate candidates))
	       (flet ((use (x)
			(push (memoryblock-id x) locked)
			(return-from choose-from-fragments x)))
		 (when candidates (use (car (sort candidates #'> :key #'memoryblock-lifetime))))))
	     (apply-creation (time)
	       (loop for mb of-type MemoryBlock in I
		     if (allocate-p mb time) do
		       (let ((buffer (choose-from-fragments mb time)))
			 (if buffer
			     (setf (memoryblock-answer mb) (memoryblock-id buffer))
			     (setf (memoryblock-answer mb) (memoryblock-id mb))))))
	     (apply-release (time)
	       (loop for mb of-type MemoryBlock in I
		     if (and (release-p mb time) (memoryblock-answer mb)) do
		       (setf locked (remove (memoryblock-answer mb) locked)))))
      (dotimes (time total-time)
	(apply-release time)
	(apply-creation time))
      I)))

(defmethod evaluate ((mp MemoryPlanner))
  ;; xxx MiB
  )

(defmethod memory-plan ((mp MemoryPlanner) &aux (avm (mp-avm mp)))
  "Applies memory-optimization for the graph.
Resourses:
- https://arxiv.org/pdf/2203.00448
- https://arxiv.org/abs/1804.10001
Goal: overlapping the lifespan, e.g.:
Lifespan:
 |    T1
 |  a----b   T2
 |       c----d
 |
-------------------
- T1 exists from t=a to t=b.
- T2 exists from t=c to t=d.
- T1 and T2 are orthogonal.

(TODO: Update the description)
Formulation:

First, nodes are eager to make themselve in-place
If making in-place strategy will corrupt the result of kernel, tries:
- Using cached and realized buffer. (out of lifespan)
- Allocating a new tensor, involving them into a stage.
"
  (flet ((sync ()
	   (dolist (node (graph-nodes (mp-graph mp))) (mp-update-node mp node))
	   (dolist (g (mp-groups mp))
	     (when (group-realize-on-vm g)
	       (dolist (node (graph-nodes (group-graph g)))
		 (mp-update-node mp node))))
	   (mp-update-avm mp avm)))
    (sync)
    (let* ((trace-table (make-hash-table))
	   (id2type (make-hash-table))
	   (total-time (length (graph-nodes (mp-graph mp))))
	   (outputs (avm-bw-outputs avm))
	   (constants))
      (loop for node in (graph-nodes (mp-graph mp))
	    for nth upfrom 0 do
	      ;; Not going to make the dynamic shape in-place.
	      (when (and (eql (node-type node) :Load) (symbolp (getattr node :value)))
		(push (getattr node :value) constants))
	      (loop for val in (node-reads1 node)
		    for typ in (relay-reads1 node)
		    for time = `(,nth ,@(gethash val trace-table))
		    if (and (symbolp val) (null (find val constants)))
		      do (setf (gethash val id2type) typ (gethash val trace-table) time))
	      (loop for val in (node-writes node)
		    for typ in (relay-writes (read-type-relay node))
		    if (and (symbolp val) (null (gethash val trace-table)))
		      do (setf (gethash val id2type) typ (gethash val trace-table) (list nth))))
      (let* ((memory-blocks
	       (loop for key in (hash-table-keys trace-table)
		     for typ = (gethash key id2type)
		     collect
		     (make-memoryblock
		      key typ
		      (apply #'min (gethash key trace-table))
		      ;; Set the longest time for gradients
		      (if (find key outputs)
			  total-time
			  (apply #'max (gethash key trace-table))))))
	     (solved (GreedySolveDSA memory-blocks total-time))
	     (alias-map (mp-alias mp)))
	(loop for mb in solved
	      do (setf (gethash (memoryblock-id mb) alias-map) (or (memoryblock-answer mb) (memoryblock-id mb))))
	(setf (mp-alias mp) alias-map)))
    (sync)
    (loop for group in (mp-groups mp)
	  for kernel in (mp-kernels mp)
	  if kernel do (dolist (k kernel) (apply-current-plan mp group k)))))

(defun remove-unused-kernels (groups kernels meta-id
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

(defmethod retrive-kernels ((mp MemoryPlanner))
  ;; - Index計算のSimplify
  ;; - Index計算が変わる要素は以下に絞れる
  ;;   - 同じGroupのIndexはStrideが同じなら等しい (int32 idx0 = xxx;)を生成する
  ;;   - UnrollしたGroupのIndexはidx0+1を生成すればOK
  ;;   - Offset加算などはPattern Matcherでうまく対応
  ;;   - 重複したINdex計算を根絶できる
  ;; [TODO] 計算したIndexをHash-Tableに保存+文字列ベースでCache+Minimize
  ;; Unrollingしたときは？
  (setf (mp-kernels mp) (remove-unused-kernels (mp-groups mp) (mp-kernels mp) (append (avm-fw-outputs (mp-avm mp)) (avm-bw-outputs (mp-avm mp)))))
  (optimize-memory-load mp)
  (loop for group in (mp-groups mp)
	for kernels in (mp-kernels mp)
	if (group-realize-on-vm group)
	  collect group
	else	  
	  collect
	  (loop for k in kernels
		if (kernel-renderer-nodes k)
		  collect (pack-loop-funcall k (group-polyhedron group) (device-packed-by (mp-device mp))))))
