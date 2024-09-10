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

;; TODO: Improve Memory_Planner (Support Statement and Symbolic)
;; TODO: Index Computation Simplification
;; TODO: out deps -> float mutation
;; TODO: Remove unused argument

(defun render-graph/get-timestamps (graph)
  (declare (type graph graph))
  (loop for node in (graph-nodes graph) if (eql (node-type node) :FUNCALL) collect (getattr node :idx)))

;; ~~ Simplifier ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun simplify-groups (groups)
  "Simplifies the given groups"
  (declare (type list groups))
  (mapc
   (compose
    #'group-apply-reduction
    #'group-apply-load-simplification)
   groups)
  groups)

(defmethod group-apply-reduction ((group Group))
  "Rewriting:
OUT <- f(x, y, reduction=t)
as
X <- f(x, y, reduction=t)"
  (loop for node in (graph-nodes (group-graph group))
	if (getattr node :reduction)
	  do (setf (car (node-writes node)) (car (node-reads node))))
  group)

(defmethod group-apply-load-simplification ((group Group))
  "Removing: val_1 <- val_1"
  group)
;; ~~ MemoryPlanner ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass MemoryPlanner ()
  ((groups :initarg :groups :initform nil :accessor mp-groups)
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

(defmethod initialize-instance :after ((mp MemoryPlanner) &key (groups nil) &aux (groups (simplify-groups groups)))
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
	  (avm-bw-outputs avm) (map 'list #'replacer (avm-bw-outputs avm))
	  ;; vm-inputs are fixed (they are dynamic shapes)
	  ;;(poly-vm-outputs polyhedral) (map 'list #'replacer (poly-vm-outputs polyhedral))
	  ;;(group-args group) (map 'list #'newid (group-args group))
	  )
    (macrolet ((renew (accessor)
		 `(let ((new-table (make-hash-table)))
		    (maphash
		     #'(lambda (k v)
			 (setf (gethash (replacer k) new-table) v))
		     ,accessor)
		    (setf ,accessor new-table))))
      (renew (avm-id2tensor avm))
      ;;(renew (poly-vm-io-types polyhedral))
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

(defun newid-from-str (obj) (if (stringp obj) (intern obj) obj))
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
    ;; Tensors firstly appeared in the read
    (loop
      for (name . type) in (nodes-depends-on/buffers nodes)
      for sv4bw-p = (is-sv4bw group name)
      for written-deps = (find name nodes :key #'node-writes :test #'find)
      for read-deps    = (find name nodes :key #'node-reads  :test #'find)
      do (setf (buffer-shape type) (map 'list #'reveal-buffer (buffer-shape type))
	       (buffer-shape type) (loop for s in (buffer-shape type)
					 for nth upfrom 0
					 for view = (nth nth (buffer-views type))
					 if (or (null view) (null (fourth view)))
					   collect s
					 else
					   collect 1))
      do (push
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
      with read-set = (map 'list #'node-reads nodes)
      for node in nodes
      for nth upfrom 0 do
	(loop for write in (node-writes node)
	      for type in (relay-writes (read-type-relay node))
	      if (null (find write (apply #'append (subseq read-set 0 nth)))) do
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
    out))

;; [TODO]
;; - env: MEMORY_PLANNER=MIP, HEURISTIC
;; Formulation
;; Implementation for https://arxiv.org/pdf/2210.12924
(defstruct (MemoryBlock
	    (:constructor make-memoryblock (id type create release)))
  "An abstraction for memory_block
    |
 i  |  (create)  (release)
 d  |     |----------| 
    |
-------------------------
   t i m e
MemoryBlock(id) is allocated when t=create, preserved until t become `release`.
"
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
  (and (created-p mb time) (not (preserved-p mb time))))
  
(defun SolveDSA (I total-time)
  "
An solver for statement.
I = {(s1, r1, c1), ..., (sn, rn, cn)}

s = length
projection(r1, c1) r1 -> c1
i.e.: I_n is associated with a node that requires s_n buffers, used from t=rn to t=cn.

objective: min Σ(area(r, c))

https://arxiv.org/pdf/1804.10001
https://arxiv.org/pdf/2210.12924
Section 3.2, Best-fit heuristic

Statement:
           T
     ┏━━━━━┓
   O ┃           ┃
     ┗━━━━━┛
   where O = offset, T = time

"
  (declare (type list I))
  ;; StatementのOffsetが低くなるようにi.e.: peak_mem_usageがなるべく低くなるように配置
  ;; Total_Size <= mo ok
  (let ((statement (make-array total-time :element-type 'fixnum :initial-element 0))
	(locked))
    (labels ((choose-from-fragments (mb time &aux (candidates))
	       ;; Without colliding
	       ;; With the longest lifetime
	       ;; With the lowest offset
	       ;; If there's nothing, allocate the new one.
	       (loop for candidate of-type MemoryBlock in I
		     if (and (null (find (memoryblock-id candidate) locked)) (freed-p mb time)
			     (equal (buffer-shape (memoryblock-type candidate))
				    (buffer-shape (memoryblock-type mb))))
		       do (push candidate candidates))
	       (flet ((use (x)
			(push (memoryblock-id x) locked)
			(return-from choose-from-fragments x)))
		 ;; Use
		 (when candidates
		   (use (car (sort candidates #'> :key #'memoryblock-lifetime))))))
	     (apply-creation (time)
	       (loop for mb of-type MemoryBlock in I
		     if (allocate-p mb time) do
		       (let ((buffer (choose-from-fragments mb time)))
			 (if buffer
			     (setf (memoryblock-answer mb) (memoryblock-id buffer))
			     (setf (memoryblock-answer mb) (memoryblock-id mb))))))
	     (apply-release (time)
	       (loop for mb of-type MemoryBlock in I
		     if (and (release-p mb time) (memoryblock-answer mb))  do
		       (setf locked (remove (memoryblock-answer mb) locked)))))
      (dotimes (time total-time)
	(apply-creation time)
	(apply-release time))
      I)))

(defmethod evaluate ((mp MemoryPlanner))
  ;; xxx MiB
  )

;; やること
;; - [ ] scalarにpropagateする
;; -
;; - Allocationの最適化
(defmethod memory-plan ((mp MemoryPlanner) avm)
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

We resort to MIP.3
Formulation:

First, nodes are eager to make themselve in-place
If making in-place strategy will corrupt the result of kernel, tries:
- Using cached and realized buffer. (out of lifespan)
- Allocating a new tensor, involving them into a stage.

"
  (let* ((trace-table (make-hash-table))
	 (id2type (make-hash-table)))
    (loop for node in (graph-nodes (mp-graph mp))
	  for nth upfrom 0 do
	    (loop for val in `(,@(node-reads node) ,@(node-writes node))
		  for typ in `(,@(relay-reads (read-type-relay node)) ,@(relay-writes (read-type-relay node)))
		  for time = `(,nth ,@(gethash val trace-table))
		  if (symbolp val)
		    do (setf (gethash val id2type) typ (gethash val trace-table) time)))
    (let* ((memory-blocks
	    (loop for key in (hash-table-keys trace-table)
		  for typ = (gethash key id2type)
		  collect
		  (make-memoryblock key typ (apply #'min (gethash key trace-table)) (apply #'max (Gethash key trace-table)))))
	   (solved (solveDSA memory-blocks (length (graph-nodes (mp-graph mp)))))
	   (alias-map (make-hash-table)))
      (loop for mb in solved
	    do (setf (gethash (memoryblock-id mb) alias-map) (memoryblock-answer mb)))
      (setf (mp-alias mp) alias-map)))
  
  (dolist (node (graph-nodes (mp-graph mp))) (mp-update-node mp node))
  (mp-update-avm mp avm)
  (loop for group in (mp-groups mp)
	for kernel in (mp-kernels mp)
	if kernel do (dolist (k kernel) (apply-current-plan mp group k))))

;; - [ ] 最初にIndexのIN/OUTをはっきりさせる
;;  - Reference_Countが尽きたIDを持って来たら自然とそうなる？
;; - [ ] それ以外はScalarにして良い
;;  - どのタイミングでやるか・・・
;; - [ ] ScalarもIn-place mutationのアルゴリズムで，重複を減らす
;; - [ ] IDを変更しないメリット: 時系列がはっきりする: expr-eqで重複する計算はLOADに書き換えることが可能
(defmethod retrive-kernels ((mp MemoryPlanner))
  ;; - Index計算のSimplify
  ;; - Index計算が変わる要素は以下に絞れる
  ;;   - 同じGroupのIndexはStrideが同じなら等しい (int32 idx0 = xxx;)を生成する
  ;;   - UnrollしたGroupのIndexはidx0+1を生成すればOK
  ;;   - Offset加算などはPattern Matcherでうまく対応
  ;;   - 重複したINdex計算を根絶できる
  ;; [TODO] 計算したIndexをHash-Tableに保存+文字列ベースでCache+Minimize
  ;; Unrollingしたときは？

  ;; 最終的なArgsと，Kernelのリストを生成する
  (loop for group in (mp-groups mp)
	for kernels in (mp-kernels mp)
	if (group-realize-on-vm group)
	  collect group
	else	  
	  collect
	  (loop for k in kernels
		if (kernel-renderer-nodes k)
		  collect (pack-loop-funcall k (group-polyhedron group) (device-packed-by (mp-device mp))))))

