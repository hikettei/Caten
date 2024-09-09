(in-package :caten/ajit)
;; [Group -> Kernel]
;; Allocation Overlapping
;; - Schedules overlapped allocation
;; - Simplifies the index computation
;; 

(defun render-graph/get-timestamps (graph)
  (declare (type graph graph))
  (loop for node in (graph-nodes graph) if (eql (node-type node) :FUNCALL) collect (getattr node :idx)))

(defgeneric node/in-place-mutation (id node) (:documentation "Return a symbol indicating the position of output (chosen from node-reads)"))
(defmethod node/in-place-mutation :around (id node)
  (if (next-method-p)
      (call-next-method)
      (progn
	(warn "node/in-place-mutation for ~a is not defined, ignoring in-place-mutation opt." id)
	nil)))
(defmethod node/in-place-mutation ((id (eql :EXPR)) node) (car (node-reads node)))
(defmethod node/in-place-mutation ((id (eql :WMMA)) node) (car (node-reads node)))
;; ~~ Simplifier ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun simplify-groups (groups)
  "Simplifies the given groups"
  (declare (type list groups))
  (mapc
   (compose
    ;#'group-apply-reduction
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
   (realized :initform (make-hash-table))
   (on-stage :initform nil)
   (alias :initform (make-hash-table)))
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
	       unless (group-realize-on-vm group)
		 append
		 (loop for idx in (render-graph/get-timestamps (group-render-graph group))
		       append (graph-nodes (gethash idx (poly-pipeline (group-polyhedron group)))))))))

(defmethod ->argument ((mp MemoryPlanner) (group Group) (found-at (eql :node)) name (meta Buffer))

  )

(defmethod evaluate ((mp MemoryPlanner))
  ;; xxx MiB
  )
;; やること
;; - [x] :reductionがTrueの時，out_toは絶対にmyself
;; - [ ] scalarにpropagateする
;; -
;; - Allocationの最適化
(defmethod memory-plan ((mp MemoryPlanner))
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

We resort to MIP.
Formulation:

First, nodes are eager to make themselve in-place
If making in-place strategy will corrupt the result of kernel, tries:
- Using cached and realized buffer. (out of lifespan)
- Allocating a new tensor, involving them into a stage.

"
  ;; - [ ] 最初にIndexのIN/OUTをはっきりさせる
  ;; - [ ] それ以外はScalarにして良い
  ;; - [ ] ScalarもIn-place mutationのアルゴリズムで，重複を減らす
  ;; - [ ] IDを変更しないメリット: 時系列がはっきりする: expr-eqで重複する計算はLOADに書き換えることが可能
  )

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
		  collect (pack-loop-funcall (print k) (group-polyhedron group) (device-packed-by (mp-device mp))))))

(defstruct (Reference-Counter
	    (:conc-name refcount-))
  (alias (make-hash-table) :type hash-table)
  (refcount (make-hash-table) :type hash-table)
  (refcount-by (make-hash-table) :type hash-table)
  (tmpvars nil :type list))
(defmethod print-object ((r reference-counter) stream)
  (flet ((print-hash (obj)
	   (with-output-to-string (out)
	     (maphash #'(lambda (k v) (format out "~a -> ~a~%" k v)) obj))))
    (format stream "<Reference-Counter
Alias:
~a
Refcount:
~a
Refcount-by:
~a
>"
	    (print-hash (refcount-alias r))
	    (print-hash (refcount-refcount r))
	    (print-hash (refcount-refcount-by r)))))
(defun id->memwrites (graph self id)
  "Counts how many times id was read in the graph, and by who?"
  (declare (type graph graph) (symbol self) (type symbol id))
  (let ((count 0) (nodes))
    (loop for node in (graph-nodes graph)
	  for position = (node/in-place-mutation (node-type node) node)
	  if (and position (eql position id))
	    do (incf count) (push node nodes))
    (values count (remove-duplicates nodes :key #'node-id))))
(defun create-reference-counter (groups)
  (declare (type list groups))
  (let* ((refcount (make-hash-table))
	 (refby (make-hash-table))
	 (graph
	   (apply
	    #'make-graph
	    (loop for group in groups
		  unless (group-realize-on-vm group)
		    append
		    (loop for idx in (render-graph/get-timestamps (group-render-graph group))
			  append (graph-nodes (gethash idx (poly-pipeline (group-polyhedron group))))))))
	 (seen)
	 (all-ids (loop for x in (remove-duplicates
				  (nconc
				   (apply #'append (map 'list #'node-reads (graph-nodes graph)))
				   (apply #'append (map 'list #'node-writes (graph-nodes graph)))))
			if (symbolp x) collect x)))
    (labels ((inplace (node-id id)
	       ;; (in-place-p . users)
	       (multiple-value-bind (count users) (id->memwrites graph node-id id)
		 (cons count users))))
      (loop for node in (graph-nodes graph) do
	(loop for id in (node-writes node)
	      for (refc . refb) = (inplace (node-id node) id)
	      do (setf (gethash id refcount) refc
		       (gethash id refby) refb)
		 (push id seen)))
      ;; If the top is Allocation, never appeared in the graph.
      (loop for id in all-ids
	    if (null (find id seen))
	      do (let ((out (inplace nil id)))
		   (setf (gethash id refcount) (car out)
			 (gethash id refby) (cdr out)))))
    (make-reference-counter :refcount refcount :refcount-by refby)))

(defun refcount/refalias (count id)
  (let ((val (gethash id (refcount-alias count))))
    (if val
	(if (eql id val)
	    val
	    (refcount/refalias count val))
	id)))

(defun refcount/make-alias (count node in-place save-for-backwards)
  (if (eql (node-type node) :Allocate)
      (setf (gethash (car (node-writes node)) (refcount-alias count)) (car (node-writes node)))
      (if in-place
	  (let ((id (node/in-place-mutation (node-type node) node)))
	    (if (find id save-for-backwards)
		nil
		(when id (setf (gethash (car (node-writes node)) (refcount-alias count)) id))))
	  (setf (gethash (car (node-writes node)) (refcount-alias count)) (car (node-writes node))))))

(defun refcount/update-buffer (count buffer)
  (flet ((new (x) (refcount/refalias count (reveal-buffer x))))
    (setf (buffer-shape buffer) (map 'list #'new (buffer-shape buffer))
	  (buffer-stride buffer) (map 'list #'new (buffer-stride buffer))
	  (buffer-views buffer)
	  (loop for v in (buffer-views buffer)
		if v
		  collect (list (new (nth 0 v)) (new (nth 1 v)) (new (nth 2 v)) (nth 3 v))
		else
		  collect v))))

(defun refcount/update-node (node refcount)
  (declare (type node node))
  (flet ((ref (x) (refcount/refalias refcount x)))
    (when (eql (node-type node) :EXPR)
      (flet ((replacer (x) (refcount/refalias refcount x)))
	(expr-recursive-replace (getattr node :expr) #'replacer)))
    ;;(assert (null (getattr node :_reads)))
    ;;(assert (null (getattr node :_writes)))
    (when (getattr node :_type_relay)
      (map 'list #'(lambda (x) (when x (refcount/update-buffer refcount x))) `(,@(relay-writes (read-type-relay node)) ,@(relay-reads (read-type-relay node)))))
    (setf (getattr node :_loop_bound_nodes) (map 'list #'ref (getattr node :_loop_bound_nodes))
	  (getattr node :_reads)  (node-reads node)
	  (getattr node :_writes) (node-writes node)
	  (node-reads node) (map 'list #'ref (node-reads node))
	  (node-writes node) (map 'list #'ref (node-writes node)))))

(defun remove-unused-kernels! (kernels pipeline save-for-backwards seen-by-rendering-graph)
  (declare (type list kernels save-for-backwards)
	   (type hash-table pipeline))
  (let ((seen (loop for kernel in kernels
		    collect
		    (loop for time in (render-graph/get-timestamps (apply #'make-graph (kernel-renderer-nodes kernel)))
			  append
			  (loop for node in (graph-nodes (gethash time pipeline))
				append
				(node-reads node))))))
    (labels ((not-used-p (val nth)
	       (declare (type (or symbol number) val))
	       (if (numberp val)
		   nil
		   (if (or (find val save-for-backwards) (find val seen-by-rendering-graph))
		       nil
		       (<= (count val (the list (apply #'append (nthcdr nth seen)))) 1))))
	     (timestamp-not-used-p (graph nth)
	       (every #'(lambda (x) (not-used-p x nth))
		      (apply #'append (map 'list #'(lambda (x) (append (node-writes x) (node-reads x))) (graph-nodes graph)))))
	     (kernel-not-used-p (kernel nth)
	       (every
		#'identity
		(loop for time in (render-graph/get-timestamps (apply #'make-graph (kernel-renderer-nodes kernel)))
		      if (timestamp-not-used-p (gethash time pipeline) nth)
			collect
			(progn
			  ;; 1. Remove the unused pipeline
			  ;; 2. Calls simplifier
			  (setf (kernel-renderer-nodes kernel)
				(remove time (kernel-renderer-nodes kernel) :key #'(lambda (x) (and (eql (node-type x) :FUNCALL) (getattr x :idx))))
				(kernel-renderer-nodes kernel)
				(simplify-rendering-nodes (kernel-renderer-nodes kernel)))
			  t)
		      else
			collect nil))))
      (setf kernels
	    (loop for kernel in kernels
		  for nth upfrom 0
		  unless (kernel-not-used-p kernel nth)
		    collect kernel)))
    (let ((seen
	    (remove-duplicates
	     (append
	      seen-by-rendering-graph
	      (loop for kernel in kernels
		    append
		    (loop for time in (render-graph/get-timestamps (apply #'make-graph (kernel-renderer-nodes kernel)))
			  append
			  (loop for node in (graph-nodes (gethash time pipeline))
				append (node-writes node)
				append (node-reads node))))))))
      (loop for k in kernels do
	(setf (kernel-renderer-args k)
	      (loop for arg in (kernel-renderer-args k)
		    if (or (find (argument-name arg) seen) (find (argument-name arg) seen-by-rendering-graph))
		      collect arg))))))

(defun apply-memory-planner! (group avm polyhedral refcount render-graph save-for-backwards device id2buffer)
  (declare (type avm avm) (type group group) (type polyhedral polyhedral) (type Reference-counter refcount)
	   (type graph render-graph) (type list save-for-backwards))
  (let* ((kernels (render-graph-from-polyhedral polyhedral (graph-nodes render-graph)))
	 (pipeline (poly-pipeline polyhedral))
	 (meta-ids))
    (labels ((inplace-p (node time)
	       ;; Return: (in-place-p . intersects-with-current-pipeline?)
	       (when (eql (node-type node) :Allocate) (return-from inplace-p (cons t t)))
	       ;;(when (find (car (node-writes node)) save-for-backwards) (return-from inplace-p (cons nil nil)))
	       (let* ((id (or (car (node-writes node));;(node/in-place-mutation (node-type node) node)
			      (return-from inplace-p (cons nil nil))))
		      (refcount-n (gethash id (refcount-refcount refcount)))
		      (refdom     (gethash id (refcount-refcount-by refcount))))
		 (when (numberp refcount-n)
	  	   (assert (>= refcount-n 0) () "refcount-n should not be a negative! ~a" refcount-n))
		 ;;(format t "~%_____~%~a -> ~a;~%~a~%" id (car (node-reads node)) refcount-n)
		 (if refcount-n
		     (cons
		      nil;(<= refcount-n 1)
		      (every #'(lambda (node) (find (node-id node) (graph-nodes (gethash time pipeline)) :key #'node-id)) refdom))
		     (cons t t))))
	     (newid (x) (refcount/refalias refcount x))
	     (newid-from-str (x)
	       (if (stringp x)
		   (newid
		    (or (find x (poly-vm-inputs polyhedral) :test #'equalp :key #'symbol-name)
			(intern x)))
		   (newid x)))
	     (defbuffer (id buffer)
	       (when (null (gethash id id2buffer))
		 (setf (gethash id id2buffer) buffer)))
	     (refbuffer (id buffer)
	       (or (gethash id id2buffer) buffer)))
      ;; O(nlogn) * the cost of id->users ...
      (loop for time in (sort (render-graph/get-timestamps render-graph) #'<)
	    for graph = (gethash time pipeline) do
	      (loop
		for node in (graph-nodes graph)
		for (inplace-p . all-exists-in-the-same-pipeline) = (inplace-p node time) do
		  (assert (= 1 (length (node-writes node))) ())
		  (refcount/make-alias refcount node inplace-p save-for-backwards)
		  (setf save-for-backwards (map 'list #'newid save-for-backwards))
		  ;; If write-to area is not going to be used by any other ops, let's make it in-place
		  (refcount/update-node node refcount)))
      (loop for kernel in kernels
	    for timestamps = (render-graph/get-timestamps (apply #'make-graph (kernel-renderer-nodes kernel))) do
	      (let* ((nodes (apply #'append (map 'list #'(lambda (x) (graph-nodes (gethash x pipeline))) timestamps)))
		     (buffer-args (loop for (name . type) in (nodes-depends-on/buffers nodes)
					for only-used-in-this-kernel-p = (find name save-for-backwards)
					for written = (find name nodes :key #'node-writes :test #'find)
					for read    = (find name nodes :key #'node-reads  :test #'find)
					do (setf (buffer-shape type) (map 'list #'reveal-buffer (buffer-shape type))
						 (buffer-shape type) (loop for s in (buffer-shape type)
									   for nth upfrom 0
									   for view = (nth nth (buffer-views type))
									   if (or (null view) (null (fourth view)))
									     collect s
									   else
									     collect 1))
					collect (make-argument :name (progn
								       (defbuffer name type)
								       name)
							       :pointer-p (if (= (buffer-nrank type) 0)
									      (if written t nil)
									      t)
							       :dtype (buffer-dtype type)
							       :type (if (null (find name save-for-backwards))
									 :tmp ;; potentially can be rewritten as a float _tmp_xxx
									 :user)
							       :io (if (and written read)
								       :io
								       (if written
									   :output
									   :input))
							       :metadata (refbuffer name type))))
		     (failed-inplace-list
		       (loop with read-set = (map 'list #'node-reads nodes)
			     for node in nodes
			     for nth upfrom 0
			     append
			     (loop for write in (node-writes node)
				   for type in (relay-writes (read-type-relay node))
				   if (null (find write (apply #'append (subseq read-set 0 nth))))
				     collect
				     (make-argument :name write
						    :pointer-p t
						    :dtype (buffer-dtype type)
						    :type :tmp
						    :io :output
						    :metadata (refbuffer write type)))))
		     (irs (loop for node in (kernel-renderer-nodes kernel)
				if (find (node-type node) `(:FOR :IF))
				  collect node))
		     (index-components
		       (loop for node in (kernel-renderer-nodes kernel)
			     if (eql (node-type node) :FOR)
			       collect (newid-from-str (getattr node :idx))))
		     (loop-args
		       (loop for ir in irs
			     append
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
				      do (push name meta-ids) and collect
					 (make-argument :name name :pointer-p nil :dtype *default-uint* :type :shape :io :input
							:metadata (make-buffer 0 nil nil *default-uint* nil)))))
			   (:IF
			    (let ((deps
				    (remove-duplicates
				     (expr-recursive-deps (getattr ir :condition)))))
			      (loop for dep in deps
				    for name = (newid-from-str dep)
				    unless (find name index-components)
				      do (push name meta-ids) and collect
					 (make-argument :name name :pointer-p nil :dtype *default-uint* :type :shape :io :input
							:metadata (make-buffer 0 nil nil *default-uint* nil))))))))
		 (kernel-args `(,@loop-args ,@(reverse buffer-args) ,@failed-inplace-list)))
	    (dolist (node nodes)
	      (dolist (r (relay-reads (read-type-relay node)))
		(dolist (s (buffer-reconstruct-view-args r :except-for-shape t))
		  (when (and (symbolp s) (null (find s buffer-args :key #'argument-name)))
		    (push
		     (make-argument :name s :pointer-p nil :dtype *default-uint* :type :shape :io :input
				    :metadata (make-uconst-buffer))
		     kernel-args)
		    (push s meta-ids)))))
	    (setf (kernel-renderer-args kernel) (remove-duplicates kernel-args :key #'argument-name))
	    ;;(assert (equal (map 'list #'argument-name (kernel-renderer-args kernel))
	    ;;		   (map 'list (compose #'newid #'argument-name) (kernel-renderer-args kernel)))
	    ;;	    ()
	    ;;	    "There are inconsistencies in the time-series dependencies. ~a" (map 'list #'argument-name (kernel-renderer-args kernel)))
	    ))
      (remove-unused-kernels! kernels pipeline save-for-backwards meta-ids)
      ;; Reduction
      ;;   Reduce  [1, 2, 3] -> [1]
      ;;   Scatter [1] -> [1 2 3]
      ;; TODO: tmpvar optimization
      ;; [TODO]
      ;; Minimizing the number of allocations by following the rule:
      ;; 1. (car reads) becomes write, (except for %WHERE)
      ;;  | -   A <- f(B, C, D)
      ;;  | ->  B <- f(B, C, D)
      ;; 2. If (car reads) is duplicated in the graph, allocates tmpvar e.g.:
      ;;  | -   A1 <- f(A, B)
      ;;  | -   A2 <- f(A, C)
      ;;  | -   O1 <- f(A1, A2)
      ;;   ->
      ;;  | -   At1 <- f(A, B)
      ;;  | -   At2 <- f(A, C)
      ;;  | -   O1 <-  f(At1, At2)
      ;;  Where At1, At2 is a scalar value, being rendered `float _val_0_0` in clang.
      (flet ((replacer (x) (refcount/refalias refcount x)))
	(loop for g in (graph-nodes render-graph) do
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
		 (expr-recursive-replace x #'replacer)))))))
      
      (setf (avm-fw-outputs avm) (map 'list #'newid (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'newid (avm-bw-outputs avm))
	    ;; vm-inputs are fixed (they are dynamic shapes)
	    (poly-vm-outputs polyhedral) (map 'list #'newid (poly-vm-outputs polyhedral))
	    (group-args group) (map 'list #'newid (group-args group)))

      (macrolet ((renew (accessor)
		   `(let ((new-table (make-hash-table)))
		      (maphash
		       #'(lambda (k v)
			   (setf (gethash (newid k) new-table) v))
		       ,accessor)
		      (setf ,accessor new-table))))
	(renew (avm-id2tensor avm))
	(renew (poly-vm-io-types polyhedral))
	(renew (avm-variables avm)))
      
      (loop for k in kernels if (kernel-renderer-nodes k)
	    collect (pack-loop-funcall k polyhedral (device-packed-by device))))))

(defun group/apply-memory-planner! (group refcount)
  (loop for node in (graph-nodes (group-graph group))
	do (refcount/update-node node refcount))
  group)
