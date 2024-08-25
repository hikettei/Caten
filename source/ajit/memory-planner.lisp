(in-package :caten/ajit)

(defun render-graph/get-timestamps (graph)
  (declare (type graph graph))
  (loop for node in (graph-nodes graph)	if (eql (node-type node) :FUNCALL) collect (getattr node :idx)))

(defgeneric node/in-place-mutation (id node) (:documentation "Return a symbol indicating the position of output (chosen from node-reads)"))
(defmethod node/in-place-mutation :around (id node)
  (if (next-method-p)
      (call-next-method)
      (progn
	(warn "node/in-place-mutation for ~a is not defined, ignoring in-place-mutation opt." id)
	nil)))
(defmethod node/in-place-mutation ((id (eql :EXPR)) node) (car (node-reads node)))
(defmethod node/in-place-mutation ((id (eql :WMMA)) node) (car (node-reads node)))
  
;; ~~ reference-counter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
(defun id->memwrites (graph id)
  "Counts how many times id was read in the graph, and by who?"
  (declare (type graph graph) (type symbol id))
  (let ((count 0)
	(nodes))
    (loop for node in (graph-nodes graph)
	  for reads = (node-reads node) do
	    (loop for r in reads
		  if (eql r id) do (incf count) (push node nodes)))
    (values count (remove-duplicates nodes :key #'node-id))))
(defun create-reference-counter (groups)
  (declare (type list groups))
  (let ((refcount (make-hash-table))
	(refby (make-hash-table))
	(graph
	  (apply
	   #'make-graph
	   (loop for group in groups
		 unless (group-realize-on-vm group)
		   append
		   (loop for idx in (render-graph/get-timestamps (group-render-graph group))
			 append (graph-nodes (gethash idx (poly-pipeline (group-polyhedron group)))))))))
    (labels ((relevant-graph (pos) (apply #'make-graph (subseq (graph-nodes graph) pos)))
	     (inplace (node pos)
	       ;; (in-place-p . users)
	       (multiple-value-bind (count users) (id->memwrites (relevant-graph pos) (car (node-writes node)))
		 (cons count users))))
      (loop for node in (graph-nodes graph)
	    for pos upfrom 0
	    for (refc . refb) = (inplace node pos)
	    do (setf (gethash (car (node-writes node)) refcount) refc
		     (gethash (car (node-writes node)) refby) refb)))
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
      (let ((buffers (getattr node :buffers)))
	(assert (every #'(lambda (x) (find (expr-op x) `(:Const :Aref))) buffers))
	(mapc
	 #'(lambda (aref)
	     (setf (expr-x aref) (ref (expr-x aref))))
	 buffers)))
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

(defun apply-memory-planner! (group avm polyhedral refcount render-graph save-for-backwards)
  (declare (type avm avm) (type group group) (type polyhedral polyhedral) (type Reference-counter refcount)
	   (type graph render-graph) (type list save-for-backwards))
  (let* ((kernels (split-kernel (graph-nodes render-graph)))
	 (pipeline (poly-pipeline polyhedral))
	 (meta-ids))
    (labels ((inplace-p (node time)
	       ;; Return: (in-place-p . intersects-with-current-pipeline?)
	       (dolist (r (node-reads node))
		 (when (and (symbolp r) (gethash r (refcount-refcount refcount)))
		   (decf (gethash r (refcount-refcount refcount)))))
	       (when (eql (node-type node) :Allocate) (return-from inplace-p (cons t t)))
	       ;;(when (find (car (node-writes node)) save-for-backwards) (return-from inplace-p (cons nil nil)))
	       (let* ((id (or (node/in-place-mutation (node-type node) node)
			      (return-from inplace-p (cons nil nil))))
		      (refcount-n (gethash id (refcount-refcount refcount)))
		      (refdom     (gethash id (refcount-refcount-by refcount))))
		 (if refcount-n
		     (cons
		      (<= refcount-n 1)
		      (every #'(lambda (node) (find (node-id node) (graph-nodes (gethash time pipeline)) :key #'node-id)) refdom))
		     (cons t t))))
	     (newid (x) (refcount/refalias refcount x)))
      ;; O(nlogn) * the cost of id->users ...
      (loop
	for kernel in kernels
	for timestamps = (render-graph/get-timestamps (apply #'make-graph (kernel-renderer-nodes kernel))) do
	  (loop
	    for time in timestamps
	    for graph = (gethash time pipeline) do
	      (loop
		for node in (graph-nodes graph)
		for (inplace-p . all-exists-in-the-same-pipeline) = (inplace-p node time) do
		  (assert (= 1 (length (node-writes node))) ())
		  (refcount/make-alias refcount node inplace-p save-for-backwards)
		  (setf save-for-backwards (map 'list #'newid save-for-backwards))
		  ;; If write-to area is not going to be used by any other ops, let's make it in-place
		  ;; otherwise:
		  ;;  - If write-to-user exists in the same schedule -> create a tmpvar.
		  ;;  - If write-to-user exists in the another schedule -> they are save-for-backwards, lets keep them copying
		  (refcount/update-node node refcount)))
	  ;; save-for-backwardsはnewidする (OK)
	  ;; Argsを宣言するノードはどこに存在するか？-> 削除
	  ;; Stride/Shape/Loop_Bound計算に必要である計算ノード?
	  ;; Creating a final allocation information:
	  ;; Loadはval_にLoadしないで(書き込み以外) In-place Ruleに書き換えられるべきじゃね？
	  ;; save-for-backwardsはTimestampの単位で，ここはKernelの単位でも依存を確認する必要がある。
	  ;; Reduce AccumlationはVectorizeを実装してからやる？とりあえずこのPRではMemory-Plannerのみを考える
	  ;; In-place失敗するとUndefined Varを生成するけど，これはCache使いまわす処理を実装したいから，最後にやる
	  ;; ここら辺でIndex計算に使用したSymbolを列挙し，Depsに含める
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
				    collect (make-argument :name name
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
							   :metadata type)))
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
						:metadata type))))
		 (irs (loop for node in (kernel-renderer-nodes kernel)
			    if (find (node-type node) `(:FOR :IF))
			      collect node))
		 (index-components
		   (loop for node in (kernel-renderer-nodes kernel)
			 if (eql (node-type node) :FOR)
			   collect (intern (string-upcase (getattr node :idx)))))
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
				    for name = (newid (if (stringp dep) (intern (string-upcase dep)) dep))
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
				    for name = (newid (if (stringp dep) (intern (string-upcase dep)) dep))
				    unless (find name index-components)
				      do (push name meta-ids) and collect
					 (make-argument :name name :pointer-p nil :dtype *default-uint* :type :shape :io :input
							:metadata (make-buffer 0 nil nil *default-uint* nil))))))))
		 (kernel-args (remove-duplicates `(,@loop-args ,@(reverse buffer-args) ,@failed-inplace-list) :key #'argument-name)))
	    (dolist (node nodes)
	      (dolist (r (relay-reads (read-type-relay node)))
		(dolist (s (buffer-reconstruct-view-args r :except-for-shape t))
		  (when (and (symbolp s) (null (find s buffer-args :key #'argument-name)))
		    (push
		     (make-argument :name s :pointer-p nil :dtype *default-uint* :type :shape :io :input
				    :metadata (make-uconst-buffer))
		     kernel-args)
		    (push s meta-ids)))))
	    (setf (kernel-renderer-args kernel) kernel-args)))
      (remove-unused-kernels! kernels pipeline save-for-backwards meta-ids)
      ;; ここでArgsの判定 etc
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
      ;; 4. Scheduleの工夫で無理だったら手動でIfとかIfの中身を移動する
      ;; 5. VM/IfNodeを実装してAllocationをする (a < 100ならreuse, a >= 100ならKeep Usingみたいに)
      ;; TODO: TmpVar
      ;; loop_nodes_boundが実際Loopの計算に必要か？を検証する
      ;; Vectorize/Unroll/Tilingはどうやる？
      ;; KernelをCompileしたら，ここで書き換えを実行する
      ;; Update allocated-items
      ;; Loop Forを読む
      ;;  -> ISL GraphのSimplifyをする
      (flet ((replacer (x) (refcount/refalias refcount x)))
	(loop for g in (graph-nodes render-graph) do
	  (case (node-type g)
	    (:FOR
	     (expr-recursive-replace (getattr g :below) #'replacer)
	     (expr-recursive-replace (getattr g :upfrom) #'replacer)
	     (expr-recursive-replace (getattr g :by) #'replacer))
	    (:IF
	     (expr-recursive-replace (getattr g :condition) #'replacer)))))
      
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
      (loop for k in kernels if (kernel-renderer-nodes k) collect k))))

(defun group/apply-memory-planner! (group refcount)
  (loop for node in (graph-nodes (group-graph group))
	do (refcount/update-node node refcount))
  group)
