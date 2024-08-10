(in-package :caten/ajit)
;; Here, applying a jit-specific optimization to the avm.graph.
;; E.g.: we can purge the view nodes since we already have a
;; type information at `type-relay.lisp`.
;; TODO: Refactor around typing relay (esp: memory-write dependencies are messing)
(defun %safely-purge-views-from-graph (avm)
  "(1.) Removes :VIEW from avm graph, (2.) Updates the read/write graph relations"
  (declare (type avm avm))
  (let ((rewrite-map
	  (loop for node in (graph-nodes (avm-graph avm))
		if (eql (node-type node) :View)
		  collect (cons (car (node-reads node)) (car (node-writes node))))))
    (labels ((->find (id) (find id rewrite-map :key #'car :test #'eql))
	     (view-composed? (id &aux (node (id->value (avm-graph avm) id)))
	       (and node (eql (node-type node) :View)))
	     (->aft (id &aux (last id))
	       (if (symbolp id)
		   (labels ((f (x &aux (next (->find x)))
			      (if next
				  (progn
				    (setf last (cdr next))
				    (if (view-composed? (cdr next))
					(f (cdr next))
					last))
				  nil)))
		     (f last)
		     last)
		   id)))
      (setf (graph-nodes (avm-graph avm))
	    (loop for n in (graph-nodes (avm-graph avm))
		  unless (eql (node-type n) :View)
		    collect
		    (progn		      
		      (setf (node-writes n) (map 'list #'(lambda (x) (or (->aft x) x)) (node-writes n)))
		      n))))))

(defun wmma-relay-from (t1 tc nth)
  (make-inferred-type `(,(nth nth (relay-reads tc)) ,@(relay-reads t1)) (relay-writes tc)))
(defun wmma-relay-from1 (t1 t2 t3)
  (make-inferred-type `(,@(relay-writes t3) ,(second (relay-reads t1)) ,(second (relay-reads t2))) (relay-writes t3))
  (setf (nth 1 (relay-reads t3)) (second (relay-reads t1)))
  t3)
;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b) :_type_relay t1) c) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 1)))
    ((:Add (c (:Mul (a b) :_type_relay t1)) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 0))))
(defsimplifier
    (contiguous-after-wmma :speed 0)
    ((:WMMA (c (:Move (_ a) :_type_relay t1) (:Move (_ b) :_type_relay t2)) :reduction reduction :_type_relay t3) -> (:WMMA (c a b) :reduction reduction :_type_relay (wmma-relay-from1 t1 t2 t3))))

(defun apply-jit-specific-simplifiers (avm)
  "A toplevel for jit-specific optimizers. (WMMA Simplification, Removing Views, Contiguous Node Removals)"
  (declare (type avm avm))
  (%safely-purge-views-from-graph avm)
  (wmma-rewriter (avm-graph avm) :no-verify t)
  (contiguous-after-wmma (avm-graph avm) :no-verify t))

(defun apply-multiexpr-grouping (pipeline)
  "Group several computation into a single :EXPR Node to simplify.
E.g.:
  T0 : X <- sin(m)
  T1 : Y <- cos(X)
is updated to:
  T0+T1 : Y <- cos(sin(m))
It uses aIR graph features; accordingly must be applied before doing memory-planner optimization!"
  (declare (type hash-table pipeline))
  (let ((copied-graph (make-hash-table)))
    (maphash
     #'(lambda (ts graph)
	 (declare (type graph graph))
	 (let* ((toplevels (graph-seen graph))
		(outputs
		    (remove-duplicates
		     (apply #'append (map 'list #'(lambda (x) (recursively-find-output-id x graph)) toplevels))))
		(out-memory-ids
		  (map
		   'list
		   #'(lambda (x &aux (node (id->value graph x)))
		       (case (node-type node)
			 (:WHERE (list (second (node-reads node)) (second (relay-reads (read-type-relay node)))))
			 (otherwise (list (first (node-reads node)) (first (relay-reads (read-type-relay node)))))))
		   outputs))
		(out-types
		  (map
		   'list
		   #'(lambda (x &aux (node (id->value graph x)))
		       (car (relay-writes (read-type-relay node))))
		   outputs)))
	   (when (and
		  outputs
		  (every
		   #'(lambda (x &aux (type (node-type x)))
		       (and (vm-instruction-p x)
			    ;; Only element wise ops are fused!
			    (not (find type `(:Allocate :MOVE :WHERE :WMMA :STORE)))
			    (if (eql (node-class x) :BinaryOps)
				(= (length (node-reads x)) 2)
				t)))
		   (graph-nodes graph)))
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
	     (let ((fused-nodes (map 'list #'(lambda (x xt r) (create-multiexpr-node graph x xt (first r) (second r))) outputs out-types out-memory-ids)))
	       (setf (gethash ts copied-graph) (copy-list (graph-nodes graph)))
	       (setf (graph-nodes graph) fused-nodes)))))
     pipeline)
    copied-graph))

(defun apply-memory-planner (pipeline avm &key (multiexpr t))
  "Applies the in-place mutation to the given graph."
  (declare (type hash-table pipeline) (avm avm))
  (let ((alias-map (make-hash-table :test #'eql))
	(allocs)
	(stash-map (make-hash-table :test #'eql))
	(scalars (make-hash-table :test #'eql))
	(stashed-graph (if multiexpr (apply-multiexpr-grouping pipeline) (make-hash-table)))
	(remove-id-list)
	(outputs-by-pipeline
	  (loop for graph being the hash-values of pipeline
		collect
		(remove-duplicates (apply #'append (map 'list #'(lambda (x) (recursively-find-output-id x graph)) (graph-seen graph)))))))
    (labels ((alias (key value) (setf (gethash key alias-map) value))
	     (read-tmp (val) (or (gethash val stash-map) (load-from-map val)))
	     (load-from-map (key) (or (gethash key alias-map) key))
	     (alias-node (node)
	       (alias (car (node-writes node)) (load-from-map (car (node-reads node)))))
	     (update-tmp-type (node)
	       (setf (relay-writes (read-type-relay node))
		     (loop for wt in (relay-writes (read-type-relay node))
			   for w in (node-writes node)
			   if (gethash w stash-map)
			     collect (make-buffer 0 nil nil (buffer-dtype wt) nil)
			   else
			     collect wt))
	       (setf (relay-reads (read-type-relay node))
		     (loop for rt in (relay-reads (read-type-relay node))
			   for r in (node-reads node)
			   if (gethash r stash-map)
			     collect (make-buffer 0 nil nil (buffer-dtype rt) nil)
			   else
			     collect rt))))
      (loop for graph being the hash-values of pipeline
	    for outputs in outputs-by-pipeline
	    for count upfrom 0
	    for copied-graph = (apply #'make-graph (copy-list (graph-nodes graph))) do
	      (dolist (node `(,@(gethash count stashed-graph) ,@(graph-nodes graph)))
		(case (node-type node)
		  (:Allocate nil)
		  (:WHERE
		   (let* ((base-write (car (node-writes node)))
			  (create-tmpvar-p (null (find base-write outputs)))
			  (writes (if create-tmpvar-p
				      (list (intern (format nil "_val_~a_~a" count (length (hash-table-keys stash-map)))))
				      (list (load-from-map (second (node-reads node))))))
			  (reads  (map 'list #'read-tmp (node-reads node))))
		     (when create-tmpvar-p
		       (push (cons (car writes) (buffer-dtype (car (relay-writes (read-type-relay node))))) allocs)
		       (setf (gethash base-write stash-map) (car writes)))
		     (alias (car (node-writes node)) (second (node-reads node)))
		     (update-tmp-type node)
		     (setf (node-writes node) writes
			   (node-reads node) reads)))
		  (otherwise
		   (if (and
			(eql (node-type node) :Load)
			(symbolp (getattr node :value))
			(let ((alloc (id->value (avm-graph avm) (car (node-reads node)))))
			  (and
			   (eql (node-type alloc) :Allocate)
			   (= (getattr alloc :nrank) 0))))
		       (progn
			 ;; X <- Alloc(...)
			 ;; Y <- LOAD(X, value=a)
			 (setf (gethash (car (node-reads node)) scalars) (getattr node :value))
			 (alias (car (node-reads node)) (getattr node :value))
			 (push (node-id node) remove-id-list))
		       (when (>= (length (node-writes node)) 1)
			 (assert (= (length (node-writes node)) 1) () "Currently, caten/ajit only supports (length node-writes) == 1.")
			 (let* ((base-write (car (node-writes node)))
			        (create-tmpvar-p (null (find base-write outputs)))
			        (writes (if create-tmpvar-p
					    (list (intern (format nil "_val_~a_~a" count (length (hash-table-keys stash-map)))))
					    (list (load-from-map (car (node-reads node))))))
			        (reads (map 'list #'read-tmp (node-reads node))))
			   (when create-tmpvar-p
			     (push (cons (car writes) (buffer-dtype (car (relay-writes (read-type-relay node))))) allocs)
			     (setf (gethash base-write stash-map) (car writes)))
			   (alias-node node)
			   (update-tmp-type node)
			   (setf (node-writes node) writes
				 (node-reads node) reads))
			 (when (eql (node-type node) :EXPR)
			   ;; update inner exprs
			   (let ((buffers (getattr node :buffers)))
			     (assert (every #'(lambda (x) (eql :AREF (expr-op x))) buffers))
			     (mapc
			      #'(lambda (aref)
				  (setf (expr-x aref) (load-from-map (expr-x aref))))
			      buffers)))))))))
      (let* ((allocs (remove-duplicates allocs))
	     (allocs (map 'list #'(lambda (x) (make-node :Buffer :Allocate (list (car x)) nil :nrank 0 :dtype (cdr x) :_tmp t :_type_relay (make-buffer 0 nil nil (cdr x) nil))) allocs)))
	(assert (null (gethash -1 pipeline)))
	(setf (gethash -1 pipeline) (apply #'make-graph allocs)))
      (maphash
       #'(lambda (_ g)
	   (declare (ignore _))
	   (setf (graph-nodes g) (loop for n in (graph-nodes g) unless (find (node-id n) remove-id-list) collect n)))
       pipeline)
      (setf (avm-fw-outputs avm) (map 'list #'load-from-map (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'load-from-map (avm-bw-outputs avm)))
      (let ((new-id2tensor (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (load-from-map k) new-id2tensor) v))
	 (avm-id2tensor avm))
	(setf (avm-id2tensor avm) new-id2tensor))
      (let ((new-variables (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (load-from-map k) new-variables) v))
	 (avm-variables avm))
	(setf (avm-variables avm) new-variables)))
    scalars))

(defun apply-static-gensym (avm)
  (declare (type avm avm))
  (let ((alias-table (make-hash-table))
	(val-count 0))
    (labels ((val-gensym (id)
	       (if (symbolp id)
		   (or
		    (gethash id alias-table)
		    (prog1
			(setf (gethash id alias-table) (intern (format nil "val_~a" val-count)))
		      (incf val-count)))
		   id)))
      (dolist (node (graph-nodes (avm-graph avm)))
	(setf (node-writes node) (map 'list #'val-gensym (node-writes node))
	      (node-reads node) (map 'list #'val-gensym (node-reads node))))
      (setf (avm-fw-outputs avm) (map 'list #'val-gensym (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'val-gensym (avm-bw-outputs avm)))
      (let ((new-id2tensor (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (val-gensym k) new-id2tensor) v))
	 (avm-id2tensor avm))
	(setf (avm-id2tensor avm) new-id2tensor))
      (let ((new-variables (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (val-gensym k) new-variables) v))
	 (avm-variables avm))
	(setf (avm-variables avm) new-variables)))))
