(in-package :caten/ajit)
;; WIP: Refactoring this...
;; The list of nodes generated by render-graph.lisp is called a Render Graph.
;; transform.lisp applies a "graph-level optimization" to the given graph.
;; including:
;; - (Primitive) Loop Transformation. Loop Fusion and Loop Collapse
;; - Funcall -> Packed Funcall Transformation (see pack-loop-funcall), enabling the following stuff in the renderer level.
;;   - Loop Unroll
;;   - Vectorize
;;   - Tiling

(defstruct (Kernel-Renderer)
  (nodes (error "nodes must occur!") :type list)
  (nth 0 :type fixnum)
  (args nil))

(defmethod kernel-renderer-loop-depth ((k kernel-renderer))
  (let ((nest 0) (depth 0))
    (loop for node in (kernel-renderer-nodes k)
	  for type = (node-type node)
	  if (eql type :FOR)
	    do (incf depth)
	  if (eql type :ENDFOR)
	    do (decf depth)
	  end
	  do (setf nest (max nest depth)))
    nest))

(defmethod collect-loop-for ((r kernel-renderer))
  (let ((nodes (kernel-renderer-nodes r)))
    (remove-duplicates
     (loop for node in nodes
	   if (eql (node-type node) :FOR)
             collect node)
     :key #'(lambda (x) (getattr x :idx))
     :test #'equalp)))

(defmethod find-outermost-for ((r kernel-renderer))
  (let ((nodes (kernel-renderer-nodes r)))
    (loop for node in nodes
	  if (eql (node-type node) :FOR)
	    do (return-from find-outermost-for node))))

(defmethod kernel-renderer-loop-eq ((a kernel-renderer) (b kernel-renderer) pipeline)
  "Compares two outermost loops in the a and b"
  (and
   (= (kernel-renderer-loop-depth a) (kernel-renderer-loop-depth b))
   (multiple-value-bind (a-loops b-loops) (values (collect-loop-for a) (collect-loop-for b))
     (every
      #'(lambda (a b)
          (and
	   (equal (getattr a :idx) (getattr b :idx))
	   (expr-eq (getattr a :upfrom) (getattr b :upfrom))
	   (expr-eq (getattr a :below) (getattr b :below))
	   (expr-eq (getattr a :by) (getattr b :by))
	   (eql (getattr a :scope) (getattr b :scope))))
      a-loops b-loops))
   ;; No Transposed
   (flet ((ok? (kr)
            (every
             #'(lambda (x)
                 (if (eql (node-type x) :FUNCALL)
                     (let ((graph (gethash (getattr x :idx) pipeline)))
                       (every
                        #'(lambda (x)
                            ;; No Permutation
                            (or (null (buffer-inferred-permute x))
                                (equal (buffer-inferred-permute x) (range 0 (buffer-nrank x)))))
                        (loop for node in (graph-nodes graph)
                              append (relay-reads (read-type-relay node))
                              append (relay-writes (read-type-relay node)))))
                     t))
             (kernel-renderer-nodes kr))))
     (and (ok? a) (ok? b)))))

(defmethod separate-scalar-and-vector-parts ((a kernel-renderer))
  "Return: (values scalar-nodes vector-nodes) if nodes are:
T0(...)
T1(...)
for (...) {
  T2(...)
  T3(...)
}
T0 T1 are scalar-nodes, T2, T3 are vector-nodes.
"
  (let ((nodes (kernel-renderer-nodes a))
	(scalars)
	(vectors))
    (loop with loop-mode-p = nil
	  for node in nodes
	  if (eql (node-type node) :FOR)
	    do (setf loop-mode-p t)
	  end
	  if loop-mode-p
	    do (push node vectors)
	  else
	    do (push node scalars))
    (values (nreverse scalars) (nreverse vectors))))

(defmethod merge-two-loops ((a kernel-renderer) (b kernel-renderer))
  "Merges two iteration. the relations between a and b can be formulated as:
```
for(int i=0; i<10; i++) {
  T1(t=1, i) // a-vec
}
for(int i=0; i<10; i++) {
  T2(t=3, i) // b-vec
}
```
"
  
  (let ((a-outermost (find-outermost-for a))
	(b-outermost (find-outermost-for b)))
    (when (and a-outermost b-outermost)
      (unless (equal (getattr a-outermost :idx) (getattr b-outermost :idx))
	(return-from merge-two-loops)))
    (labels ((scal-p (nodes) (null (find :FOR nodes :key #'node-type)))
	     (remloop (nodes)
	       (loop for node in nodes
		     unless (or (find (node-id node) `(,a-outermost ,b-outermost) :key #'node-id)
				(and (eql (node-type node) :ENDFOR)
				     (find (getattr node :idx) `(,a-outermost ,b-outermost) :key #'(lambda (x) (getattr x :idx)))))
		       collect node))
	     (apply-merge (&rest timestamps &aux (insert-at (or (position-if (compose #'not #'scal-p) timestamps) -1)))
	       (unless (= insert-at -1)
		 (assert (every (compose #'not #'scal-p) (nthcdr insert-at timestamps))))
	       (append
		(loop for nodes in timestamps
		      for nth upfrom 0
		      if (scal-p nodes)
			append nodes
		      if (= nth insert-at)
			append (list a-outermost)
		      if (not (scal-p nodes))
			append (remloop nodes))
		(list (r/endfor (getattr a-outermost :idx))))))
      (multiple-value-bind (a-scal a-vec) (separate-scalar-and-vector-parts a)
	(multiple-value-bind (b-scal b-vec) (separate-scalar-and-vector-parts b)
          (cond
            ((and a-scal (null b-scal) a-vec b-vec)
             (apply-merge a-scal a-vec b-vec))
            ((and (null a-scal) (null b-scal) a-vec b-vec)
             (apply-merge a-vec b-vec))))))))

(defun fuse-outermost-loops (blueprints pipeline)
  "Fuses two rendering groups whose outermost loops are the completely equivalent.
This fusion is only applied in the original polyhedral group, (which is assumed to no circular deps, and time series deps are in straight)
So we asssume all pairs of loop fusion are always valid.
e.g.:
for(int i=0; i<10; i++) {
  // some element-wise operations (1)
}
for(int i=0; i<10; i++) {
  // some element-wise operations (2)
}
are fused into:
for(int i=0; i<10; i++) {
  // some element-wise operations (1)
  // some element-wise-operations (2)
}
"
  (declare (type list blueprints))
  (remove-duplicates
   (loop with last-visited = (car blueprints)
	 for blueprint in `(,@(cdr blueprints) nil)
	 for merged = (when blueprint (merge-two-loops last-visited blueprint))
	 collect
	 (if (and blueprint merged (kernel-renderer-loop-eq last-visited blueprint pipeline))
	     (progn
	       (setf last-visited
		     (make-kernel-renderer
		      :nodes merged
		      :nth (kernel-renderer-nth last-visited)))
	       last-visited)
	     (prog1
		 last-visited
	       (setf last-visited blueprint))))
   :key #'kernel-renderer-nth))

(defmethod separate-scalar-and-vector-parts ((a kernel-renderer))
  "Return: (values scalar-nodes vector-nodes) if nodes are:
T0(...)
T1(...)
for (...) {
  T2(...)
  T3(...)
}
T0 T1 are scalar-nodes, T2, T3 are vector-nodes.
"
  (let ((nodes (kernel-renderer-nodes a))
	(scalars)
	(vectors))
    (loop with loop-mode-p = nil
	  for node in nodes
	  if (eql (node-type node) :FOR)
	    do (setf loop-mode-p t)
	  end
	  if loop-mode-p
	    do (push node vectors)
	  else
	    do (push node scalars))
    (values (nreverse scalars) (nreverse vectors))))

(defun r/packed-funcall (funcall unrolled idx size)
  ;; :_packed t
  ;; :_unrolled (a list of actually unrolled funcall)
  ;; :_metadata ((idx1 . size) (idx . size) ...)
  (let ((new-funcall (copy-node funcall)))
    (setf (getattr new-funcall :_packed) t
	  (getattr new-funcall :_unrolled) unrolled
	  (getattr new-funcall :_metadata) (append (getattr new-funcall :_metadata) (list (cons idx size))))
    new-funcall))

(defun unroll-upfrom (below unroll-by &key (1p t))
  ;; unroll_by * (mod size unroll-by)
  (let* ((below (if 1p below (if (numberp below) (1+ below) (make-expr :+ below (make-expr :const 1)))))
	 (rem (if (and (numberp below) (numberp unroll-by)) (mod below unroll-by) (make-expr :% below unroll-by))))
    (if (and (numberp rem) (numberp below))
	(make-expr :const (- below rem))
	(make-expr :- below rem))))

(defun decf-below (expr idx unroll-by)
  (trivia:ematch expr
    ((Expr :op :<= :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x x))
     (if (and (numberp x) (numberp unroll-by))
	 (make-expr :<= idx (- x unroll-by))
	 (make-expr :<= (make-expr :+ idx (make-expr :const unroll-by)) (make-expr :const x))))
    ((Expr :op :< :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x x))
     (if (and (numberp x) (numberp unroll-by))
	 (make-expr :<= idx (- x unroll-by))
	 (make-expr :<= (make-expr :+ idx (make-expr :const unroll-by)) (make-expr :const x))))))

(defmethod pack-loop-funcall ((kr kernel-renderer) (poly polyhedral) (unroll-by fixnum))
  "Groups the iteration into several packed-funcall.
packed-funcall can be transformed into: Tiling/Vectorizing/Unrolling at the renderer level.
for (int i=0; i<a; i++) {
  T0(c0);
}
Is transformed into:
for (int i=0; i<(a-UNROLL_BY); i+=UNROLL_BY) {
        [packed_funcall]
                 { T0(c0+0)
  T0'(c0, 0~4) = { T0(c0+1)
                 { T0(c0+2)
                 { T0(c0+3)
}
for (int i=a - (mod a UNROLL_BY); i<a; i+=1) {
 T0(c0) // Loop Reminder (TODO: Optimize Index Computation)
}
"
  (when (= 0 (ctx:getenv :PACKED)) (return-from pack-loop-funcall kr))
  (when (= 1 unroll-by) (return-from pack-loop-funcall kr))
  (labels ((static-unroll-p (node &aux (idx (getattr node :idx)))
	     (and
	      (eql (node-type node) :FOR)
	      ;; If the shape is static (it is known whether reminder part occurs before compilation)
	      ;; If mod(loop_size, unroll_by) = 0, :GLOBAL loops can be packed.
	      (getattr node :coincident)
	      (trivia:match (getattr node :upfrom)
		((Expr :op :const :x (trivia:guard x (and (numberp x) (= x 0)))) t))
	      (trivia:match (getattr node :by)
		((Expr :op :const :x (trivia:guard x (and (numberp x) (= x 1)))) t))
	      (trivia:match (getattr node :below)
		((Expr :op :<= :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x (trivia:guard x (numberp x))))
		 (= 0 (mod (1+ x) unroll-by)))
		((Expr :op :< :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x (trivia:guard x (numberp x))))
		 (= 0 (mod x unroll-by))))))
	   (unroll-reminder-upfrom (node &aux (idx (getattr node :idx)))
	     ;; Return -> new :upfrom
	     (and
	      (eql (node-type node) :FOR)
	      ;; If the shape is symbolic, reminder part is computed at runtime.
	      ;; thus cannot vectorize the outermost loop. (TODO: Fix)
	      (eql (getattr node :scope) :LOCAL)
	      (getattr node :coincident)
	      (trivia:match (getattr node :upfrom)
		((Expr :op :const :x (trivia:guard x (and (numberp x) (= x 0)))) t))
	      (trivia:match (getattr node :by)
		((Expr :op :const :x (trivia:guard x (and (numberp x) (= x 1)))) t))
	      (trivia:match (getattr node :below)
		((Expr :op :<= :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x x))
		 (and
		  (if (numberp x)
		      (>= (1+ x) unroll-by)
		      t)
		  (unroll-upfrom x unroll-by :1p t)))
		((Expr :op :< :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x x))
		 (and
		  (if (numberp x)
		      (>= x unroll-by)
		      t)
		  (unroll-upfrom x unroll-by :1p t))))))
	   (make-unroll (idx n-unroll base-funcall)
             ;; Not related axes  (e.g.: = 0) is not unrolled.
             (if (find idx (getattr base-funcall :args) :key #'(lambda (x) (princ-to-string (expr-x x))) :test #'equalp)
	         (r/packed-funcall
	          base-funcall
	          (loop for node in (or (getattr base-funcall :_unrolled) (list base-funcall))
		        append
		        (loop for nth upfrom 0 below n-unroll
			      collect
			      (flet ((mapper (x)
				       (if (and (or (symbolp x) (stringp x)) (string= x idx))
				           (format nil "(~a+~a)" x nth)
				           x)))
			        (r/funcall
			         (getattr node :name)
			         (map
			          'list
			          #'(lambda (x &aux (x (copy-expr x))) (expr-recursive-replace x #'mapper) x)
			          (getattr node :args))
			         :unroll-offsets (append (getattr node :unroll-offsets) (list (cons idx nth)))))))
	          idx
	          n-unroll)
                 base-funcall))
 	   (unroll-valid-p (nodes)
	     (every #'(lambda (x) (find (node-type x) `(:FOR :ENDFOR :FUNCALL))) nodes))
  	   (subseq-loops (nodes start-id idx
			  &aux (nodes (nthcdr (or (position start-id nodes :key #'node-id) (error "~a is not found" idx)) nodes)))
	     (loop with flag = t
		   for n in nodes
		   if (and flag (eql (node-type n) :ENDFOR) (equal (getattr n :idx) idx))
		     collect (copy-node n) and do (setf flag nil)
		   else if flag collect (copy-node n)))
	   (replace-body (nodes replace-with start end)
	     (loop with replace-mode = nil
		   for n in nodes
		   if (eql (node-id n) start)
		     append (progn (setf replace-mode t) replace-with)
		   else if (eql (node-id n) end)
			  do (setf replace-mode nil)
		   else if (not replace-mode) collect n))		   
	   (unroll (nodes tgt-for)
	     (let* ((base-for (copy-node tgt-for))
		    (body (or
			   (subseq-loops nodes (node-id tgt-for) (getattr tgt-for :idx))
			   (return-from unroll nodes)))
		    (reminder-body (copy-list body))
		    (start-id (node-id (car body)))
		    (end-id   (node-id (car (last body))))
		    (reminder)
		    (static-p (static-unroll-p tgt-for))
		    (reminder-upfrom (unroll-reminder-upfrom tgt-for)))
	       (when (null (unroll-valid-p body)) (return-from unroll nodes))
	       (when (and (null static-p) (null reminder-upfrom)) (return-from unroll nodes))
	       (setf (getattr tgt-for :by) (make-expr :const unroll-by)
		     (car body) tgt-for)
	       (loop for nth upfrom 0
		     for node in body
		     if (eql (node-type node) :FUNCALL)
		       do (setf (nth nth body) (make-unroll (getattr tgt-for :idx) unroll-by node)))
	       (when (null static-p)
		 (setf (getattr base-for :by) (make-expr :const 1)
		       (getattr base-for :upfrom) reminder-upfrom
		       (getattr tgt-for :below) (decf-below (getattr tgt-for :below) (getattr tgt-for :idx) unroll-by)
		       (car reminder-body) base-for
		       reminder reminder-body))
	       (replace-body nodes (append body reminder) start-id end-id))))
    (let ((iterations
	    (loop with depth = 0
		  for node in (kernel-renderer-nodes kr)
		  if (eql (node-type node) :FOR)
		    collect
		    (cons depth node) and do (incf depth)
		  else if (eql (node-type node) :ENDFOR) do
		    (decf depth))))
      (dolist (iter (sort iterations #'> :key #'car))
	(setf (kernel-renderer-nodes kr) (unroll (kernel-renderer-nodes kr) (cdr iter)))))
    kr))

(defun render-graph-from-polyhedral (nodes pipeline)
  "Finalizes an rendering graph to use based on nodes."
  (declare (type list nodes))
  (let ((kernels) (outputs))
    (loop with nest = 0
	  with nest-by-loop = 0
	  for node in nodes
	  for type = (node-type node)
	  if (find type `(:FOR :IF))
	    do (push node kernels) (incf nest)
	  else if (find type `(:ENDFOR :ENDIF)) do
	    (if (and (= 1 nest) (some #'(lambda (x) (eql (node-type x) :FOR)) kernels))
		(progn (decf nest) (push node kernels) (push (nreverse kernels) outputs) (setf kernels nil))
		(progn (decf nest) (push node kernels)))
	  else do
	    (push node kernels))
    (push (nreverse kernels) outputs)
    (funcall (if (= 1 (ctx:getenv :SERIALIZE))
                 #'identity
                 #'(lambda (x) (fuse-outermost-loops x pipeline)))
             (loop for out in (reverse outputs)
                   for nth upfrom 0
	           collect (make-kernel-renderer :nodes out :nth nth)))))

(defmethod ->render-graph ((group Group))
  (when (group-polyhedron group)
    (render-graph-from-polyhedral
     (graph-nodes (group-render-graph group))
     (poly-pipeline (group-polyhedron group)))))
;; ~~ Memory Latency Optimizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod update-buffer-as-scalar ((node Node) mutated-list domain-space)
  "Mutates :output buffer to scalar w/ a certain condition.
```
FOR (...) {
  val_0[_gid0] = ... // val_0 is labelled as :out, and all of subsequent operations belongs to the same body.
  (operation)
  ...
}
```
then, val_0 is mutated to scalar.
```
FOR (...) {
  float val_0 = ...
  (operation)
  ...
}
``
"
  (flet ((mutate-scalar-buffer (id buffer expr read-p)
	   (if (= (buffer-nrank buffer) 0)
	       buffer
	       (let ((new (make-const-buffer (buffer-dtype buffer))))
		 ;; depend-idx-list: required to compute the position of unrollment. e.g.: val_5 -> val_5_0, val_5_1, val_5_2, ...
		 (setf (buffer-depend-idx-list new)
                       (permute-list
                        (or
                         (buffer-inferred-permute buffer)
                         (range 0 (buffer-nrank buffer)))
		        (loop for shape in (buffer-shape buffer)
			      for nth upfrom 0
			      for view = (nth nth (buffer-views buffer))
			      for dom = (nth nth domain-space)
                              ;; No need to assert this: If the body has a IF, they wont be unrolled.
			      ;; do (assert (eql (expr-op dom) :Const) () "Schedule is not a constant? (TODO: Add unrolling for this case ...)")
			      if (or (eql 0 (expr-x dom)) (and view (nth 3 view)))
			        collect nil
			      else
			        collect (expr-x dom))))
		 (when (and expr read-p) (expr-recursive-settype expr id new))
		 new))))
    (macrolet ((f (ids types read-p)
		 `(loop for val in (,ids node)
			for type in (,types (read-type-relay node))
			for nth upfrom 0
			if (find val mutated-list) do
			  (setf (nth nth (,types (read-type-relay node)))
				(mutate-scalar-buffer val type (when (eql (node-type node) :EXPR) (getattr node :EXPR)) ,read-p)))))
      (f node-reads relay-reads t)
      (f node-writes relay-writes t))))
;; ~~ Post-MultiExpr ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun expr-index-components-p (expr)
  (eql :INDEX-COMPONENTS (expr-op expr)))

(defun expr-zero-p (expr)
  (check-type expr Expr)
  (and (eql :Const (expr-op expr)) (eql 0 (expr-x expr))))
  
(defmethod domain-equal ((node1 Node) (node2 Node))
  (assert (eql (node-type node1) :FOR))
  (assert (eql (node-type node2) :FOR))
  (and
   (equal (getattr node1 :idx) (getattr node2 :idx))
   (expr-eq (getattr node1 :upfrom) (getattr node2 :upfrom))
   (expr-eq (getattr node1 :below) (getattr node2 :below))
   (expr-eq (getattr node1 :by) (getattr node2 :by))))

(defun equal-but-id (id1 id2)
  "#'equal but id1==id2 becomes true. (e.g.: use when you want expr_eq(_gid0 >= 3, _gid1 >= 3) to be true"
  (flet ((equal-but-id-p (a b)
           (cond
             ((or (equal a id1) (equal a id2))
              (or (equal b id2) (equal b id1)))
             ((or (equal b id1) (equal b id2))
              (or (equal b id1) (equal b id2)))
             (t
              (equal a b)))))
    #'equal-but-id-p))

(defmethod domain-equal-space ((node1 Node) (node2 Node))
  (assert (eql (node-type node1) :FOR))
  (assert (eql (node-type node2) :FOR))
  (let ((id1 (getattr node1 :idx))
        (id2 (getattr node2 :idx)))
    (flet ((cmp (a b) (funcall (equal-but-id id1 id2) a b)))
      (and
       (cmp (getattr node1 :idx) (getattr node2 :idx))
       (expr-cmp #'cmp (getattr node1 :upfrom) (getattr node2 :upfrom))
       (expr-cmp #'cmp (getattr node1 :below) (getattr node2 :below))
       (expr-cmp #'cmp (getattr node1 :by) (getattr node2 :by))))))

;; Graph Concatenating Operations: Extend-expr and serialize-graph
(defun extend-expr (graph group target-node leaf-node leaf-id nodeid->pipeline)
  "Merges leaf-node to the target-node by grafting them.
Note: It assumes that before/after, the argument of funcall each node belongs to are the equivalent."
  (declare (type Node target-node leaf-node))
  (expr-graft-after (getattr target-node :EXPR) leaf-id (getattr leaf-node :expr))
  (setf (getattr target-node :EXPR) (simplify-expr (getattr target-node :EXPR)))
  (remnode graph (node-id leaf-node))
  (remnode
   (gethash (gethash (node-id leaf-node) nodeid->pipeline) (poly-pipeline (group-polyhedron group)))
   (node-id leaf-node))
  (let ((used (expr-recursive-deps (getattr target-node :EXPR))))
    (setf (relay-reads (read-type-relay target-node))
          (loop for rt in (append (relay-reads (read-type-relay target-node)) (relay-reads (read-type-relay leaf-node)))
                for r in (append (node-reads target-node) (node-reads leaf-node))
                for nth upfrom 0
                if (or (= nth 0) (and (not (eql r leaf-id)) (find r used)))
                  collect rt)
          (node-reads target-node)
          (loop for r in (append (node-reads target-node) (node-reads leaf-node))
                for nth upfrom 0
                if (or (= nth 0) (and (not (eql r leaf-id)) (find r used)))
                  collect r))))

(defun serialize-graph (render-graph graph1 graph2)
  "
Relocate GRAPH1 to just before GRAPH2 to maximize the chance to scalar mutation.
```
for (...)
  GRAPH1
...
for (...)
  GRAPH2
```
=>
```
for (...)
  empty
...
for (...)
  GRAPH1
  GRAPH2"
  (declare (type Node graph1 graph2) (type graph render-graph))
  (assert (and (eql (node-type graph1) :FUNCALL) (eql (node-type graph2) :FUNCALL)))
  (assert (= 1 (count (node-id graph2) (graph-nodes render-graph) :key #'node-id)))
  (remnode render-graph (node-id graph1))
  (setf (graph-nodes render-graph)
        (loop for node in (graph-nodes render-graph)
              if (eql (node-id node) (node-id graph2))
                collect graph1
              end
              collect node)))

(defun domain->funcall-depth (graph dom)
  "Counts the depth of funcall in the graph:
```
FOR (...)
  FUNCALL }
  FUNCALL } (incf 1)
  FOR (...)
   FUNCALL } (incf 1)
  }
}
```
Sequantial FUNCALLs are counted as 1 if they belongs to the same loop body."
  (declare (type graph graph) (type list dom))
  (let ((starts (map 'list #'(lambda (x) (position (node-id x) (graph-nodes graph) :key #'node-id)) dom))
        (funcall-depth))
    (when (null starts) (return-from domain->funcall-depth 0))
    (loop with start = (apply #'min starts)
          with seen = (map 'list #'(lambda (x) (getattr x :idx)) dom)
          with idx = nil
          for node in (nthcdr start (graph-nodes graph))
          if (eql (node-type node) :FUNCALL) do (push idx funcall-depth)
          else if (eql (node-type node) :ENDFOR) do (setf seen (remove (getattr node :idx) seen :test #'equalp))
          else if (eql (node-type node) :FOR) do
          (setf idx (getattr node :idx))
          (when (null seen)
            (return-from domain->funcall-depth (length (remove-duplicates funcall-depth :test #'equalp)))))
    (length (remove-duplicates funcall-depth :test #'equalp))))

(defun relocate-two-expr (group graph src-iteration-space new-src-iteration-space-list
                          dst-iteration-space dst-node
                          read-id read-node node-domain read-domain
                          nodeid->pipeline funcall->domain)
  (declare (type group group)
           (type node src-iteration-space dst-iteration-space read-node)
           (type list new-src-iteration-space-list node-domain read-domain)
           (type hash-table nodeid->pipeline funcall->domain))
  (when (and
         ;; no permutation is required
         (every #'expr-eq (getattr src-iteration-space :args) new-src-iteration-space-list)
         ;; lives in the equivalent
         (every #'eql (map 'list #'node-id node-domain) (map 'list #'node-id read-domain))
         ;; no other kernel requires the output
         (null (find read-id (group-across-time-deps group)))
         (= (length (id->users graph read-id)) 1))
    ;; Graft the expr directly
    (extend-expr graph group dst-node read-node read-id nodeid->pipeline)
    (return-from relocate-two-expr t))
  (setf (getattr src-iteration-space :args) new-src-iteration-space-list
        (gethash (gethash (node-id read-node) nodeid->pipeline) funcall->domain) node-domain)
  (when (expr-index-components-p (getattr read-node :expr))
    ;; By doing this, serialized index-components are 100% mutated as scalar
    (when (= (length (node-reads read-node)) 2)
      (setf (node-reads read-node) (butlast (node-reads read-node))
            (relay-reads (read-type-relay read-node)) (butlast (relay-reads (read-type-relay read-node))))))
  (serialize-graph (group-render-graph group) src-iteration-space dst-iteration-space))

(defun find-permutation (lst predicate)
  (declare (type list lst) (type function predicate)
           (optimize (speed 3)))
  (block find-permutation
    (labels ((helper (remaining perm)
               (if (null remaining)
                   (when (funcall predicate perm)
                     (return-from find-permutation perm))
                   (dolist (x remaining)
                     (helper (remove x remaining :count 1)
                             (append perm (list x)))))))
      (helper lst nil)
      nil)))

(defun domain-intersect-p (dom1 dom2 &aux (unseen-dom (copy-list dom1)))
  "Assumes dom2 ⊆ dom1. dom2 and dom1 are partially equal."
  (and
   (>= (length dom1) (length dom2))
   (every
    #'(lambda (x)
        (let ((val (find x unseen-dom :test #'domain-equal-space)))
          (when val
            (setf unseen-dom (remove (node-id val) unseen-dom :key #'node-id))
            t)))
    dom2)))

;; [TODO] Refactor this function
(defmethod expr-apply-loop-fusion ((group group) (graph graph) (node node) funcall->domain nodeid->pipeline
                                   &aux
                                     (changed-p nil))
  (flet ((get-domain-from-funcall (node)
           (gethash (or (gethash (node-id node) nodeid->pipeline) (error "~a is not defined in nodeid->pipeline." node)) funcall->domain))
         (node->funcall (node)
	   (let ((idx (gethash (node-id node) nodeid->pipeline)))
	     (or
              (find idx (graph-nodes (group-render-graph group)) :key #'(lambda (x) (getattr x :idx)))
              (error "~a should be found in the rendering graph.~%~a" node (group-render-graph group)))))
         (buffer-iteration-space (buffer funcall)
           (let ((aref (simplify-expr (render-isl-aref buffer :genid #'(lambda (nth) (expr-x (nth nth (getattr funcall :args)))))))
                 (idx-list (loop for l in (graph-nodes (group-render-graph group)) if (eql (node-type l) :FOR) collect (getattr l :idx))))
             (when aref
               (intersection (expr-recursive-deps aref) idx-list :test #'equalp))))
         (mg (ids domains)
           (loop for dom in domains
                 if (find (getattr dom :idx) ids :test #'equalp)
                   collect (getattr dom :idx)))
         (expr-space-eq (buffer1 buffer2 funcall1 funcall2 permute)
           (declare (type buffer buffer1 buffer2)
                    (type node funcall1 funcall2)
                    (type list permute)
                    (optimize (speed 3)))
           (let* ((args1 (permute-list permute (getattr funcall1 :args)))
                  (args2 (getattr funcall2 :args))
                  (space1 (render-isl-aref
                           buffer1
                           :genid #'(lambda (nth) (expr-x (nth nth args1)))
                           :sum nil))
                  (space2 (render-isl-aref
                           buffer2
                           :genid #'(lambda (nth) (expr-x (nth nth args2)))
                           :sum nil)))
             (declare (type list space1 space2))
             (and
              space1 space2
              (= (length space1) (length space2))
              (every
               #'(lambda (x)
                   (let ((val (find x space2 :test #'expr-eq)))
                     (if val
                         (progn
                           (setf space2 (remove x space2 :test #'expr-eq))
                           t)
                         (return-from expr-space-eq))))
               space1))))
         (shuffle-args-with-fixing-zero-pos (src-args new-args)
           (let ((new-args-w/o-zero
                   (loop for arg in new-args
                         unless (expr-zero-p arg)
                           collect arg)))
             (loop for arg in src-args
                   if (expr-zero-p arg)
                     collect arg
                   else
                     collect (pop new-args-w/o-zero)))))
    (assert (find (node-type node) `(:WMMA :EXPR)) () "A trigger for pre-loop fusion should be a WMMA or EXPR. butgot ~a" node)
    (loop with node-domain = (get-domain-from-funcall node)
          with node-iteration-space = (node->funcall node)
          for read in (cdr (node-reads node))
          for read-type in (cdr (relay-reads (read-type-relay node)))
          for read-node-orig = (id->value graph read)
          for read-node = (when (and read-node-orig (eql (node-type read-node-orig) :EXPR)) read-node-orig)
          for read-domain = (and read-node (get-domain-from-funcall read-node))
          for read-iteration-space = (and read-node (node->funcall read-node))
          for read-write-type = (and read-node (car (relay-writes (read-type-relay read-node))))
          ;; A list of symbols which destination buffer depends on
          for dst-space = (and read-type (mg (buffer-iteration-space read-type node-iteration-space) node-domain))
          ;; A list of symbols which source buffer depends on
          for src-space = (and read-write-type (mg (buffer-iteration-space read-write-type read-iteration-space) read-domain))
          ;; [Loop Fusion]
          ;; Here, we try to relocate the source node (T0, T1) to the destination node (T2), to maximize the locality of memory.
          ;; Loop Fusion is performed by this function, and ISL.
          ;;     [read1]                [read2]
          ;; | for (...)  |         | for (...)   |
          ;; |  for (...) |         |  for (...)  | ...
          ;; |    T0[...] |         |    T1[...]  |
          ;;               \       /
          ;;               |[node]|
          ;;             | for (...) |
          ;;             |  T3[...]  |
          ;; To relocate the source node into the destination node, two nodes must satisfy the following conditions:
          ;; 1. the size of the iteration space is the partially equivalent
          ;; 2. By permuting the schedule, there's a case that access functions are the same.
          ;; Especially if two nodes lives in the same domain (loop) and not anymore used by other kernels, they can be fused directly using the graft-expr function.
          ;; In that case, T0 will be merged into T1, and T0 will be eliminated from the graph (Post-Operator Fusion).
          ;; Otherwise, the compiler will serialize the two nodes in the same domain, expecting the scalar mutation by memory-planner.
          ;; (If two vector operations lives in the same domain, and output tensor is labelled as :out, the tensor is mutated to scalar.)
          if (and
              ;; read exists
              read-node
              (domain-intersect-p node-domain read-domain)
              ;; (not (eql read (car (node-reads node))))
              ;; dimensions should match (but it is obvious because they are created from the same polyhedron)
              (= (length (getattr node-iteration-space :args)) (length (getattr read-iteration-space :args)))
              (= (length dst-space) (length src-space))
              ;; Should satisfy either of: (not to fused kernel and fused kernel)
              ;; - All bands should be intersect with the destination domain
              ;; - When merging with another kernel domain, the nest should be one.
              (=
               1
               (domain->funcall-depth
                (group-render-graph group)
                (loop for d in read-domain
                      unless (find (node-id d) node-domain :key #'node-id)
                        collect d))))
            do (let ((new-src-funcall (copy-node read-iteration-space))
                     (rank (length (getattr node-iteration-space :args)))
                     (stop nil))
                 (setf (getattr new-src-funcall :args)
                       (loop for arg in (getattr new-src-funcall :args)
                             if (expr-zero-p arg)
                               collect arg
                             else
                               collect
                               (let ((pos (position (expr-x arg) src-space :test #'equalp)))
                                 (if pos
                                     (make-expr :const (nth pos dst-space))
                                     (progn
                                       (setf stop t) ;; If not found -> failed
                                       (make-expr :const 0))))))
                 (flet ((ok? (permute)
                          (expr-space-eq
                           read-write-type
                           read-type
                           new-src-funcall
                           node-iteration-space
                           permute)))
                   ;; [FixME] Is there an algorithm for finding this permutation in a one shot?
                   ;; I think doing this is ridiculous.
                   (let ((valid-permutation (find-permutation (range 0 rank) #'ok?)))
                     (when (and (not stop) valid-permutation)
                       (setf changed-p t)
                       (relocate-two-expr
                        group graph
                        read-iteration-space
                        (shuffle-args-with-fixing-zero-pos
                         (getattr read-iteration-space :args)
                         (permute-list valid-permutation (getattr new-src-funcall :args)))
                        node-iteration-space node read read-node node-domain read-domain nodeid->pipeline funcall->domain)))))))
  changed-p)

(defmethod post-simplify-multiexpr ((group Group))
  "Applies further multiexpr grouping to the scheduled mp.
Consider this pseudo-python kernel representing Embedding Op.

(Unoptimized)
```python
def main(val_35, val_54, val_48, val_31, val_37, val_41) {
  for _gid0 in range(0, 1000): # sentence_len
    for _gid1 in range(0, 101): # batch_size
      for _gid2 in range(0, 100): # vocab_size
        val_48[_gid1, _gid0, _gid2] = _gid2 == val_37[_gid1, _gid0] # T0(_gid1, _gid0, _gid2)
      for _gid2 in range(0, 102): # embedding_dim
        val_54_acc = 0.0 # T1(_gid0, _gid1, _gid2)
        for _gid3 in range(0, 100): # vocab_size
          val_35[_gid1, _gid0, _gid3, _gid2] = val_31[_gid3, _gid2] if val_48[_gid1, _gid0, _gid3] else 0.0 # T2(_gid0, _gid1, _gid2, _gid3)
        for _gid3 in range(0, 100): # vocab_size
           val_54_acc += val_35[_gid1, _gid0, _gid3, _gid2] # T3(_gid0, _gid1, _gid2, _gid3)
        val_54[_gid1, _gid0, _gid2] = val_54_acc
}
```

Here, T2 and T0 are the strongly-connected components, and the domain T0 is a subset of T2. In default, ISL Scheduler (Pluto) won't fuse these two domains because this will produce a dead execution loop. However, in general, making this cache often results in lower performance especially in deep learning inference. So we will fuse them.

(Optimized)
```python
def main(val_35, val_54, val_48, val_31, val_37, val_41)
  for _gid0 in range(0, 1000): # sentence_len
    for _gid1 in range(0, 101): # batch_size
      for _gid2 in range(0, 102): # embedding_dim
        val_54_acc = 0.0
        for _gid3 in range(0, 100): # vocab_size?
          val_54_acc += val_31[_gid3, _gid2] if _gid3 == val_37[_gid1, _gid0]) else 0.0
        val_54[_gid1, _gid0, _gid2] = val_54_acc
```
Note: This is a trade-off: it minimizes the number of DRAM accesses, which generally improves performance, but it ignores the number of Domain executions, which can create Dead Loop.
"
  (when (group-realize-on-vm group) (return-from post-simplify-multiexpr))
  (let ((graph (groups->graph (list group)))
	(render-graph (group-render-graph group))
	(funcall->domain (make-hash-table))
	(nodeid->pipeline (make-hash-table))
	(pipeline (poly-pipeline (group-polyhedron group))))
    ;; Gathering information ...
    (flet ((simple-p (node)
             (and
              (find (node-type node) `(:FOR :ENDFOR :FUNCALL))
              (if (eql (node-type node) :FUNCALL)
                  (every #'(lambda (x) (eql (expr-op x) :Const)) (getattr node :args))
                  t))))
      (unless (every #'simple-p (graph-nodes render-graph)) (return-from post-simplify-multiexpr))
      (maphash
       #'(lambda (ts graph)
	   (dolist (n (graph-nodes graph))
	     (setf (gethash (node-id n) nodeid->pipeline) ts)))
       pipeline)
      (loop with domains = nil
	    for node in (graph-nodes render-graph)
	    if (eql (node-type node) :FOR)
	      do (push node domains)
	    if (eql (node-type node) :ENDFOR)
	      do (setf domains (remove (getattr node :idx) domains :key #'(lambda (x) (getattr x :idx)) :test #'equalp))
	    if (eql (node-type node) :FUNCALL)
	      do (when (gethash (getattr node :idx) funcall->domain)
		   (when (>= (ctx:getenv :JIT_DEBUG) 1)
		     (warn "The node ~a appeared in the scheduled graph more than twise times and thus cannot apply post-simplify multiexpr." node))
		   (return-from post-simplify-multiexpr))
		 (setf (gethash (getattr node :idx) funcall->domain) (reverse domains))))
    ;; Applying Simplifiers ...
    (macrolet ((do-funcall (form &key (recursively nil))
		 `(flet ((f (&aux (changed-p nil))
                           (loop for node in (graph-nodes render-graph)
			         if (eql (node-type node) :FUNCALL) do
			           (dolist (node (graph-nodes (gethash (getattr node :idx) pipeline)))
			             (when (or (eql (node-type node) :WMMA) (eql (node-type node) :EXPR))
			               (setf changed-p (or changed-p ,form)))))
                           changed-p))
                    ,(if recursively
                         `(loop while (f))
                         `(f)))))
      ;; Reading render-node by render-node
      ;; t=0 | FOR idx = ...    (skip)
      ;; t=1 | FUNCALL = ...    (apply simplifier)
      ;; t=2 | ENDFOR idx = ... (skip)
      ;;       ...
      ;; Applying render-graph level simplifiers, all of these are optional.
      ;; - [ ] Fuse Scalar Kernel + Matmul Kernel, (See (!normal) with :mean=0.0, :std=1.0)
      (do-funcall (expr-apply-loop-fusion group graph node funcall->domain nodeid->pipeline) :recursively t)
      (simplify-render-graph group))))
