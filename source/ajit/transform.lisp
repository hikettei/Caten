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
	      n-unroll))
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

(defun render-graph-from-polyhedral (nodes)
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
    (loop for out in (reverse outputs)
	  for nth upfrom 0
	  collect (make-kernel-renderer :nodes out :nth nth))))

(defmethod ->render-graph ((group Group))
  (when (group-polyhedron group)
    (render-graph-from-polyhedral (graph-nodes (group-render-graph group)))))

;; ~~ Memory Latency Optimizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod update-buffer-as-scalar ((node Node) mutated-list domain-space)
  (flet ((mutate-scalar-buffer (id buffer expr read-p)
	   (if (= (buffer-nrank buffer) 0)
	       buffer
	       (let ((new (make-const-buffer (buffer-dtype buffer))))
		 ;; depend-idx-list: required to compute the position of unrollment. e.g.: val_5 -> val_5_0, val_5_1, val_5_2, ...
		 (setf (buffer-depend-idx-list new)
		       (loop for shape in (buffer-shape buffer)
			     for nth upfrom 0
			     for view = (nth nth (buffer-views buffer))
			     for dom = (nth nth domain-space)
			     do (assert (eql (expr-op dom) :Const) () "Schedule is not a constant? (TODO: Add unrolling for this case ...)")
			     if (or (eql 0 (expr-x dom)) (and view (nth 3 view)))
			       collect nil
			     else
			       collect (expr-x dom)))
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
;; ** Post-MultiExpr is WIP**
(defmethod expr-index-components-p ((node node))
  (assert (eql (node-type node) :EXPR))
  (and
   (getattr node :EXPR)
   (eql (expr-op (getattr node :EXPR)) :INDEX-COMPONENTS)))

(defmethod domain-permutation ((fc1 Node) (fc2 Node))
  "
```
for (int c0 = 0; c0 <= 9; c0 += 1) {
  T0(c0, 0); ----------------------------|
}                                        |
                                         | Moving
for (int c0 = 0; c0 <= 9; c0 += 1) {     |
  T1 (0, c0); <--------------------------|
}
```
When moving a node in T0 into T1, the operation is represented as:
`permute_all_buffers_in_funcall(T0, domain_permutation(T0, T2))`
"
  (assert (eql (node-type fc1) :FUNCALL))
  (assert (eql (node-type fc2) :FUNCALL))
  (assert (= (length (getattr fc1 :args)) (length (getattr fc2 :args))))
  (flet ((zero-p (x)
	   (and
	    (eql (expr-op x) :Const)
	    (eql 0 (expr-x x)))))
    (let ((initial-range (range 0 (length (getattr fc1 :args)))))
      (macrolet ((swap (a b)
		   `(let ((tmp (nth ,a initial-range)))
		      (setf (nth ,a initial-range) (nth ,b initial-range)
			    (nth ,b initial-range) tmp))))
	(loop for arg in (getattr fc1 :args)
	      for nth upfrom 0
	      unless (zero-p arg) do
		(let ((pos (position arg (getattr fc2 :args) :test #'expr-eq)))
		  (unless pos (return-from domain-permutation)) ;; Failed (fc2 is not a subset of fc1)
		  (swap pos nth))))
      initial-range)))

(defun %permute (buffer permute &aux (buffer (copy-buffer buffer)))
  (assert (= (length permute) (buffer-nrank buffer)))
  (setf (buffer-shape buffer) (permute-list permute (buffer-shape buffer))
	(buffer-stride buffer) (permute-list permute (buffer-stride buffer))
	(buffer-views buffer) (and (buffer-views buffer) (every #'identity (buffer-views buffer)) (permute-list permute (buffer-views buffer))))
  buffer)

(defmethod domain-equal ((node1 Node) (node2 Node))
  (assert (eql (node-type node1) :FOR))
  (assert (eql (node-type node2) :FOR))
  (and
   (equal (getattr node1 :idx) (getattr node2 :idx))
   (expr-eq (getattr node1 :upfrom) (getattr node2 :upfrom))
   (expr-eq (getattr node1 :below) (getattr node2 :below))
   (expr-eq (getattr node1 :by) (getattr node2 :by))))

(defun extend-expr (target-node leaf-node)
  (declare (type Node target-node leaf-node))
  
  )

(defun serialize-graph (graph1 graph2)
  "
Relocated GRAPH1 in advance of GRAPH2
for (...)
  GRAPH1
for (...)
  GRAPH2
->
for (...)
  GRAPH1
  GRAPH2"
  (declare (type Graph graph1 graph2))

  )

;; [TODO]
;; - Randn < 2 Kernels (Fuse Scalar Kernels and vector parts)
(defmethod expr-apply-post-multiexpr-in-domain ((group group) (graph graph) (node node) funcall->domain nodeid->pipeline)
  "Post MultiExpr Fusion in the same domain."
  (flet ((get-domain-from-funcall (node)
           (gethash (or (gethash (node-id node) nodeid->pipeline) (error "~a is not defined in nodeid->pipeline." node)) funcall->domain))
         (domain-eq (dom1 dom2)
           (and (= (length dom1) (length dom2))
                (every #'eql (map 'list #'node-id dom1) (map 'list #'node-id dom2))))
         (no-across-domain-dep-p (id)
           ;; Returns T if A and B are connected one-by-one:
           ;; A -> B
           ;; Otherwise returns nil e.g.:
           ;; A -> B
           ;;   -> C
           (and
            ;; 
            (null (find id (group-across-time-deps group)))
            (= (length (id->users graph id)) 1))))
    (assert (eql :EXPR (node-type node)))
    ;; FOR
    ;; T0 | node0 }
    ;;    | node1 }
    ;; T1 | node2 }
    ;;    | node3 } 
    ;; ENDFOR
    ;; node0, node1 and node2, node3 are not merged because in the initial schedule, they are assigned to the different loop.
    ;; After ISL Scheduling, and if they are scheduled to the same loop, merge them with paying attention for the read/write deps.
    (loop with node-domain = (get-domain-from-funcall node)
          ;; EXPR (out-to, arg1, arg2, ...)
          ;; node -> arg1 (if arg1 and node has a single path and belongs to the same domain, merge node and arg1)
          ;;      -> arg2 ...
          ;;      -> arg3 ...
          for read in (cdr (node-reads node))
          for read-node-orig = (id->value graph read)
          for read-node = (when (and read-node-orig (eql (node-type read-node-orig) :EXPR)) read-node-orig) ;; Only EXPR and EXPR can be merged
          for read-domain = (and read-node (get-domain-from-funcall read-node))
          if (and node-domain read-domain (domain-eq node-domain read-domain)
                  (no-across-domain-dep-p read))
            do (expr-graft-after (getattr node :EXPR) read (getattr read-node :expr))
               (remnode graph (node-id read-node))
               (remnode
                (gethash (gethash (node-id read-node) nodeid->pipeline) (poly-pipeline (group-polyhedron group)))
                (node-id read-node))
               (let ((used (expr-recursive-deps (getattr node :EXPR))))
                 (setf (relay-reads (read-type-relay node))
                       (loop for rt in (append (relay-reads (read-type-relay node)) (relay-reads (read-type-relay read-node)))
                             for r in (append (node-reads node) (node-reads read-node))
                             for nth upfrom 0
                             if (or (= nth 0) (and (not (eql r read)) (find r used)))
                               collect rt)
                       (node-reads node)
                       (loop for r in (append (node-reads node) (node-reads read-node))
                             for nth upfrom 0
                             if (or (= nth 0) (and (not (eql r read)) (find r used)))
                               collect r))))))

(defmethod expr-apply-post-multiexpr-in-equivalent-domain ((group group) (graph graph) (node node) funcall->domain nodeid->pipeline)
  (flet ((get-domain-from-funcall (node)
           (gethash (or (gethash (node-id node) nodeid->pipeline) (error "~a is not defined in nodeid->pipeline." node)) funcall->domain))
         (domain-eq (dom1 dom2)
           (and (= (length dom1) (length dom2))
                ;; Conflicts with expr-apply-post-multiexpr-in-equivalent-domain
                (not (every #'eql (map 'list #'node-id dom1) (map 'list #'node-id dom2)))
                (every #'domain-equal dom1 dom2)))
         (node->funcall (node)
	   (let ((idx (gethash (node-id node) nodeid->pipeline)))
	     (find idx (graph-nodes (group-render-graph group)) :key #'(lambda (x) (getattr x :idx)))))
         (no-across-domain-dep-p (id)
           ;; Returns T if A and B are connected one-by-one:
           ;; A -> B
           ;; Otherwise returns nil e.g.:
           ;; A -> B
           ;;   -> C
           (and
            ;; 
            (null (find id (group-across-time-deps group)))
            (= (length (id->users graph id)) 1))))
    (assert (eql :EXPR (node-type node)))
    ;; FOR
    ;; T0 | node0 }
    ;;    | node1 }
    ;; T1 | node2 }
    ;;    | node3 } 
    ;; ENDFOR
    ;; node0, node1 and node2, node3 are not merged because in the initial schedule, they are assigned to the different loop.
    ;; After ISL Scheduling, and if they are scheduled to the same loop, merge them with paying attention for the read/write deps.
    (loop with node-domain = (get-domain-from-funcall node)
          with node-iteration-space = (node->funcall node)
          ;; EXPR (out-to, arg1, arg2, ...)
          ;; node -> arg1 (if arg1 and node has a single path and belongs to the same domain, merge node and arg1)
          ;;      -> arg2 ...
          ;;      -> arg3 ...
          for read in (cdr (node-reads node))
          for read-node-orig = (id->value graph read)
          for read-node = (when (and read-node-orig (eql (node-type read-node-orig) :EXPR)) read-node-orig) ;; Only EXPR and EXPR can be merged
          for read-domain = (and read-node (get-domain-from-funcall read-node))
          if (and node-domain read-domain (domain-eq node-domain read-domain)
                  (every #'expr-eq (getattr node-iteration-space :args) (getattr (node->funcall read-node) :args))
                  ;; Transform and apply? (is it possible?)
                  (null (getattr read-node :reduction))
                  (no-across-domain-dep-p read))
            do ;; (defun merge-exprを追加する
               ;; merge-expr意外に，グラフのノードを移動する方法を追加しておく (when n.args >= 2)
               (expr-graft-after (getattr node :EXPR) read (getattr read-node :expr))
               (remnode graph (node-id read-node))
               (remnode
                (gethash (gethash (node-id read-node) nodeid->pipeline) (poly-pipeline (group-polyhedron group)))
                (node-id read-node))
               (let ((used (expr-recursive-deps (getattr node :EXPR))))
                 (setf (relay-reads (read-type-relay node))
                       (loop for rt in (append (relay-reads (read-type-relay node)) (relay-reads (read-type-relay read-node)))
                             for r in (append (node-reads node) (node-reads read-node))
                             for nth upfrom 0
                             if (or (= nth 0) (and (not (eql r read)) (find r used)))
                               collect rt)
                       (node-reads node)
                       (loop for r in (append (node-reads node) (node-reads read-node))
                             for nth upfrom 0
                             if (or (= nth 0) (and (not (eql r read)) (find r used)))
                               collect r))))))
;; Merge: Scalar
;;          |
;;        Matrix
(defmethod expr-apply-post-multiexpr-subdomain ((group group) (graph graph) (node node) funcall->domain nodeid->pipeline)
  (assert (eql :EXPR (node-type node)))
  (flet ((get-domain-from-funcall (node)
           (gethash (or (gethash (node-id node) nodeid->pipeline) (error "~a is not defined in nodeid->pipeline." node)) funcall->domain))
         (no-across-domain-dep-p (id)
           ;; Returns T if A and B are connected one-by-one:
           ;; A -> B
           ;; Otherwise returns nil e.g.:
           ;; A -> B
           ;;   -> C
           (and
            ;; 
            (null (find id (group-across-time-deps group)))
            (= (length (id->users graph id)) 1))))
    (assert (eql :EXPR (node-type node)))
    ;; FOR
    ;; T0 | node0 }
    ;;    | node1 }
    ;; T1 | node2 }
    ;;    | node3 } 
    ;; ENDFOR
    ;; node0, node1 and node2, node3 are not merged because in the initial schedule, they are assigned to the different loop.
    ;; After ISL Scheduling, and if they are scheduled to the same loop, merge them with paying attention for the read/write deps.
    (loop with node-domain = (get-domain-from-funcall node)
          ;; EXPR (out-to, arg1, arg2, ...)
          ;; node -> arg1 (if arg1 and node has a single path and belongs to the same domain, merge node and arg1)
          ;;      -> arg2 ...
          ;;      -> arg3 ...
          for read in (cdr (node-reads node))
          for read-node-orig = (id->value graph read)
          for read-node = (when (and read-node-orig (eql (node-type read-node-orig) :EXPR)) read-node-orig) ;; Only EXPR and EXPR can be merged
          for read-domain = (and read-node (get-domain-from-funcall read-node))
          if (and node-domain read-domain (no-across-domain-dep-p read))
            do (format t "~%== [Fusion Chance] ====~%")
               ;; FUNCALL(0, 0, _gid0, 0)
               ;; ->
               ;; FUNCALL(0, 0,, _gid2, 0)
               (print node)
               (print read)
               (print read-node))))

(defmethod post-simplify-multiexpr ((group Group))
  "Applies further multiexpr grouping to the scheduled mp.
Consider this fake-python kernel representing Embedding Op.

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
    (flet ((simple-p (node) (find (node-type node) `(:FOR :ENDFOR :FUNCALL))))
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
    (macrolet ((do-funcall (form)
		 `(loop for node in (graph-nodes render-graph)
			if (eql (node-type node) :FUNCALL) do
			  (dolist (node (graph-nodes (gethash (getattr node :idx) pipeline)))
			    (when (eql (node-type node) :EXPR)
			      ,form)))))
      ;; Globalize Index-Components (its 100% no benefits of making a cache)
      ;; Reading render-node by render-node
      ;; t=0 | FOR idx = ...    (skip)
      ;; t=1 | FUNCALL = ...    (apply simplifier)
      ;; t=2 | ENDFOR idx = ... (skip)
      ;;       ...
      (do-funcall (expr-apply-post-multiexpr-in-domain group graph node funcall->domain nodeid->pipeline))
      (do-funcall (expr-apply-post-multiexpr-in-equivalent-domain group graph node funcall->domain nodeid->pipeline))
      ;; (do-funcall (expr-apply-post-multiexpr-subdomain group graph node funcall->domain nodeid->pipeline))
      ;; (do-funcall (expr-apply-index-component-globalize group graph node funcall->domain nodeid->pipeline))
      ;; (do-funcall (expr-apply-post-multiexpr-in-subdomain group graph node funcall->domain nodeid->pipeline))
      )))
