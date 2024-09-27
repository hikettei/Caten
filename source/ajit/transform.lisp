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
Relocate GRAPH1 to just before GRAPH2
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

(defun domain-eq-1 (dom1 dom2)
  (and (= (length dom1) (length dom2))
       (every #'eql (map 'list #'node-id dom1) (map 'list #'node-id dom2))))

(defmethod expr-apply-post-multiexpr-in-domain ((group group) (graph graph) (node node) funcall->domain nodeid->pipeline)
  "Post MultiExpr Fusion Applicable Case 1, FUNCALL belongs to the same loop (compared by node-id)"
  (flet ((get-domain-from-funcall (node)
           (gethash (or (gethash (node-id node) nodeid->pipeline) (error "~a is not defined in nodeid->pipeline." node)) funcall->domain))
         (node->funcall (node)
	   (let ((idx (gethash (node-id node) nodeid->pipeline)))
	     (or
              (find idx (graph-nodes (group-render-graph group)) :key #'(lambda (x) (getattr x :idx)))
              (error "~a should be found in the rendering graph.~%~a" node (group-render-graph group)))))
         (no-across-domain-dep-p (id)
           ;; Returns T if A and B are connected one-by-one:
           ;; A -> B
           ;; Otherwise returns nil e.g.:
           ;; A -> B
           ;;   -> C
           (and
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
          if (and node-domain read-domain (domain-eq-1 node-domain read-domain)
                  (no-across-domain-dep-p read)
                  (every #'expr-eq (getattr node-iteration-space :args) (getattr (node->funcall read-node) :args)))
            ;; for (...)   }
            ;;  for  (...) } 1. Belongs to the same loop
            ;;
            ;;   T0(a, b)  }  
            ;;   T1(a, b)  } 2. Schedule is the same
            ;; (1. and 2.) = T0 and T1 are fusable.
            do (extend-expr graph group node read-node read nodeid->pipeline))))

(defun domain-eq-2 (dom1 dom2)
  (and (= (length dom1) (length dom2))
       ;; Conflicts with expr-apply-post-multiexpr-in-equivalent-domain
       (not (domain-eq-1 dom1 dom2))
       (every #'domain-equal dom1 dom2)))

(defmethod expr-apply-post-multiexpr-in-equivalent-domain ((group group) (graph graph) (node node) funcall->domain nodeid->pipeline)
  "Post MultiExpr Fusion Applicable Case 2, FUNCALLs strongly connected, and belongs to the same loop (compared by idx, size, and order.)"
  (flet ((get-domain-from-funcall (node)
           (gethash (or (gethash (node-id node) nodeid->pipeline) (error "~a is not defined in nodeid->pipeline." node)) funcall->domain))
         (node->funcall (node)
	   (let ((idx (gethash (node-id node) nodeid->pipeline)))
	     (find idx (graph-nodes (group-render-graph group)) :key #'(lambda (x) (getattr x :idx)))))
         (no-across-domain-dep-p (id)
           (and
            (null (find id (group-across-time-deps group)))
            (= (length (id->users graph id)) 1))))
    (assert (eql :EXPR (node-type node)))
    (loop with node-domain = (get-domain-from-funcall node)
          with node-iteration-space = (node->funcall node)
          for read in (cdr (node-reads node))
          for read-node-orig = (id->value graph read)
          for read-node = (when (and read-node-orig (eql (node-type read-node-orig) :EXPR)) read-node-orig)
          for read-domain = (and read-node (get-domain-from-funcall read-node))
          if (and node-domain read-domain (domain-eq-2 node-domain read-domain)
                  (null (getattr read-node :reduction))
                  (no-across-domain-dep-p read))
            ;; for (...)   }
            ;;  for  (...) } 1. The size of iteration is the same.
            ;;   T0(a, b)  }
            ;; for (...)   }
            ;;  for (...)  } 1. The size of iteration is the same.
            ;;   T1(a, b)  }
            ;; If the schedule of T0 and T1 is the equivalent -> they can be fused directly
            ;; Otherwise -> T0 and T1 can be serialized e.g.:
            ;; for (...)
            ;;  for (...)
            ;;   T0(a, b)
            ;;   T1(a, b)
            do (if (every #'expr-eq (getattr node-iteration-space :args) (getattr (node->funcall read-node) :args))
                   (extend-expr graph group node read-node read nodeid->pipeline)
                   (serialize-graph (group-render-graph group) (node->funcall read-node) node-iteration-space)))))

(defun domain-eq-3 (dom1 dom2 &aux (unseen-dom (copy-list dom1)))
  "Assumes dom2 ⊆ dom1. dom2 and dom1 are partially equal."
  (and
   (not (domain-eq-2 dom1 dom2))
   (>= (length dom1) (length dom2))
   (every
    #'(lambda (x)
        (let ((val (find x unseen-dom :test #'domain-equal-space)))
          (when val
            (setf unseen-dom (remove (node-id val) unseen-dom :key #'node-id))
            t)))
    dom2)))

(defun find-new-iteration-space (space dest-space space-dom dest-dom)
  "Creates a new funcall when moving space into dest-space.
If failed, the function returns a keyword :failed"
  (declare (type node space dest-space)
           (type list space-dom dest-dom))
  (flet ((id->dom (spc dm)
           (let ((out (make-hash-table :test #'equal)))
             (loop for sp in (getattr spc :args)
                   ;; It is asserted that sp is a :Const
                   for d = (or
                            (find (princ-to-string (expr-x sp)) dm :key #'(lambda (x) (getattr x :idx)) :test #'equalp)
                            (when (expr-zero-p sp)
                              :broadcast))
                   do (assert (eql (expr-op sp) :Const))
                      (assert d)
                      (unless (eql d :broadcast)
                        (setf (gethash (princ-to-string (expr-x sp)) out) d)))
             out)))
    (multiple-value-bind (space/id2dom dest/id2dom)
        (values (id->dom space space-dom) (id->dom dest-space dest-dom))
      (labels ((->as-expr (idx-str)
                 (or
                  (find idx-str (getattr dest-space :args) :key #'(lambda (x) (princ-to-string (expr-x x))) :test #'equalp)
                  (error "->as-expr: ~a is not found?" idx-str)))
               (find-bands-from-unseen (idx-expr nth-first)
                 (assert (not (expr-zero-p idx-expr)))
                 (let ((key-domain (gethash (princ-to-string (expr-x idx-expr)) space/id2dom)))
                   (assert (and key-domain (not (eql key-domain :broadcast))))
                   (let ((new-band-idx-key
                           (or
                            ;; Innermost or the same rank iter first.
                            (let ((key (gethash (princ-to-string (expr-x (nth nth-first (getattr dest-space :args)))) dest/id2dom)))
                              (when (and key (domain-equal-space key-domain key))
                                (princ-to-string (expr-x (nth nth-first (getattr dest-space :args))))))
                            (find key-domain (hash-table-keys dest/id2dom)
                                  :test #'(lambda (x y) (domain-equal-space x (gethash y dest/id2dom)))))))
                     (if new-band-idx-key
                         (prog1
                             (->as-expr new-band-idx-key)
                           (remhash new-band-idx-key dest/id2dom))
                         (return-from find-new-iteration-space :failed))))))
        ;; T0(0, 0, c, 0)  Transform
        ;; ->                  =>     T0(0, 0, c, 0)
        ;; T1(a, c, b, d)
        (assert (= (length (getattr space :args)) (length (getattr dest-space :args))))
        (loop for axis in (getattr space :args)
              for nth upfrom 0
              for new-arg = (if (expr-zero-p axis)
                                axis
                                (find-bands-from-unseen axis nth))
              collect new-arg)))))

(defmethod expr-apply-post-multiexpr-subdomain ((group group) (graph graph) (node node) funcall->domain nodeid->pipeline &aux (changed-p nil))
  "Post MultiExpr Fusion Applicable Case 3, FUNCALLs strongly connected, and belongs to the partially equivalent loop (compared by idx, size, and order.)"
  (flet ((get-domain-from-funcall (node)
           (gethash (or (gethash (node-id node) nodeid->pipeline) (error "~a is not defined in nodeid->pipeline." node)) funcall->domain))
         (node->funcall (node)
	   (let ((idx (gethash (node-id node) nodeid->pipeline)))
	     (or
              (find idx (graph-nodes (group-render-graph group)) :key #'(lambda (x) (getattr x :idx)))
              (error "~a should be found in the rendering graph.~%~a" node (group-render-graph group)))))
         (no-across-domain-dep-p (id)
           (and
            (null (find id (group-across-time-deps group)))
            (= (length (id->users graph id)) 1))))
    (assert (eql :EXPR (node-type node)))
    (loop with node-domain = (get-domain-from-funcall node)
          with node-iteration-space = (node->funcall node)
          for read in (cdr (node-reads node))
          for read-node-orig = (id->value graph read)
          for read-node = (when (and read-node-orig (eql (node-type read-node-orig) :EXPR)) read-node-orig)
          for read-domain = (and read-node (get-domain-from-funcall read-node))
          if (and (not (eql read (car (node-reads node))))
                  ;; ↑の条件だけで足りるか？。。。ViewがNull or Broadcast Onlyの方が安心感がある
                  ;; EXPR(output, x, y)
                  ;; output cannot be overwritten
                  node-domain read-domain
                  ;; Transform and apply? (is it possible?)
                  (null (getattr read-node :reduction))
                  (no-across-domain-dep-p read)
                  (not (every #'eql (map 'list #'node-id node-domain) (map 'list #'node-id read-domain)))
                  (domain-eq-3 node-domain read-domain))
            ;; for (...)               }
            ;;   T0(0, 0, a, 0)
            ;; for (...)               } If loops are partially equivalent.
            ;;   for(...)
            ;;     for(...)
            ;;       for (...)
            ;;         T1(a, b, c, d)
            ;; The compiler can insert Permute(T0) before the execution of T1, by permuting the schedule of T0.
            do (let ((read-iteration-space (node->funcall read-node)))
                 ;; Assumes read-iteration-space funcall was used at once in the render-graph.
                 ;; So it is ok to overwrite its attributes.
                 (assert (eql (node-type read-iteration-space) :FUNCALL))
                 ;; Transforms the iteration space of funcall to fit the destination.
                 (let ((new-iteration-space (find-new-iteration-space
                                             read-iteration-space node-iteration-space
                                             read-domain node-domain)))
                   (when (not (eql new-iteration-space :failed))
                     (setf changed-p t)
                     ;; It is required to make new FUNCALL with args are properly shuffed.
                     ;; By finding the equivalent loop bound
                     (setf (getattr read-iteration-space :args) new-iteration-space
                           (gethash (gethash (node-id read-node) nodeid->pipeline) funcall->domain) node-domain)
                     (serialize-graph (group-render-graph group) read-iteration-space node-iteration-space))))))
  changed-p)

;; (defmethod expr-apply-post-multiexpr-wmma-transpose
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
    (macrolet ((do-funcall (form &key (type :EXPR) (recursively nil))
		 `(flet ((f (&aux (changed-p nil))
                           (loop for node in (graph-nodes render-graph)
			         if (eql (node-type node) :FUNCALL) do
			           (dolist (node (graph-nodes (gethash (getattr node :idx) pipeline)))
			             (when (eql (node-type node) ,type)
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
      (do-funcall (expr-apply-post-multiexpr-in-domain group graph node funcall->domain nodeid->pipeline))
      (do-funcall (expr-apply-post-multiexpr-in-equivalent-domain group graph node funcall->domain nodeid->pipeline))
      (do-funcall (expr-apply-post-multiexpr-subdomain group graph node funcall->domain nodeid->pipeline) :recursively t)
      ;; TODO: Special Simplifier to :type :WMMA
      ;; - [ ] Fix: broadcast-regression-test (when packed=1, they cannot be unrolled, especially when including :INDEX_COMPONENTS)
      ;; - [ ] WMMA+Transpoe Fusion

      ;; TODO: Merge Domain and SubDomain in order to complete following thing:
      ;; 1. Tranpose+Matmul Fusion (< 1 Kernels by propagating transpose)
      ;; 2. [ ] Randn < 2 Kernels by propagation scalar parts
      ;; Merge: Scalar
      ;;          |
      ;;        Matrix
      ;; 3. [x] In-Place Embedding, By propagating index-components and boolean parts

      ;; [TODO] Delete following pattern node (after applying memory-planner)
      ;; A = A; (confirmeed by calling !normal (where :mean=0.0, :std=1.0))
      ;; [TODO] Tile/Parallel/Loop Fission Scheduling to the graph applied memory-planner.
      (simplify-render-graph group))))
