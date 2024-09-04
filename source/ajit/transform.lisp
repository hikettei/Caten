(in-package :caten/ajit)
;; Toplevel for this function is `render-graph-from-polyhedral`

(defstruct (Kernel-Renderer)
  (nodes (error "nodes must occur!") :type list)
  (nth 0 :type fixnum)
  (args nil))

(defmethod find-outermost-for ((r kernel-renderer))
  (let ((nodes (kernel-renderer-nodes r)))
    (loop for node in nodes
	  if (eql (node-type node) :FOR)
	    do (return-from find-outermost-for node))))

(defmethod kernel-renderer-outermost-loop-eq ((a kernel-renderer) (b kernel-renderer))
  "Compares two outermost loops in the a and b"
  (multiple-value-bind (a b) (values (find-outermost-for a) (find-outermost-for b))
    (and a b
	 (equal (getattr a :idx) (getattr b :idx))
	 (expr-eq (getattr a :upfrom) (getattr b :upfrom))
	 (expr-eq (getattr a :below) (getattr b :below))
	 (expr-eq (getattr a :by) (getattr b :by))
	 (eql (getattr a :scope) (getattr b :scope)))))

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

(defmethod merge-two-loops ((a kernel-renderer) (b kernel-renderer) (poly Polyhedral))
  "Merges two iteration. the relations between a and b can be formulated as:
```
T0(t=0, i)  // a-scal
for(int i=0; i<10; i++) {
  T1(t=1, i) // a-vec
}
T2(t=2, 0) // b-scal
for(int i=0; i<10; i++) {
  T2(t=3, i) // b-vec
}
```
We consider shuffling these nodes which never violates lexiographical order.
"
  ;; [TODO] Graph will never starts with IF
  (let ((lex (poly-lex-table poly))
	(a-outermost (find-outermost-for a))
	(b-outermost (find-outermost-for b)))
    (when (and a-outermost b-outermost)
      (assert (equal (getattr a-outermost :idx) (getattr b-outermost :idx))))
    (labels ((nodes->lex (nodes)
	       (loop for node in nodes
		     if (eql (node-type node) :FUNCALL)
		       collect (gethash (getattr node :idx) lex)))
	     (lex-dep-ok (a b)
	       "for all: (a1, a2, ...) > (b1, b1, ...)"
	       (<= (apply #'max (nodes->lex a)) (apply #'min (nodes->lex b))))
	     (scal-p (nodes) (null (find :FOR nodes :key #'node-type)))
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
      ;; [TODO] There are more fusable (or relocation) iteration patterns.
      (multiple-value-bind (a-scal a-vec) (separate-scalar-and-vector-parts a)
	(multiple-value-bind (b-scal b-vec) (separate-scalar-and-vector-parts b)
	  ;; For simplicity, we consider all 4 patterns:
	  (cond
	    ((and (null a-scal) (null b-scal) a-vec b-vec)
	     ;; When T0, T2 is null. We can merge them as long as a-vec/b-vec are satisfying lex-dep.
	     (cond
	       ((lex-dep-ok a-vec b-vec) (apply-merge a-vec b-vec))
	       ((lex-dep-ok b-vec a-vec) (apply-merge b-vec a-vec))
	       (T nil)))
	    ((and a-scal b-scal (null a-vec) (null b-vec))
	     ;; As well as on the around case.
	     (cond
	       ((lex-dep-ok a-scal b-scal) (apply-merge a-scal b-scal))
	       ((lex-dep-ok b-scal a-scal) (apply-merge b-scal a-scal))
	       (T nil)))
	    ((and a-scal (null b-scal) a-vec b-vec)
	     ;; T2=null, there is no separating node -> fuse always
	     (when (lex-dep-ok a-vec b-vec) ;; (lex-dep-ok a-scal a-vec) is always true since originally scheduled so.
	       (apply-merge a-scal a-vec b-vec)))
	    ((and (null a-scal) a-vec b-scal b-vec)
	     ;; T=0 is null, try relocate T2 into T0, (if it fails, they cannot be fused)
	     (when (lex-dep-ok b-scal a-vec)
	       (apply-merge b-scal a-vec b-vec)))
	    ((and a-scal b-scal a-vec b-vec)
	     (when (and
		    (lex-dep-ok b-scal a-vec)
		    (lex-dep-ok a-scal b-scal))
	       (apply-merge a-scal b-scal a-vec b-vec)))))))))
  
(defun fuse-outermost-loops (polyhedral blueprints)
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
This may reduce the number of extra allocation for tmpvar.
"
  (declare (type polyhedral polyhedral)
	   (type list blueprints))
  (remove-duplicates
   (loop with last-visited = (car blueprints)
	 for blueprint in `(,@(cdr blueprints) nil)
	 for merged = (when blueprint (merge-two-loops last-visited blueprint polyhedral))
	 collect
	 (if (and blueprint merged (kernel-renderer-outermost-loop-eq last-visited blueprint))
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

(defmethod collapse-loop ((kr kernel-renderer) (poly polyhedral))
  "Collapses the loop in the kernel to get more parallelization/vectorization opportunities, e.g.:
for(int i=0; i<10; i++) {
  for(int j=0; j<10; j++) {
    T0(i, j);
  }
}
is transformed into:
for(int i=0; i<10*10; i++) {
  T0(i);
}
"
  kr)

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
  "TODO:
外側から(device.config.parallelize_outermost_n)はParallelize
それ以外はUnroll, Tilingをする
;; Unrollを考えてから，float Accumを考えた方がいいと思う。(float _acc_0は難しいこと考えなくて良くて，最初のwriteと最後のwriteをTrackすればいい)
"
  ;; [TODO] The Goal
  ;; - Symbolic Unrolling
  ;; - Support Loop Reminder
  ;; - Optimize Gemm
  ;; - float acc_0 temporary var
  ;; - index computation simplification
  ;; UnrollしてReminderが発生したらもう一度分割する必要がある
  ;; Reminderが発生しない場合のみを考えてみる (GPUだと余分なカーネルが増えて律速になる)
  ;; or UNROLL_SYMBOLIC=1 ?
  ;; [Memo] apply-memory-plannerに影響しない? -> しない
  ;; TODO: Metal float4を使いたい (OK)
  ;; TODO: UnrolledFuncall         (OK)
  ;; TODO: VectorizedFuncall       (OK)
  ;;  - attributeとして実装する PackedFuncall
  ;; 一番深いところより一個下はReminderを生成してもいい
  ;; Index Computation Simplification is required!
  ;; TODO: これする前にIndex ComputationをSimplifyする
  (when (= 0 (ctx:getenv :PACKED)) (return-from pack-loop-funcall kr))
  (when (some #'(lambda (x) (null (find (node-type x) `(:FOR :ENDFOR :FUNCALL)))) (kernel-renderer-nodes kr))
    ;; Failed: Constains IF
    (return-from pack-loop-funcall kr))
  (labels ((unroll-size (node &aux (idx (getattr node :idx)))
	     (and
	      (eql (node-type node) :FOR)
	      (eql (getattr node :scope) :LOCAL) ;; <- TODO: Delete
	      (getattr node :coincident)
	      (trivia:match (getattr node :upfrom)
		((Expr :op :const :x (trivia:guard x (and (numberp x) (= x 0)))) t))
	      (trivia:match (getattr node :by)
		((Expr :op :const :x (trivia:guard x (and (numberp x) (= x 1)))) t))
	      (trivia:match (getattr node :below)
		((Expr :op :<= :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x (trivia:guard x (numberp x))))
		 (and (= 0 (mod (1+ x) unroll-by))
		      (/ (1+ x) unroll-by)))
		((Expr :op :< :x (Expr :op :const :x (trivia:guard id (equal id idx))) :y (Expr :op :const :x (trivia:guard x (numberp x))))
		 (and (= 0 (mod x unroll-by))
		      (/ x unroll-by))))))
	   (unroll-p-allow-reminder (node &aux (idx (getattr node :idx)))
	     ;; Returning new :upfrom
	     (and
	      (eql (node-type node) :FOR)
	      (eql (getattr node :scope) :LOCAL) ;; <- TODO: Delete
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
	   (extract-reminders (rem-table nodes &key (seen))
	     (let ((out
		     (loop with stacked-reminders = (make-hash-table :test #'equal)
			   for node in nodes
			   if (eql (node-type node) :FUNCALL)
			     collect node
			   else if (eql (node-type node) :FOR)
				  collect
				  (let ((rem (gethash (node-id node) rem-table)))
				    (when (null (find (node-id node) seen))
				      (push (node-id node) seen)
				      (when rem (setf (gethash (getattr node :idx) stacked-reminders) rem)))
				    node)
			   else if (eql (node-type node) :ENDFOR)
				  append (append
					  (list node)
					  (prog1
					      (gethash (getattr node :idx) stacked-reminders)
					    (remhash (getattr node :idx) stacked-reminders)))
			   else
			     collect node)))
	       (if (= (length out) (length nodes))
		   out
		   (extract-reminders rem-table out :seen seen))))
	   (subseq-loops (nodes idx)
	     ;; TODO: Consider loop reminders and update them
	     ;; :_val_suffix = _0_0
	     (loop with flag = t
		   for n in nodes
		   if (and flag (eql (node-type n) :ENDFOR) (equal (getattr n :idx) idx))
		     collect (copy-node n) and do (setf flag nil)
		   else if flag collect (copy-node n)))
	   (unroll-valid-p (nodes)
	     (every #'(lambda (x) (find (node-type x) `(:FOR :ENDFOR :FUNCALL))) nodes))
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
			      (getattr node :args))))))
	      idx
	      n-unroll)))
    (let ((replacements (make-hash-table :test #'equal))
	  (reminders (make-hash-table :test #'equal)))
      (loop for node in (kernel-renderer-nodes kr)
	    for nth upfrom 0
	    for unroll-size = (unroll-size node)
	    for unroll-with-reminder = (unroll-p-allow-reminder node)
	    for loop-body = (when (or unroll-with-reminder unroll-size)
			      (subseq-loops (nthcdr nth (kernel-renderer-nodes kr)) (getattr node :idx)))
	    for unroll-valid-p = (unroll-valid-p loop-body)
	    for idx = (getattr node :idx)
	    if (and unroll-valid-p unroll-size) do
	      (setf (getattr node :by) (make-expr :const unroll-by))
	      (dolist (node loop-body)
		(when (eql (node-type node) :FUNCALL)
		  (setf (gethash (getattr node :name) replacements)
			(make-unroll idx unroll-by (or (gethash (getattr node :name) replacements) node)))))
	    else if unroll-with-reminder do
	      ;; can be unrolled by introducing a reminder just after the loop
	      ;; 並列化の次元に注意 (global-sizeと干渉すると新たなIterationが増える)
	      (let ((base-loop (copy-node node)))
		(setf (getattr node :by) (make-expr :const unroll-by)
		      (getattr node :below) (decf-below (getattr node :below) idx unroll-by))
		(dolist (node loop-body)
		  (when (eql (node-type node) :FUNCALL)
		    (setf (gethash (getattr node :name) replacements)
			  (make-unroll idx unroll-by (or (gethash (getattr node :name) replacements) node)))))
		;; Generating Loop Reminder
		(setf (getattr base-loop :by) (make-expr :const 1)
		      (getattr base-loop :upfrom) unroll-with-reminder 
		      (car loop-body) base-loop
		      (gethash (node-id node) reminders) (append loop-body))))
      (setf (kernel-renderer-nodes kr)
	    (extract-reminders
	     reminders
	     (loop
	       for node in (kernel-renderer-nodes kr)
	       if (eql (node-type node) :FUNCALL)
		 collect (or (gethash (getattr node :name) replacements) node)
	       else
		 collect node)))
      ;; [TODO] Vectorize the generated loop-reminder.
      kr)))

(defun render-graph-from-polyhedral (polyhedral nodes)
  "Finalizes an rendering graph to use based on nodes."
  (declare (type list nodes) (type polyhedral))
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
    (flet ((opt (x) (pack-loop-funcall (collapse-loop x polyhedral) polyhedral 4)))
      (map
       'list
       #'opt
       (funcall (if (= 0 (ctx:getenv :SERIALIZE))
		    #'fuse-outermost-loops
		    #'(lambda (x y) (declare (ignore x)) y))
		polyhedral
		(loop for out in (reverse outputs)
		      for nth upfrom 0
		      collect (make-kernel-renderer :nodes out :nth nth)))))))
