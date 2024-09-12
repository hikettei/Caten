(in-package :caten/ajit)

;; render-graph.lisp
;; Lispfied AST of the ISL AST.
;; Optimizations are in `transform.lisp`

(defun apply-bands (bands nodes &key (global-rank 2))
  "A special graph dedicated to the rendering process
Loop is either of :Global or :Local
:Global is attributed only after the polyhedral compiler is confirmed that it is parallelized."
  (flet ((find-band (sched)
	   ;; obviously this needs to be reconsidered
	   (find sched bands :test #'(lambda (x y) (search x (format nil "~a" (band-domain y)) :test #'char=)))))
    (loop with stacked-loops = nil
	  for node in nodes
	  if (eql (node-type node) :FUNCALL)
	    do (let ((band (find-band (getattr node :name))))
		 (when band
		   (loop with last-dim = (1- (length stacked-loops))
			 for lp in (reverse stacked-loops)
			 for c in (band-coincident band)
			 for rank upfrom 0
			 do (setf (getattr lp :scope) (if (= (length stacked-loops) 1)
							  :global
							  (if (and (<= rank global-rank) (not (= last-dim rank))) (if c :global :local) :local))
				  (getattr lp :coincident) c
				  (getattr lp :permutable) (band-permutable band)))))
	  else if (eql (node-type node) :FOR)
		 do (push node stacked-loops)
	  else if (eql (node-type node) :ENDFOR)
		 do (setf stacked-loops (remove (getattr node :idx) stacked-loops :test #'string= :key #'(lambda (x) (getattr x :idx)))))
    nodes))

(defun r/for (idx upfrom below by) (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by))
(defun r/endfor (idx) (make-node :Render :ENDFOR nil nil :idx idx))
(defun r/funcall (name args &key (unroll-offsets))
  ;; :idx = (T12 -> 12)
  (make-node :Render :FUNCALL nil nil :name name :args args :idx (parse-integer (subseq name 1)) :unroll-offsets unroll-offsets))
(defun r/if (condition) (make-node :Render :IF nil nil :condition condition))
(defun r/else () (make-node :Render :ELSE nil nil))
(defun r/endif () (make-node :Render :ENDIF nil nil))

(defun create-rendering-graph (lisp-ast bands device max-dimension)
  ;; -1 is a placeholder for the tmpvar allocation.
  (let ((new-graph))
    (labels ((lower (object)
	       (when (listp object) (return-from lower (map 'list #'lower object)))
	       (trivia:ematch object
		 ((AstBlock :body body) (map 'list #'lower body))
		 ((AstFor :idx idx :from upfrom :to to :by by :body body :execute-once _)
		  (push (r/for idx upfrom to by) new-graph)
		  (lower body)
		  (push (r/endfor idx) new-graph))
		 ((User :name name :args args)
		  (push (r/funcall name (padding-list args max-dimension :with (make-expr :const 0))) new-graph))
		 ((AstIf :condition cond :then-node then :else-node else)
		  (push (r/if cond) new-graph)
		  (lower then)
		  (when else
		    (push (r/else) new-graph)
		    (lower else))
		  (push (r/endif) new-graph))
		 ((Expr :op _ :x _ :y _)
		  (error "create-rendering-graph: Expr should not occur here!")))))
      (lower lisp-ast))
    (apply #'make-graph (apply-bands bands (simplify-rendering-nodes (reverse new-graph)) :global-rank (device-parallel-depth device)))))

(defun simplify-rendering-nodes (nodes)
  (let ((len (length nodes)))
    (let ((new (funcall (compose #'simplifier/relocate-guard-node #'simplifier/remove-empty-for #'simplifier/remove-empty-if) nodes)))
      (if (= (length new) len)
	  new
	  (simplify-rendering-nodes new)))))

(defun simplifier/remove-empty-if (nodes &aux (removed nil))
  (loop for node in nodes
	for nth upfrom 0
	if (and (eql (node-type node) :IF)
		(nth (1+ nth) nodes)
		(eql (node-type (nth (1+ nth) nodes)) :ENDIF))
	  do (push (node-id (nth (1+ nth) nodes)) removed)
	else unless (find (node-id node) removed)
	       collect node))

(defun simplifier/remove-empty-for (nodes &aux (removed nil))
  (loop for node in nodes
	for nth upfrom 0
	if (and (eql (node-type node) :FOR)
		(nth (1+ nth) nodes)
		(eql (node-type (nth (1+ nth) nodes)) :ENDFOR))
	  do (push (node-id (nth (1+ nth) nodes)) removed)
	else unless (find (node-id node) removed)
	       collect node))

(defun simplifier/relocate-guard-node (nodes)
  "Tries to relocated the body under the guard node.
For example, consider the following schedule:
```
{
  for (int c0 = 0; c0 <= 9; c0 += 1)
    for (int c1 = 0; c1 <= 9; c1 += 1)
      // T0, T3, T4, T5 can be moved here
      for (int c2 = 0; c2 <= 19; c2 += 1)
        TN(XXXX)
        for (int c3 = 0; c3 <= 9; c3 += 1) {
          if (c2 == 0) {
            T0(c0, c1, c3, 0);
            T3(c0, c1, c3, 0);
            T4(c0, c1, c3, 0);
            T5(c0, c1, c3, 0);
          }
          T6(c0, c1, c3, c2);
          if (c3 == 0)
            T7(c0, c1, 0, c2);
          T8(c0, c1, c3, c2);
        }
  for (int c0 = 0; c0 <= 9; c0 += 1) {
    T1(c0);
    T2(c0);
  }
}
```
Here, T0, T3, T4, and T5 are independent of c3 but located in the body of `c3`. If they are also independent from TN(XXXX).
"
  (let ((if-statements
	  (loop with count = 0
		while (and (nth count nodes) (<= count (length nodes)))
		for ir = (nth count nodes)
		for body = (when (eql (node-type ir) :IF)
			     (let* ((body (subseq nodes (1+ count)))
				    (endif (position :ENDIF body :key #'node-type))
				    (body (subseq body 0 endif)))
			       ;; No deeper nests here?
			       (and
				(every #'(lambda (x) (null (find (node-type x) `(:FOR :IF :ELSE)))) body)
				(every #'(lambda (x) (eql (node-type x) :FUNCALL)) (cdr body))
				body)))    
		if (and (eql (node-type ir) :IF) body)
		  collect (prog1 `(,ir ,@body ,(nth (+ 1 count (length body)) nodes)) (incf count (length body)))
		else
		  do (incf count)))
	(index-components
	  (loop for node in nodes
		if (eql (node-type node) :FOR)
		  collect (getattr node :idx))))
    (labels ((guard-p (condition)
	       ;; Currently supports for a single expr, but (_gid0==0&&_gid3==0) should be relocated as well.
	       (trivia:match condition
		 ((Expr :op :equal :x (Expr :op :const :x (trivia:guard x (stringp x))) :y (Expr :op :const :x 0))
		  ;; Check x is a index-component
		  (when (find x index-components :test #'equalp)
		    (list x)))))
	     (try-relocate (if-state)
	       "if-state: a list of nodes starting with IF, ending with corresponding ENDIF."
	       (assert (eql :IF (node-type (car if-state))))
	       (assert (eql :ENDIF (node-type (car (last if-state)))))
	       (multiple-value-bind (start-if body end-if)
		   (values (car if-state) (cdr (butlast if-state)) (car (last if-state)))
		 (let ((indices (guard-p (getattr start-if :condition)))
		       (related-nodes)
		       (determined-ids)
		       (start-position (1- (position (node-id start-if) nodes :key #'node-id))))
		   (when (null indices) (return-from try-relocate))
		   ;; Reading the node in backward until discovering all indices
		   ;; for  _gid0 in ...      }
		   ;;   for ...              } related-nodes
		   ;;    T_n                 }
		   ;;    if (_gid0==0) { <- current position is here
		   ;;       ..
		   ;;    }
		   (when (< start-position 0) (return-from try-relocate)) ;; No outermost loops to relocate
		   (loop named tape
			 with position = start-position
			 for i downfrom position to 0
			 for node = (nth i nodes)
			 if (eql (node-type node) :FOR) do
			   (push (getattr node :idx) determined-ids)
			   (push node related-nodes)
			   (when (every #'(lambda (x) (find x determined-ids :test #'equalp)) indices)
			     (return-from tape))
			 else if (or (eql (node-type node) :ENDIF) (eql (node-type node) :FOR)) do
			   ;; If the nest is complicated, it is beyond this function: giving up
			   (return-from try-relocate)
			 else
			   do (push node related-nodes))
		   ;; Checking the validity of relocating
		   ;; <Place_To_Relocate>
		   ;; for _gid0 in ...
		   ;;   <NodeSet1>
		   ;;   for _gid1 in ...
		   ;;;    <NodeSet2>
		   ;;     if (_gid0==0) {
		   ;;         <Body>
		   ;;     }
		   ;; If <Body> is independent from <NodeSet1> <NodeSet2> ..., they can be relocated.
		   ;; Hmmmm
		   (print related-nodes)))))
      (dolist (if-state if-statements)
	(try-relocate if-state))
      nodes)))
