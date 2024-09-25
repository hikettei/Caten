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
			 for lp in stacked-loops
			 for c in (band-coincident band)
			 for rank downfrom (1- (length stacked-loops)) to 0
			 do (setf (getattr lp :scope) (if (= (length stacked-loops) 1)
							  :global
							  (if (and (<= rank global-rank) (not (= last-dim rank))) (if c :global :local) :local))
				  (getattr lp :coincident) c
				  (getattr lp :permutable) (band-permutable band)))))
	  else if (eql (node-type node) :FOR)
		 do (push node stacked-loops)
	  else if (eql (node-type node) :ENDFOR)
		 do (setf stacked-loops (remove (getattr node :idx) stacked-loops :test #'equalp :key #'(lambda (x) (getattr x :idx)))))
    nodes))

(defun r/for (idx upfrom below by) (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by))
(defun r/endfor (idx) (make-node :Render :ENDFOR nil nil :idx idx))
(defun r/funcall (name args &key (unroll-offsets))
  ;; :idx = (T12 -> 12)
  (make-node :Render :FUNCALL nil nil :name name :args args :idx (parse-integer (subseq name 1)) :unroll-offsets unroll-offsets))
(defun r/if (condition) (make-node :Render :IF nil nil :condition condition))
(defun r/else () (make-node :Render :ELSE nil nil))
(defun r/endif () (make-node :Render :ENDIF nil nil))

(defun %create-rendering-graph-nodes (lisp-ast max-dimension)
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
    (nreverse new-graph)))

(defun create-rendering-graph (lisp-ast bands device max-dimension)
  ;; -1 is a placeholder for the tmpvar allocation.
  (apply
   #'make-graph
   (apply-bands
    bands
    (simplify-rendering-nodes (%create-rendering-graph-nodes lisp-ast max-dimension))
    :global-rank (device-parallel-depth device))))

(defun simplify-rendering-nodes (nodes)
  (let ((len (length nodes)))
    (let ((new (funcall (compose #'simplifier/remove-empty-for #'simplifier/remove-empty-if) nodes)))
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
