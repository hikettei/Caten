(in-package :caten/ajit)

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
  "Assumes loop fusion in a base polyhedral is always valid."
  (multiple-value-bind (a b) (values (find-outermost-for a) (find-outermost-for b))
    (and a b
	 (equal (getattr a :idx) (getattr b :idx))
	 (expr-eq (getattr a :upfrom) (getattr b :upfrom))
	 (expr-eq (getattr a :below) (getattr b :below))
	 (expr-eq (getattr a :by) (getattr b :by))
	 (eql (getattr a :scope) (getattr b :scope)))))

(defun fuse-kernels (blueprints)
  (flet ((except-for (nodes for-a for-b)
	   `(,for-a
	     ,@(loop for node in nodes
		     unless (or (find (node-id node) `(,for-a ,for-b) :key #'node-id)
				(and (eql (node-type node) :ENDFOR)
				     (find (getattr node :idx) `(,for-a ,for-b) :key #'(lambda (x) (getattr x :idx)))))
		       collect node)
	     ,(r/endfor (getattr for-a :idx)))))
    (remove-duplicates
     (loop with last-visited = (car blueprints)
	   for blueprint in `(,@(cdr blueprints) nil)
	   collect
	   (if (and blueprint (kernel-renderer-outermost-loop-eq last-visited blueprint))
	       (let ((a (find-outermost-for last-visited))
		     (b (find-outermost-for blueprint)))
		 (setf last-visited
		       (make-kernel-renderer
			:nodes (except-for (append (kernel-renderer-nodes last-visited) (kernel-renderer-nodes blueprint)) a b)
			:nth (kernel-renderer-nth last-visited)))
		 last-visited)
	       (prog1
		   last-visited
		 (setf last-visited blueprint))))
     :key #'kernel-renderer-nth)))

(defun split-kernel (nodes)
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
    (fuse-kernels
     (loop for out in (reverse outputs)
	   for nth upfrom 0
	   collect (make-kernel-renderer :nodes out :nth nth)))))

(defun apply-bands (bands nodes &key (global-rank 2))
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
;; A special graph dedicated to the rendering process
;; Loop is either of :Global or :Local
;; :Global is attributed only after the polyhedral compiler is confirmed that it is parallelized.
(defun r/for (idx upfrom below by) (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by))
(defun r/endfor (idx) (make-node :Render :ENDFOR nil nil :idx idx))
(defun r/funcall (name args)
  ;; :idx = (T12 -> 12)
  (make-node :Render :FUNCALL nil nil :name name :args args :idx (parse-integer (subseq name 1))))
(defun r/if (condition) (make-node :Render :IF nil nil :condition condition))
(defun r/else () (make-node :Render :ELSE nil nil))
(defun r/endif () (make-node :Render :ENDIF nil nil))
(defun create-rendering-graph (lisp-ast bands)
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
		  (push (r/funcall name args) new-graph))
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
    (apply #'make-graph (apply-bands bands (simplify-rendering-nodes (reverse new-graph))))))

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

(defun padding-list (list rank &key (with 0))
  (append list (loop for i in (range 0 (- rank (length list))) collect with)))

