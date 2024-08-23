(in-package :caten/ajit)

(defstruct (Kernel-Renderer)
  (nodes (error "nodes must occur!") :type list)
  (nth 0 :type fixnum)
  (args nil))

(defun split-kernel (nodes)
  (declare (type list nodes))
  (let ((kernels) (outputs))
    (loop with nest = 0
	  for node in nodes
	  for type = (node-type node)
	  if (eql type :FOR)
	    do (push node kernels) (incf nest)
	  else if (eql type :ENDFOR) do
	    (if (= 1 nest)
		(progn (decf nest) (push node kernels) (push (nreverse kernels) outputs) (setf kernels nil))
		(progn (decf nest) (push node kernels)))
	  else do
	    (push node kernels))
    (when kernels (push kernels outputs))
    (loop for out in outputs
	  for nth upfrom 0
	  collect (make-kernel-renderer :nodes out :nth nth))))

;; A special graph dedicated to the rendering process
(defun r/for (idx upfrom below by) (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by))
(defun r/endfor (idx) (make-node :Render :ENDFOR nil nil :idx idx))
(defun r/funcall (name args)
  ;; :idx = (T12 -> 12)
  (make-node :Render :FUNCALL nil nil :name name :args args :idx (parse-integer (subseq name 1))))
(defun r/if (condition) (make-node :Render :IF nil nil :condition condition))
(defun r/else () (make-node :Render :ELSE nil nil))
(defun r/endif () (make-node :Render :ENDIF nil nil))
(defun create-rendering-graph (polyhedron lisp-ast)
  (declare (type polyhedral polyhedron))
  ;; -1 is a placeholder for the tmpvar allocation.
  (let ((new-graph (list (r/funcall "T-1" nil))))
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
    (setf (gethash -1 (poly-pipeline polyhedron)) (make-graph))
    (apply #'make-graph (reverse new-graph))))
