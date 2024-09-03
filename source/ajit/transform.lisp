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

(defun fuse-outermost-loops (blueprints)
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
    (fuse-outermost-loops
     (loop for out in (reverse outputs)
	   for nth upfrom 0
	   collect (make-kernel-renderer :nodes out :nth nth)))))

;; [TODO]
;;  - coincidenceから Loop Collapseをここでやる
;;  - TODO: Symbolic Loop Unrolling (not going to use ISL)
;;  - gemm-testを通す
;;  - !rand segvを修正する
;;    - ;; (caten (!matmul (ax+b `(10 20) 1 0) (!sin (ax+b `(20 30) 1 0))))
;;    - Shape Inferの時点でよくみるとおかしい
;;  - graph.lispから，ループ関連の処理をどっかに分ける
