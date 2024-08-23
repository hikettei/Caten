(in-package :caten/aasm)

(defpattern number (x) `(guard ,x (numberp ,x)))
(defpattern boolean (x) `(guard ,x (typep ,x 'boolean)))

(defun reinitialize-tensor (graph id node)
  (declare (type graph graph))
  (multiple-value-bind (nrank shape stride dtype views)
      (infer-tensor-info graph id)
    ;; [TODO] Fix why shape infer fails
    (when (or (null nrank) (null dtype))
      (return-from reinitialize-tensor))
    (flet ((->find (x) (id->value graph x)))
      (setf shape (map 'list #'->find shape)
	    stride (map 'list #'->find stride)))
    (let ((viewed (every #'identity views)))
      (if (= nrank 0)
	  (with-context-nodes (m1 (%salloc :dtype dtype :id id)))
	  (with-context-nodes
	    (m1 (%alloc nrank shape stride :dtype dtype :id (if viewed (gensym "TID") (node->id node))))
	    ;; [TODO] Test against viewed outputs
	    (m2 (if viewed (%view m1 shape (nth 0 views) (nth 1 views) (nth 2 views) (nth 3 views) stride :id (node->id node)) m1)))))))

;; Folds against scalar values
(defsimplifier
    (%0_fuse_load_alloc)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value (number x)) -> (:_TmpScalarConst (x) :dtype dtype))
    ((:Load ((:Allocate () :nrank 0 :dtype :bool)) :value (boolean x)) -> (:_TmpScalarBool () :value x)))

(defsimplifier
    (%1_fold_constant :speed 0)
    ((:Add ((:_TmpScalarConst (x) :dtype dtype) (:_TmpScalarConst (y)))) -> (:_TmpScalarConst ((+ x y)) :dtype dtype))
    ((:Mul ((:_TmpScalarConst (x) :dtype dtype) (:_TmpScalarConst (y)))) -> (:_TmpScalarConst ((* x y)) :dtype dtype))
    ((:Neg ((:_TmpScalarConst (x) :dtype dtype))) -> (:_TmpScalarConst ((- x)) :dtype dtype))
    ((:Recip ((:_TmpScalarConst (x) :dtype dtype))) -> (:_TmpScalarConst ((/ x)) :dtype dtype))
    ((:GCD ((:_TmpScalarConst (x) :dtype dtype) (:_TmpScalarConst (y)))) -> (:_TmpScalarConst ((gcd x y)) :dtype dtype))
    ((:< (_ (:_TmpScalarConst (x)) (:_TmpScalarConst (y)))) -> (:_TmpScalarBool () :value (< x y)))
    ((:!= (_ (:_TmpScalarConst (x)) (:_TmpScalarConst (y)))) -> (:_TmpScalarBool () :value (not (= x y))))
    ((:MAX ((:_TmpScalarConst (x) :dtype dtype) (:_TmpScalarConst (y)))) -> (:_TmpScalarConst ((max x y)) :dtype dtype))
    ((:NOT ((:_TmpScalarBool () :value value))) -> (:_TmpScalarBool () :value (not value)))
    ((:AND ((:_TmpScalarBool () :value x) (:_TmpScalarBool () :value y))) -> (:_TmpScalarBool () :value (and x y)))
    ((:OR ((:_TmpScalarBool () :value x) (:_TmpScalarBool () :value y))) -> (:_TmpScalarBool () :value (or x y)))
    ((:WHERE ((:_TmpScalarBool () :value x) (:_TmpScalarConst (y) :dtype dtype) (:_TmpScalarConst (z))))
     ->
     (:_TmpScalarConst ((if x y z)) :dtype dtype))     
    ((:Mul (_ (:_TmpScalarConst ((= 0))))) -> ((node graph) (reinitialize-tensor graph (car (node-writes node)) node)))
    ((:Mul ((:_TmpScalarConst ((= 0))) _)) -> ((node graph) (reinitialize-tensor graph (car (node-writes node)) node)))
    ((:Mul (x (:_TmpScalarConst ((= 1))))) -> (:_TmpPurged (x)))
    ((:Mul ((:_TmpScalarConst ((= 1))) x)) -> (:_TmpPurged (x)))
    ((:Add (x (:_TmpScalarConst ((= 0))))) -> (:_TmpPurged (x)))
    ((:Add ((:_TmpScalarConst ((= 0))) x)) -> (:_TmpPurged (x)))
    ((:INDEX-COMPONENTS (~ ss))
     -> ;; inlining the shape/stride computation
     ((node graph)
      (when ss
	(let* ((ss-nodes (map 'list #'(lambda (x) (id->value graph x)) ss))
	       (new-shape (loop for ss-node in (cdr ss-nodes)
				for ss-val  in (cdr ss)
				if (and ss-node (eql (node-type ss-node) :_TmpScalarConst))
				  collect (car (node-reads ss-node))
				else
				  collect ss-val)))
	  (unless (equal new-shape (cdr ss))
	    (make-node
	     :Indexing :INDEX-COMPONENTS
	     (node-writes node) `(,(car ss) ,@new-shape)))))))
    ((:Allocate (~ ss) :nrank (guard nrank (> 0)) :dtype dtype :from from)
     -> ;; inlining the shape/stride computation
     ((node graph)
      (when ss
	(let* ((ss-nodes (map 'list #'(lambda (x) (id->value graph x)) ss))
	       (new-shape (loop for ss-node in ss-nodes
				for ss-val  in ss
				if (and ss-node (eql (node-type ss-node) :_TmpScalarConst))
				  collect (car (node-reads ss-node))
				else
				  collect ss-val)))
	  (unless (equal new-shape ss)
	    (make-node
	     :Buffer :Allocate
	     (node-writes node) new-shape
	     :nrank nrank :dtype dtype :from from))))))
    ((:View (~ ss) :broadcast broadcast :nrank nrank)
     ->
     ((node graph)
      (when ss
	(let* ((ss-nodes (map 'list #'(lambda (x) (id->value graph x)) ss))
	       (new-views (loop for ss-node in ss-nodes
				for ss-val  in ss
				if (and ss-node (eql (node-type ss-node) :_TmpScalarConst))
				  collect (car (node-reads ss-node))
				else
				  collect ss-val)))
	  (unless (equal new-views ss)
	    (make-node
	     :Buffer :View
	     (node-writes node) new-views
	     :nrank nrank :broadcast broadcast)))))))

(defsimplifier
    (%2_unfold_load_alloc)
    ((:_TmpScalarConst (x) :dtype dtype)
     ->
     ((node graph)
      (with-context-nodes (_ (%load (%salloc :dtype dtype) x :id (node->id node))))))
    ((:_TmpScalarBool () :value value)
     ->
     ((node graph)
      (with-context-nodes (_ (%load (%salloc :dtype :bool) value :id (node->id node))))))
    ((:_TmpPurged (x))
     ->
     ((node graph)
      (make-node :Buffer :Move (node-writes node) (list (car (node-writes node)) x)))))

(defun fold-constant (graph &aux (n (length (graph-nodes graph))))
  (declare (type Graph graph))
  (assert (null (find :_TmpScalarConst (graph-nodes graph) :key #'node-type))
	  ()
 	  "_TmpScalarConst shouldn't exist!")
  (assert (null (find :_TmpScalarBool (graph-nodes graph) :key #'node-type))
	  ()
 	  "_TmpScalarBool shouldn't exist!")
  (%0_fuse_load_alloc graph :no-verify t)
  (%1_fold_constant graph :no-verify t)
  (let ((purges (loop for node in (graph-nodes graph)
		      if (eql (node-type node) :_TmpPurged)
			collect node)))
    ;; If y <- _TmpPurge(x), rewrite all y with x
    (dolist (pnode purges)
      (assert (symbolp (car (node-reads pnode))))
      (flet ((->new (x)
	       (if (eql (car (node-writes pnode)) x)
		   (car (node-reads pnode))
		   x)))
	(loop for node in (graph-nodes graph)
	      for n upfrom 0
	      do (setf (node-reads node) (map 'list #'->new (node-reads node))))
	(assert (equal (graph-outputs graph) (map 'list #'->new (graph-outputs graph))) ()))))
  (%2_unfold_load_alloc graph :no-verify t)
  ;;(setf (graph-nodes graph) (loop for n in (graph-nodes graph) if (not (eql (node-type n) :_TmpPurged)) collect n))
  (assert (null (find :_TmpScalarConst (graph-nodes graph) :key #'node-type))
	  ()
	  "_TmpScalarConst shouldn't exist! (but it is a simplifier's bug)")
  (assert (null (find :_TmpScalarBool (graph-nodes graph) :key #'node-type))
	  ()
	  "_TmpScalarBool shouldn't exist! (but it is a simplifier's bug)")
  (assert (null (find :_TmpPurged (graph-nodes graph) :key #'node-type))
	  ()
	  "_TmpPurged shouldn't exist! (it is a simplifier's bug)")
  ;;(print n)
  (if (not (= (length (graph-nodes graph)) n))
      (progn
	(verify-graph graph)
	(fold-constant graph))
      (verify-graph graph))
  graph)
