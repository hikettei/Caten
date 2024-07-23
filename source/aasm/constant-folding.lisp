(in-package :caten/aasm)

(defpattern number (x) `(guard ,x (numberp ,x)))
;; Folds against scalar values
(defsimplifier
    (%0_fuse_load_alloc)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value (number x)) -> (:_TmpScalarConst (x) :dtype dtype)))

(defsimplifier
    (%1_fold_constant :speed 0)
    ((:Add ((:_TmpScalarConst (x) :dtype dtype) (:_TmpScalarConst (y))))
     ->
     (:_TmpScalarConst ((+ x y)) :dtype dtype))
    ((:Mul ((:_TmpScalarConst (x) :dtype dtype) (:_TmpScalarConst (y))))
     ->
     (:_TmpScalarConst ((* x y)) :dtype dtype))
    ((:Allocate (~ ss) :nrank (guard nrank (> 0)) :dtype dtype)
     ->
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
	     :nrank nrank :dtype dtype))))))
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
      (with-context-nodes (_ (%load (%salloc :dtype dtype) x :id (node->id node)))))))

(defun fold-constant (graph)
  (declare (type Graph graph))
  (assert (null (find :_TmpScalarConst (graph-nodes graph) :key #'node-type))
	  ()
 	  "_TmpScalarConst shouldn't exist!")
  (%0_fuse_load_alloc graph)
  (%1_fold_constant graph)
  (%2_unfold_load_alloc graph)
  (assert (null (find :_TmpScalarConst (graph-nodes graph) :key #'node-type))
	  ()
	  "_TmpScalarConst shouldn't exist! (but it is a simplifier's bug)")
  graph)
