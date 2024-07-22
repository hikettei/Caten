(in-package :caten/api)

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
     (:_TmpScalarConst ((* x y)) :dtype dtype)))

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
  ;; TODO: Read undefined check
  graph)
