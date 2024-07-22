(in-package :caten/api)

(defpattern number (x) `(guard ,x (numberp ,x)))
;; Folds against scalar values
(defsimplifier
    (%0_fuse_load_alloc)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value (number x)) -> (:_TmpScalarConst (x) :dtype dtype)))

(defsimplifier
    (%1_fold_constant :speed 0)
    ((:Add ((:_TmpScalarConst (x)) (:_TmpScalarConst (y))))
     ->
     (:Const ((+ x y))))
    ((:Mul ((:_TmpScalarConst (x)) (:_TmpScalarConst (y))))
     ->
     (:Const ((* x y)))))

(defsimplifier
    (%2_unfold_load_alloc)
    ((:_TmpScalarConst (x) :dtype dtype)
     ->
     ((node graph)
       (with-context-nodes (_ (%load (%salloc :dtype dtype) x :id (node->id node)))))))

;; wip: 1 is pruned
(defun fold-constant (graph)
  (%0_fuse_load_alloc graph)
  (%1_fold_constant graph)
  (%2_unfold_load_alloc graph)

  ;; TODO: Assert _TmpScalarConst is removed.
  graph)
