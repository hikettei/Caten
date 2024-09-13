(in-package :caten/aasm)

(defsimplifier
    (fuse-duplicated-store :speed 0)
    ((:Store ((:Allocate (~ s1) :nrank nrank :dtype dtype1) (:Allocate (~ s2) :dtype dtype2)))
     ->
     ((node graph)
      (when (and (eql dtype1 dtype2) (equal s1 s2))
	(make-node :Buffer :Allocate (node-writes node) s1 :nrank nrank :dtype dtype1)))))

(defsimplifier
    (%replace-dynamic-shape :speed 0)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value (guard x (symbolp x))) -> (:_TmpDynamicShape (x) :dtype dtype)))

(defsimplifier
    (%unfold-dynamic-shape :speed 0)
    ((:_TmpDynamicShape (x) :dtype dtype)
     ->
     ((node graph)
      (with-context-nodes (_ (%load (%salloc :dtype dtype) x :id (node->id node)))))))

(defun simplify-dynamic-arithmetic (graph)
  "Consider the following graph structure:
```
val_1 = a * b
val_2 = a * b
```
Here, we refer to Dynamic Shape as `a`, and `b`. dynamic shapes are usually used to determine the loop bound by ajit.
In this case, the following loop structure can be generated:
```
for (int _gid=0;_gid<val_1;_gid++) { ... }
for (int _gid=0;_gid<val_2;_gid++) { ... }
```
When determining the validity of fusing the two outermost loops, the ISL scheduler may need to know `val_1==val_2`.
In this simplification, we enumerate all of such dynamic computation arithmetic and remove all but the first one, updating read dependencies to keep the graph structure.
This may reduce the compilation time of the dynamic kernel dramatically, also simplifies the generated kernel. (duplicated computation are eliminated)"
  (declare (type graph graph))
  ;; 1. Replace: (Load (Alloc) Symbol) -> _TmpDynamicShape
  (%replace-dynamic-shape graph :no-verify t)
  ;; 2. (First step) Remove duplicated dynamic shape loading path. e.g.: the following pattern can be considered in the caten/apis design:
  ;; val_1 = _TmpDynamicShape(a)
  ;; val_2 = _TmpDynamicShape(a)
  ;; If val_1 is firstly declared in the graph, val_2 is replaced with val_1.
  (let ((cache (make-hash-table :test #'equal))) ;; Cached by (symbol . dtype)
    (flet ((read-cache (sym)
	     (when (not (symbolp sym)) (return-from read-cache sym))
	     (let ((val (id->value graph sym)))
	       (when (null val) (return-from read-cache sym))
	       (when (not (eql (node-type val) :_TmpDynamicShape)) (return-from read-cache sym))
	       (let* ((key `(,(car (node-reads val)) ,(getattr val :dtype)))
		      (cached-val (ensure-gethash key cache (car (node-writes val)))))
		 (setf (gethash key cache) cached-val)
		 cached-val))))
      (loop for node in (graph-nodes graph)
	    do (setf (node-reads node) (map 'list #'read-cache (node-reads node))))))
  ;; [TODO] The previous step will produce a duplicated comptuation process e.g.: val_1 = a * b and val_2 = a * b
  ;; Likewise previous impl remove such patterns.
  (%unfold-dynamic-shape graph :no-verify t)
  (verify-graph graph)
  graph)

(defun optimize-aasm (graph)
  (declare (type graph graph))
  (fold-constant graph)
  (simplify-dynamic-arithmetic graph)
  (fuse-duplicated-store graph))
