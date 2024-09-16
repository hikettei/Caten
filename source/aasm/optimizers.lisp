(in-package :caten/aasm)

(defnode (:Tmp :TmpDynamicShape) () "" :slots ((dtype)))
(defsimplifier
    (fuse-duplicated-store :speed 0)
    ((:Store ((:Allocate (~ s1) :nrank nrank :dtype dtype1) (:Allocate (~ s2) :dtype dtype2)))
     ->
     ((node graph)
      (when (and (eql dtype1 dtype2) (equal s1 s2))
	(make-node :Buffer :Allocate (node-writes node) s1 :nrank nrank :dtype dtype1)))))

(defsimplifier
    (%replace-dynamic-shape :speed 0)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value (guard x (or (numberp x) (symbolp x)))) -> (:TmpDynamicShape (x) :dtype dtype)))

(defsimplifier
    (%unfold-dynamic-shape :speed 0)
    ((:TmpDynamicShape (x) :dtype dtype)
     ->
     ((node graph)
      (with-context-nodes (_ (%load (%salloc :dtype dtype) x :id (node->id node)))))))

(defun simplify-dynamic-arithmetic (graph &aux (arithmetic `(:ADD :NEG :MUL :RECIP :IDIV)))
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
  ;; 2. (First step) Remove duplicated dynamic shape loading path. e.g.: the following pattern can be considered in the caten/apis design:
  ;; val_1 = _TmpDynamicShape(a)
  ;; val_2 = _TmpDynamicShape(a)
  ;; If val_1 is firstly declared in the graph, val_2 is replaced with val_1.
  (flet ((tmp-dynamic-shape-p (node)
	   (and (eql (node-type node) :Load)
		(let ((prev (id->value graph (car (node-reads node)))))
		  (and
		   prev
		   (eql (node-type prev) :Allocate)
		   (= (getattr prev :nrank) 0))))))
    (let ((cache (make-hash-table :test #'equal))
	  (alias (make-hash-table)))
      (flet ((read-cache (node)
	       (when (some #'(lambda (x) (find x (graph-outputs graph))) (node-writes node))
		 (return-from read-cache node))
	       (when (null (find (node-type node) arithmetic))
		 (return-from read-cache node))
	       (let ((reads (map 'list #'(lambda (x) (id->value graph x)) (node-reads node))))
		 (when (some #'(lambda (x) (or (null x) (not (tmp-dynamic-shape-p x)))) reads)
		   (return-from read-cache node))
		 (when (null reads) (return-from read-cache node))
		 (when (not (= (length (node-writes node)) 1)) (return-from read-cache node))
		 (let* ((key `(,(node-type node) ,@(map 'list #'(lambda (x) (getattr x :value)) reads)))
			(del-p (not (null (gethash key cache))))
			(cache-result (ensure-gethash key cache (car (node-writes node)))))
		   (setf (gethash key cache) cache-result)
		   (if del-p
		       (progn
			 (setf (gethash (car (node-writes node)) alias) cache-result)
			 node)
		       node))))
	     (r (sym) (ensure-gethash sym alias sym)))
	(setf (graph-nodes graph)
	      (loop for node in (graph-nodes graph)
		    for new = (read-cache node)
		    if new collect new))
	(dolist (n (graph-nodes graph))
	  (setf (node-reads n) (map 'list #'r (node-reads n))))))
    ;; [TODO] Apply recursively?
    ;; [TODO] If the EXPR was nested? a * b + c
    (verify-graph graph)
    graph))

(defun optimize-aasm (graph)
  (declare (type graph graph))
  (fold-constant graph)
  (simplify-dynamic-arithmetic graph)
  (fuse-duplicated-store graph))
