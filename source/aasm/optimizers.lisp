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

(defun minify-duplicated-alloc (graph &key (assert-optimized nil))
  "Minimizes the number of LOAD"
  (declare (type Graph graph) (optimize (speed 3)))
  (let ((replaceable-map (make-hash-table)) (defined) (id->node (make-hash-table)))
    (labels ((load-eql-p (node1 node2)
               (and (eql (node-type node1) :LOAD) (eql (node-type node2) :LOAD)
                    (eql (getattr node1 :value) (getattr node2 :value))
                    (let ((alloc1 (gethash (car (node-reads node1)) replaceable-map))
                          (alloc2 (gethash (car (node-reads node2)) replaceable-map)))
                      (and alloc1 alloc2 (eql (the keyword (getattr alloc1 :dtype)) (the keyword (getattr alloc2 :dtype)))))))
             (c (node)
               (when (not (= (length (node-writes node)) 1)) (return-from c))
               (setf (gethash (car (node-writes node)) id->node) node)
               (case (node-type node)
                 (:Allocate
                  (when (= 0 (length (node-reads node)))
                    (setf (gethash (car (node-writes node)) replaceable-map) node)))
                 (:Load
                  (when (gethash (car (node-reads node)) replaceable-map)
                    (when (not (member node defined :test #'load-eql-p))
                      (push node defined))))
                 (otherwise)))
             (newid (sym)
               (when (not (symbolp sym)) (return-from newid sym))
               (let ((node (gethash sym id->node)))
                 (when (null node) (return-from newid sym))
                 (let ((c (find node defined :test #'load-eql-p)))
                   (if c
                       (car (node-writes c))
                       sym))))
             (r (node)
               (let ((node (copy-node node)))
                 (setf (node-reads node) (map 'list #'newid (node-reads node)))
                 node)))
      (mapc #'c (tpsort-graph graph))
      (setf (graph-nodes graph) (map 'list #'r (graph-nodes graph)))
      (if (graph-outputs graph)
          (progn
            ;; Optimized path
            (setf (graph-outputs graph) (graph-outputs graph)
                  (graph-seen graph) (graph-seen graph)
                  graph (->fast-graph graph))
            (verify-graph graph)
            (setf graph (->graph-with-tpsort graph)))
          (verify-graph graph))
      ;; [TODO] Assert is optional
      (when assert-optimized
        (dolist (x defined)
          (assert (= 1 (count x (graph-nodes graph) :test #'load-eql-p)) () "~a is used multiple times." x)))
      graph)))
;; - When is ready:
;; - Complete ability to remove duplicated computation for
;;  - :< :=
;;  - :MUL
;;  - Index-Components
;; - Optimize Padding/Triu
;; - Write a test for proving :ALLOCATE is placed at once.
;; - A*Bはグラフに一つしか存在しないことを保証すると，EXPR-Scalar-Equivalent-Pを削除できる => Complete Symbolic
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
  (declare (type (or FastGraph Graph) graph))
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
	(etypecase graph
	  (FastGraph
	   (insert-nodes graph
			 (loop for node in (tpsort-graph graph)
			       for new = (read-cache node) if new collect new)))
	  (Graph
	   (setf (graph-nodes graph)
		 (loop for node in (tpsort-graph graph)
		       for new = (read-cache node)
		       if new collect new))))
	(dolist (n (graph-nodes graph))
	  (setf (node-reads n) (map 'list #'r (node-reads n))))))
    (verify-graph graph)
    graph))

(defun minimize-duplicated-symbolic-path (graph)
  (let ((copy-graph (->graph graph)))
    (setf copy-graph (minify-duplicated-alloc copy-graph)
          copy-graph (simplify-dynamic-arithmetic copy-graph))
    (if (typep graph 'FastGraph)
        (progn
          (setf (graph-outputs copy-graph) (graph-outputs graph)
                (graph-seen copy-graph) (graph-seen graph)
                copy-graph (->fast-graph copy-graph))
          copy-graph)
        copy-graph)))

(defun optimize-aasm (graph &key (debug-opt nil) (heavy-opt-threshold 5))
  (fold-constant graph :debug-opt debug-opt)
  (when (>= (length (graph-nodes graph)) heavy-opt-threshold)
    (setf graph (minimize-duplicated-symbolic-path graph)))
  (fuse-duplicated-store graph))
