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

(defparameter *replaceable-ops* `(:ADD :NEG :MUL :RECIP :IDIV))
;; [TODO] Rewrite to a single function. (which is possible)
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

(defun simplify-dynamic-arithmetic1 (graph &key (arithmetic *replaceable-ops*))
  "TODO: docs"
  (declare (type Graph graph) (optimize (speed 3)))
  (assert (not (typep graph 'FastGraph)))
  (let ((seen (make-hash-table))) ;; seen records an ID and node which is replaceable.
    (labels ((trace-seen-graph (id)

               )
             (apply-cache (node)
               ;; Scalar Allocation for an entry point of seen
               (case (node-type node)
                 (:Allocate
                  (when (null (node-reads node))
                    (setf (gethash (car (node-writes node)) seen) node)
                    (return-from apply-cache)))
                 (otherwise
                  (unless (find (node-type node) arithmetic) (return-from apply-cache))
                  ;; All parents are seen, or number => label as a cacheable node.
                  (assert (= 1 (length (node-writes node)))) ;; arithmetic == not an custom kernel.
                  (when (every #'(lambda (x) (or (numberp x) (gethash x seen))) (node-reads node))
                    ;; In order to remove duplicated computation, 
                    (setf (gethash (car (node-writes node)) seen) node))))))
      ;; Labelling a replaceable node in the seen hash table.
      (mapc #'apply-cache (tpsort-graph graph))
      
      (loop for node in (graph-nodes graph)
            ;; except for seen node
            ))))

(defun rewrite-unique-computation-path (graph &key (arithmetic *replaceable-ops*))
  (declare (type Graph graph) (optimize (speed 3)))
  (assert (not (typep graph 'FastGraph)))
  (let ((defined) (alias-map (make-hash-table)))
    (labels ((node-eq (a b)
               (declare (type node a b))
               (when (not (eql (node-type a) (node-type b))) (return-from node-eq nil))
               (case (node-type a)
                 ;; [TODO] Add :LOAD
                 ((:ADD :MUL)
                  ;; Assume BinaryOps ADD/MUL has two operands, so reverse works.
                  (or (equal (node-reads a) (node-reads b))
                      (equal (node-reads a) (reverse (node-reads b)))))
                 ((:NEG :RECIP :IDIV)
                  (equal (node-reads a) (node-reads b)))
                 (otherwise nil)))
             (apply-cache (node)
               ;; Needs to be a vmop
               (when (not (= 1 (length (node-writes node)))) (return-from apply-cache node))
               (when (null (find (node-type node) (the list arithmetic))) (return-from apply-cache node))
               (let ((prev-defined (find node defined :test #'node-eq)))
                 (when prev-defined
                   ;; Rewrite (node-writes node) -> (node-writes pref-defined)
                   (setf (gethash (car (node-writes node)) alias-map) (car (node-writes prev-defined)))
                   (return-from apply-cache nil))
                 (push node defined)
                 node))
             (r (sym) (gethash sym alias-map sym)))
      (setf (graph-nodes graph)
            (loop for node in (tpsort-graph graph)
                  for new = (apply-cache node)
                  if new do
                    (setf (node-reads new) (map 'list #'r (node-reads new)))
                    and collect new))
      graph)))
;; A*B, A+Bはかかんなのでreverseして等しくてもOK
;; Nodeのてっぺんを揃えれば連鎖的に解消できる？
(defun simplify-dynamic-arithmetic (graph &key (arithmetic *replaceable-ops*))
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
			(cache-result (gethash key cache (car (node-writes node)))))
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
    (rewrite-unique-computation-path graph)
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
