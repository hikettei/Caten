(in-package :caten/aasm)

(defparameter *replaceable-ops* `(:ADD :NEG :MUL :RECIP :IDIV) "A graph consisted of these operations are subject to this rewriting rule.")

(defsimplifier
    (fuse-duplicated-store :speed 0)
    ((:Store ((:Allocate (~ s1) :nrank nrank :dtype dtype1) (:Allocate (~ s2) :dtype dtype2)))
     ->
     ((node graph)
      (when (and (eql dtype1 dtype2) (equal s1 s2))
	(make-node :Buffer :Allocate (node-writes node) s1 :nrank nrank :dtype dtype1)))))

(defun minify-duplicated-alloc (graph &key (assert-optimized nil))
  "Consider the following graph structure:
```
Z = load(1.0)
D = load(1.0)
```
This function will rewrite the graph to:
```
Z = load(1.0)
D = Z
```
"
  (declare (type Graph graph) (optimize (speed 3)))
  (let ((replaceable-map (make-hash-table)) (users-map (make-hash-table)) (defined) (id->node (make-hash-table)))
    (labels ((invalid-load-p (from node)
               (if (eql (node-type node) :VIEW)
                   (eql from (the symbol (car (node-reads node))))
                   nil))
             (load-eql-p (node1 node2)
               (and (eql (node-type node1) :LOAD) (eql (node-type node2) :LOAD)
                    (eql (getattr node1 :value) (getattr node2 :value))
                    (let ((alloc1 (gethash (car (node-reads node1)) replaceable-map))
                          (alloc2 (gethash (car (node-reads node2)) replaceable-map))
                          (users1 (gethash (car (node-writes node1)) users-map))
                          (users2 (gethash (car (node-writes node2)) users-map)))
                      ;; Do not remove LOAD for accumlation
                      (when (or (find-if #'(lambda (x) (invalid-load-p (car (node-writes node1)) x)) (the list users1))
                                (find-if #'(lambda (x) (invalid-load-p (car (node-writes node2)) x)) (the list users2)))
                        (return-from load-eql-p nil))
                      (and alloc1 alloc2 (eql (the keyword (getattr alloc1 :dtype)) (the keyword (getattr alloc2 :dtype)))))))
             (c (node)
               (when (not (= (length (node-writes node)) 1)) (return-from c))
               (setf (gethash (car (node-writes node)) id->node) node)
               (dolist (r (node-reads node))
                 (push node (gethash r users-map)))
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

(defun rewrite-unique-computation-path (graph &key (arithmetic *replaceable-ops*) (rewrite-output nil) (table (make-hash-table)) &aux (changed-p nil))
  "Consider the following graph structure:
```
Z = A * B
D = A * B
```
This function will rewrite the graph to:
```
Z = A * B
D = Z
```
"
  (declare (type Graph graph) (optimize (speed 3)))
  (assert (not (typep graph 'FastGraph)))
  (let ((defined) (alias-map (make-hash-table)))
    (labels ((node-eq (a b)
               (declare (type node a b))
               (when (not (eql (node-type a) (node-type b))) (return-from node-eq nil))
               (case (node-type a)
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
                   (when (find (the symbol (car (node-writes node))) (the list (graph-outputs graph)))
                     (if rewrite-output
                         (setf (gethash (car (node-writes node)) table) (car (node-writes prev-defined)))
                         (return-from apply-cache node)))
                   ;; Rewrite (node-writes node) -> (node-writes pref-defined)
                   (setf (gethash (car (node-writes node)) alias-map) (car (node-writes prev-defined))
                         changed-p t)
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
      (if changed-p
          (values (rewrite-unique-computation-path graph :rewrite-output rewrite-output :table table) table)
          (values graph table)))))

(defun minimize-duplicated-symbolic-path (graph &key (rewrite-output nil))
  "An entry point for duplicated graph path optimization."
  (let ((copy-graph (->graph graph)))
    (setf copy-graph (minify-duplicated-alloc copy-graph)) ;; Make equivalent toplevel to be exist only once!
    ;; Simply just trace and replace them recursively!
    (multiple-value-bind (copy-graph table) (rewrite-unique-computation-path copy-graph :rewrite-output rewrite-output)
      (if (typep graph 'FastGraph)
          (progn
            (setf (graph-outputs copy-graph) (graph-outputs graph)
                  (graph-seen copy-graph) (graph-seen graph)
                  copy-graph (->fast-graph copy-graph))
            (values copy-graph table))
          (values copy-graph table)))))

(defun optimize-aasm (graph &key (debug-opt nil) (heavy-opt-threshold 25))
  (fold-constant graph :debug-opt debug-opt)
  (when (>= (length (graph-nodes graph)) heavy-opt-threshold)
    (setf graph (minimize-duplicated-symbolic-path graph)))
  (fuse-duplicated-store graph))
