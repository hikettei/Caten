(in-package :caten/ir)

(defpattern number (x) `(guard ,x (numberp ,x)))
(defpattern boolean (x) `(guard ,x (typep ,x 'boolean)))
(defpattern dtype-float-p (dtype) `(guard ,dtype (caten/common.dtype:dtype/floatp ,dtype)))
(defnode (:Tmp :_TmpScalarConst) () "" :slots ((dtype))) ;; TODO: delete

(defun reinitialize-tensor (graph node &aux (id (car (node-writes node))))
  (declare (type graph graph) (optimize (speed 3)))
  (multiple-value-bind (nrank shape stride dtype views)
      (infer-tensor-info graph id)
    ;; [TODO] Fix why shape infer fails
    (when (or (null nrank) (null dtype))
      (return-from reinitialize-tensor))
    (flet ((->find (x) (or (id->value graph x) x)))
      (setf shape (map 'list #'->find shape)
	    stride (map 'list #'->find stride)))
    (let ((viewed (every #'identity views)))
      (if (= (the fixnum nrank) 0)
	  (with-context-nodes (m1 (%salloc :dtype dtype :id id)))
	  (with-context-nodes
	    (m1 (%alloc nrank shape stride :dtype dtype :id (if viewed (gensym "TID") (node->id node))))
	    (m2 (if viewed (%view m1 shape (nth 0 views) (nth 1 views) (nth 2 views) (nth 3 views) stride :id (node->id node)) m1)))))))
;; :Cast Instead of :Load
(defpattern Const (x dtype)
  `(or
    (<Rule> :Load ((:Allocate () :nrank 0 :dtype ,dtype)) :value (number ,x))
    (and (<Rule> :Allocate () :nrank 0 :dtype ,dtype) (<> ,x 0))))

(defpattern Var (x dtype)
  `(or
    (<Rule> :Load ((:Allocate () :nrank 0 :dtype ,dtype)) :value ,x)
    ,@(when (equal x `(= 0))
        `((<Rule> :Allocate () :nrank 0 :dtype ,dtype)))))

(defun Const (x dtype) (with-context-nodes (_ (%load (%salloc :dtype dtype) x))))
(defpattern Bool (x) `(<Rule> :Load ((:Allocate () :nrank 0 :dtype :bool)) :value (boolean ,x)))
(defpattern Cast (x dtype) `(<Rule> :Cast ((:Allocate () :nrank 0 :dtype ,dtype) (Const ,x))))
(defun Purged (node x)
  (make-node :Buffer :Store (node-writes node) (list (car (node-writes node)) x)))

(declaim (inline scalar-p))
(defun scalar-p (id graph)
  (declare (type graph graph) (optimize (speed 3)))
  (let* ((load (id->value graph id))
         (alloc (and load (id->value graph (car (node-reads load))))))
    (and load alloc (eql (node-type load) :LOAD) (eql (node-type alloc) :Allocate)
         (= (the fixnum (getattr alloc :nrank)) 0) (numberp (getattr load :value))
         (getattr load :value))))

(declaim (inline sfold-index-components sfold-allocate sfold-view))
(defun sfold-index-components (ss node graph)
  (declare (type list ss) (type node node) (type graph graph)
           (optimize (speed 3)))
  (when ss
    (let* ((ss-nodes (map 'list #'(lambda (x) (id->value graph x)) ss))
	   (new-shape (loop for ss-node in (cdr ss-nodes)
			    for ss-val  in (cdr ss)
                            for val = (and ss-node (scalar-p ss-val graph))
			    if val
			      collect val
			    else
			      collect ss-val)))
      (unless (equal new-shape (cdr ss))
        (list
	 (make-node
          :Indexing :INDEX-COMPONENTS
          (node-writes node) `(,(car ss) ,@new-shape)))))))

(defun sfold-allocate (ss node graph nrank dtype from)
  (declare (type list ss) (type node node) (type graph graph) (optimize (speed 3)))
  (when ss
    (let* ((ss-nodes (map 'list #'(lambda (x) (id->value graph x)) ss))
	   (new-shape (loop for ss-node in ss-nodes
			    for ss-val  in ss
                            for val = (and ss-node (scalar-p ss-val graph))
                            if val
                              collect val
			    else
	                      collect ss-val)))
      (unless (equal new-shape ss)
        (list
	 (make-node
          :Buffer :Allocate
          (node-writes node) new-shape
          :nrank nrank :dtype dtype :from from))))))

(defun sfold-view (ss node graph nrank broadcast permute tr)
  (declare (type list ss) (type node node) (type graph graph) (optimize (speed 3)))
  (when ss
    (let* ((ss-nodes (map 'list #'(lambda (x) (id->value graph x)) ss))
	   (new-views (loop for ss-node in ss-nodes
			    for ss-val  in ss
                            for val = (and ss-node (scalar-p ss-val graph))
                            if val
                              collect val
			    else
			      collect ss-val)))
      (when (symbolp (car new-views))
	(unless (equal new-views ss)
          (list
	   (make-node
	    :Buffer :View
	    (node-writes node) new-views
	    :nrank nrank :broadcast broadcast :permute permute :tr tr)))))))

(defun sfold (ss node graph)
  (when ss
    (let ((node (copy-node node)))
      (setf (node-id node) (node->id node))
      (let* ((ss-nodes (map 'list #'(lambda (x) (id->value graph x)) ss))
             (new-args (loop for ss-node in ss-nodes
                             for ss-val  in ss
                             for val = (and ss-node (scalar-p ss-val graph))
                             if val collect val else collect ss-val)))
        (unless (equal new-args ss)
          (setf (node-reads node) new-args)
          node)))))
;; [TODO] Logical AND/XOR/OR for threefry2x32
(defsimplifier
    (apply-fold-constant :speed 1)
    ;; (-(a)+(a+c)) -> c
    ((:Add ((:Neg ((Var x dtype1))) (:Add ((Var y dtype2) (Var z dtype3)))))
     ->
     ((node graph)
      (when (and (equal x y) (eql dtype1 dtype2))
        (Const z dtype3))))
    ;; (-(a)+((a+c)+z)) -> (c+z)
    ((:Add ((:Neg ((Var x dtype1))) (:Add ((:Add ((Var y dtype2) (Var z dtype3))) (Var w dtype4)))))
     ->
     ((node graph)
      (when (and (equal x y) (eql dtype1 dtype2))
        ;; c+z
        (with-context-nodes
          (z (%load (%salloc :dtype dtype3) z))
          (w (%load (%salloc :dtype dtype4) w))
          (out (%add z w))))))
    ((:Mod ((Const x dtype) (Const y _))) -> (Const (mod x y) dtype))
    ((:Cast (_ (Const x _)) :dtype dtype) -> (Const (caten/common.dtype:dtype/cast x dtype) dtype))
    ((:Add ((Const x dtype) (Const y _))) -> (Const (+ x y) dtype))
    ((:Mul ((Const x dtype) (Const y _))) -> (Const (* x y) dtype))
    ((:Mul ((Const x dtype) (:Recip ((Const y _))))) -> (Const (/ x y) dtype))
    ((:Neg ((Const x dtype))) -> (Const (- x) dtype))
    ((:Recip ((Const x (dtype-float-p dtype)))) -> (Const (/ x) dtype))
    ((:GCD ((Const x dtype) (Const y _))) -> (Const (gcd x y) dtype))
    ((:< (_ (Const x _) (Const y _))) -> (Const (< x y) :bool))
    ((:!= (_ (Const x _) (Const y _))) -> (Const (not (= x y)) :bool))
    ((:MAX ((Const x dtype) (Const y _))) -> (Const (max x y) dtype))
    ((:NOT ((Bool x))) -> (Const (not x) :bool))
    ((:AND ((Bool x) (Bool y))) -> (Const (and x y) :bool))
    ((:OR ((Bool x) (Bool y))) -> (Const (or x y) :bool))
    ((:WHERE ((Bool x) (Const y dtype) (Const z _))) -> (Const (if x y z) dtype))
    ((:IDIV ((Const x dtype) (Const y _))) -> (Const (floor x y) dtype))
    ((:Recip ((Var (= 1) dtype))) -> (Const 1 dtype))
    ((:Mul (_ (Var (= 0) _))) -> ((node graph) (reinitialize-tensor graph node)))
    ((:Mul ((Var (= 0) _) _)) -> ((node graph) (reinitialize-tensor graph node)))
    ((:Mul (x (Var (= 1) _))) -> x)
    ((:Mul ((Var (= 1) _) x)) -> x)
    ((:Add (x (Var (= 0) _))) -> x)
    ((:Add ((Var (= 0) _) x)) -> x))

(defsimplifier
    (fuse-vmops :speed 1)
    ((:INDEX-COMPONENTS (~ ss)) -> ((node graph) (sfold-index-components ss node graph)))
    ((:Allocate (~ ss) :nrank (guard nrank (> 0)) :dtype dtype :from from)
     ->
     ((node graph) (sfold-allocate ss node graph nrank dtype from)))
    ((:View (~ ss) :broadcast broadcast :nrank nrank :permute permute :tr tr)
     ->
     ((node graph) (sfold-view ss node graph nrank broadcast permute tr)))
    ((:Range (~ ss)) -> ((node graph) (sfold ss node graph)))
    ;; TODO: IF
    ((:Aref (name idx)) -> ((node graph) (sfold (list name idx) node graph))))

(defun fold-constant (graph &key (debug-opt nil))
  (apply-fold-constant graph :debug-opt debug-opt)
  (fuse-vmops graph :debug-opt debug-opt)
  graph)

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
