(in-package :caten/aasm)

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
     ((node graph) (sfold-view ss node graph nrank broadcast permute tr))))

(defun fold-constant (graph &key (debug-opt nil))
  (apply-fold-constant graph :debug-opt debug-opt)
  (fuse-vmops graph :debug-opt debug-opt)
  graph)
