(in-package :caten/aasm)

(defpattern number (x) `(guard ,x (numberp ,x)))
(defpattern boolean (x) `(guard ,x (typep ,x 'boolean)))
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

(defpattern Const (x dtype)
  `(or
    (<Rule> :Load ((:Allocate () :nrank 0 :dtype ,dtype)) :value (number ,x))
    (and (<Rule> :Allocate () :nrank 0 :dtype ,dtype) (<> ,x 0))))
(defpattern Var (x dtype)
  `(or
    (<Rule> :Load ((:Allocate () :nrank 0 :dtype ,dtype)) :value ,x)
    ,@(when (equal x `(= 0))
        `((and (<Rule> :Allocate () :nrank 0 :dtype ,dtype) (<> ,x 0))))))

(defun Const (x dtype) (with-context-nodes (_ (%load (%salloc :dtype dtype) x))))
(defpattern Bool (x) `(<Rule> :Load ((:Allocate () :nrank 0 :dtype :bool)) :value (boolean ,x)))
(defpattern Cast (x dtype) `(<Rule> :Cast ((:Allocate () :nrank 0 :dtype ,dtype) (Const ,x))))

(declaim (inline scalar-p))
(defun scalar-p (id graph)
  (declare (type graph graph) (optimize (speed 3)))
  (let* ((load (id->value graph id))
         (alloc (and load (id->value graph (car (node-reads load))))))
    (and load alloc (eql (node-type load) :LOAD) (eql (node-type alloc) :Allocate)
         (= (the fixnum (getattr alloc :nrank)) 0) (numberp (getattr load :value))
         (getattr load :value))))

(declaim (notinline sfold-index-components sfold-allocate sfold-view))
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
      (print graph)
      (print new-shape)
      (unless (equal new-shape ss)
        (list
	 (make-node
          :Buffer :Allocate
          (node-writes node) new-shape
          :nrank nrank :dtype dtype :from from))))))

(defun sfold-view (ss node graph nrank broadcast permute)
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
	    :nrank nrank :broadcast broadcast :permute permute)))))))
;; [TODO] Logical AND/XOR/OR for threefry2x32
(defsimplifier
    (apply-fold-constant :speed 1)
    ((:Add ((Const x dtype) (Const y _))) -> (Const (+ x y) dtype))
    ((:Mul ((Const x dtype) (Const y _))) -> (Const (* x y) dtype))
    ((:Neg ((Const x dtype))) -> (Const (- x) dtype))
    ((:Recip ((Const x dtype))) -> (Const (/ x) dtype))
    ((:GCD ((Const x dtype) (Const y _))) -> (Const (gcd x y) dtype))
    ((:< (_ (Const x _) (Const y _))) -> (Const (< x y) :bool))
    ((:!= (_ (Const x _) (Const y _))) -> (Const (not (= x y)) :bool))
    ((:MAX ((Const x dtype) (Const y _))) -> (Const (max x y) dtype))
    ((:NOT ((Bool x))) -> (Const (not x) :bool))
    ((:AND ((Bool x) (Bool y))) -> (Const (and x y) :bool))
    ((:OR ((Bool x) (Bool y))) -> (Const (or x y) :bool))
    ((:WHERE ((Bool x) (Const y dtype) (Const z _))) -> (Const (if x y z) dtype))
    ;; 0 * x
    ((:Mul (_ (Var (= 0) _))) -> ((node graph) (reinitialize-tensor graph node)))
    ((:Mul ((var (= 0) _) _)) -> ((node graph) (reinitialize-tensor graph node)))
    ;; 1 * x
    ((:Mul (x (Var (= 1) _))) -> x)
    ((:Mul ((var (= 1) _) x)) -> x)
    ;; 0 + x
    ((:Add (x (Var (= 0) _))) -> x)
    ((:Add ((Var (= 0) _) x)) -> x))

(defsimplifier
    (fuse-vmops :speed 1)
    ((:INDEX-COMPONENTS (~ ss)) -> ((node graph) (sfold-index-components ss node graph)))
    ((:Allocate (~ ss) :nrank (guard nrank (> 0)) :dtype dtype :from from)
     ->
     ((node graph) (sfold-allocate ss node graph nrank dtype from)))
    ((:View (~ ss) :broadcast broadcast :nrank nrank :permute permute)
     ->
     ((node graph) (sfold-view ss node graph nrank broadcast permute))))

(defun fold-constant (graph)
  (apply-fold-constant graph)
  (fuse-vmops graph)
  graph)
