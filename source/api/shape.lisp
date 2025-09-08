(in-package :caten/api)
;; ~~ NumpySemantic Broadcast ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun align-left (shapes)
  (let ((max-dim (reduce #'max (map 'list #'length shapes))))
    (values
     max-dim
     (loop for shape in shapes
           collect (append (make-list (- max-dim (length shape)) :initial-element 1) shape)))))

(defun broadcast-shape (shapes)
  (multiple-value-bind (max-dim shapes) (align-left shapes)
    (loop for dim upfrom 0 below max-dim
          for sdim-sizes = (map 'list #'(lambda (x) (nth dim x)) shapes)
          collect (or (find-if #'(lambda (x) (not (eql x 1))) sdim-sizes) 1))))

(defun broadcast-elwise (a b)
  (declare (type Tensor a b))
  (cond
    ((= 0 (tensor-nrank a) (tensor-nrank b))
     (values a b))
    (T
     (let ((broadcasted (broadcast-shape (list (tensor-shape a) (tensor-shape b)))))
       (values (!expand a broadcasted) (!expand b broadcasted))))))
;; ~~ View Parser ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftype axis-t () `(or number symbol Tensor))
(defstruct (ViewRange
	    (:constructor make-vrange (from to by broadcast size subscript
				       &aux
					 (from (->size from))
					 (to (->size to))
					 (by (->size by))
					 (size (->size size)))))
  (from from :type Tensor) (to to :type Tensor)
  (by by :type Tensor) (broadcast broadcast :type boolean)
  (size size :type Tensor) (subscript subscript))

(defun vrange-size (vrange)
  (declare (type ViewRange vrange))
  (!idiv (!sub (viewrange-to vrange) (viewrange-from vrange)) (viewrange-by vrange)))

(defun parse-view-subscript (g size subscript)
  (declare (type TensorGraph g) (type axis-t size))
  (labels ((->size1 (value)
             (if (tensor-p value)
                 value
                 (if (id->value g value)
                     (%%make-tensor g value)
                     (if (numberp value)
                         value
                         (->size value)))))
           (normalize (x) (if (and (numberp x) (< x 0)) (!add (->size1 size) (->size1 x)) (->size1 x)))
	   (1p (x) (if (tensor-p x) (!add x (->size1 1)) (!add (->size1 x) (->size1 1)))))
    (ematch subscript
      ((list :~ n) (make-vrange 0 (normalize n) 1 t size subscript));; broadcasting (:~ N)
      ((eql t)  (make-vrange 0 (->size1 size) 1 nil size subscript)) ;; nothing
      ((guard x (typep x 'axis-t)) (make-vrange (normalize x) (1p (normalize x)) 1 nil (->size1 size) subscript)) ;; A[i]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) 1 nil (->size1 size) subscript)) ;; A[from:to]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)) (guard by (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) by nil (->size1 size) subscript))))) ;; A[from:to:by]
;; ~~ Reduction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun parse-reduce-axes (x lst)
  (declare (type tensor x))
  (ematch lst
    ((eql t)
     (values (loop for i in (tensor-shape x) collect 1) (loop for i in (tensor-shape x) collect `(:~ ,i)) (range 0 (length (tensor-shape x)))))
    ((guard axis (numberp axis))
     (let ((axis (normalize-axis x axis))
	   (shape-after (tensor-shape x))
	   (view-after (loop for i in (tensor-shape x) collect t)))
       (setf (nth axis shape-after) 1
	     (nth axis view-after) `(:~ ,(nth axis (tensor-shape x))))
       (values shape-after view-after (list axis))))
    ((list* axes)
     (let ((axes (map 'list #'(lambda (a) (normalize-axis x a)) axes))
	   (shape-after (tensor-shape x))
	   (view-after  (loop for i in (tensor-shape x) collect t)))
       (dolist (axis axes)
	 (setf (nth axis shape-after) 1
	       (nth axis view-after) `(:~ ,(nth axis (tensor-shape x)))))
       (values shape-after view-after axes)))))
;; ~~ ShapeError Handling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *restart-point* nil)
(defclass Restart-Point nil nil)
;; [TODO] Prefer @caten.trace reader macro!
;; [TODO] caten/lang
;; - [ ] Different Symbols will never intersect (i.e: N and M = empty)
;; - [ ] Use UnionMap to identity two symbolics
;; - [ ] TensorGraph.equalities (A, B)
;; - [ ] define-compiler-macro
;; - [ ] (defun x (a) (declare (type (Tensor ~ M N) a)))
(defmacro %internal-caten/defun (name lambda-list &body body)
  `(defun ,name (,@lambda-list)
     ,@body))
;; ~~ Early View Simplifier ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defsimplifier
    (%graph-simplify-views :speed 0)
    ;; Extra !contiguous
    ((:MOVE ((:ALLOCATE (~ _)) (:ALLOCATE (~ _))) :reduction (guard r (null r)))
     ->
     ((node graph)
      ;; actually X == Y is asserted by running graph-infer-type-relay
      (multiple-value-bind (x y) (values (id->value graph (car (node-reads node))) (id->value graph (second (node-reads node))))
        (when (and x y (equal (cdr (node-reads x)) (cdr (node-reads y))) (null (getattr x :from)) (null (getattr y :from)))
          y))))
    ;; A case for view is creating identity view from contiguous.
    ((:VIEW (~ args))
     ->
     ((node graph)
      (let ((alloc (id->value graph (car args))))
        (when (eql (node-type alloc) :ALLOCATE)
          (let ((alloc-rel (car (relay-writes (read-type-relay alloc))))
                (view-rel  (car (relay-writes (read-type-relay node)))))
            (when (tensor-relay-equal alloc-rel view-rel)
              alloc))))))
    ;; Typical VIEW+VIEW, solution is found by polyhedral model
    )

(defun graph-simplify-views (graph)
  (declare (type TensorGraph graph))
  (graph-infer-type-relay graph)
  (%graph-simplify-views graph)
  graph)
