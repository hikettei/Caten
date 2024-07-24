(in-package :caten/aasm)

;; == Tensor =====================================================================================
;; Tensor is an air graph defined as:
;;  Allocate: [tensor_name] <- (shape0 shape1 ... shape_n stride0 stride1 ... strideN)
;;             where nrank = ... dtype = ...
;;
;; During training, nrank is fixed. but shape0/shape1 can be adjusted by connecting another graph
;; which produces scalar outputs.
;;
;; Set nrank=0 to create a scalar
;; Set nrank>=1 to create a tensor
;; make-node must be wrapped with emit when producing outputs
;; and with-context can recognise it
;; ================================================================================================
(defparameter *default-float* :float32)
(defparameter *default-int* :int32)
(defparameter *default-uint* :uint32)

(deftype dtype-t ()
  "A list of available keywords as a dtype"
  `(and keyword
	(member
	 :float64 :float32 :float64
	 :uint64 :uint32 :uint16 :uint8
	 :int64 :int32 :int16 :int8)))

(defun %alloc (nrank shape stride &key (dtype *default-float*) (id (gensym "TID")))
  "Equivalent to `dtype i[shape];`"
  (declare (type fixnum nrank)
	   (type list shape stride)
	   (type dtype-t dtype)
	   (type symbol id))
  (assert (every #'node-p shape) () "Assertion Failed: Shapes must be a list of Node.")
  (assert (every #'node-p stride) () "Assertion Failed: Strides must be a list of Node.")
  (assert (= nrank (length shape) (length stride)) () "Assertion Failed: the rank must be determined before the compilation.
nrank=~a
shape=~a
stride=~a" nrank shape stride)
  (multiple-value-bind (shape stride)
      (values
       (map 'list #'node->id shape)
       (map 'list #'node->id stride))
    (emit (make-node :Buffer :Allocate (list id) (append shape stride) :nrank nrank :dtype dtype))))

(defun %salloc (&key (dtype *default-float*) (id (gensym "SID")))
  "Equivalent to: `dtype i;` but nrank=0"
  (declare (type dtype-t dtype)
	   (type symbol id))
  (emit (make-node :Buffer :Allocate (list id) nil :nrank 0 :dtype dtype)))

(defun %load (node value &key (id (gensym "LID")))
  "Equivalent to: `i = initial_value;`"
  (declare (type Node node))
  (assert (eql (node-class node) :Buffer)   ())
  (assert (eql (node-type  node) :Allocate) ())
  (assert (eql (getattr node :nrank) 0) () "%load is only applied to scalar buffers.")
  (emit (make-node :Buffer :Load (list id) (list (node->id node)) :value value)))

(defun %store (x y &key (id (gensym "LID")))
  "Equivalent to x = y;"
  (declare (type node x y))
  (emit (make-node :Buffer :Store (list id) (list (node->id x) (node->id y)))))

(defun %uconst (value &key (dtype *default-uint*))
  "Creates an unsigned integer"
  (%load (%salloc :dtype dtype) value))
(defun %iconst (value &key (dtype *default-int*))
  "Creates a signed integer"
  (%load (%salloc :dtype dtype) value))
(defun %fconst (value &key (dtype *default-float*))
  "Creates a float const"
  (%load (%salloc :dtype dtype) value))

(defun %stride (shape permute &key (dtype *default-uint*))
  "Compute the stride based on permute and shape."
  (declare (type list shape permute)
	   (type dtype-t dtype))
  (let* ((n (length shape))
	 (strides (make-list n)))
    (flet ((const (n) (if (node-p n) n (%load (%salloc :dtype dtype) n))))
      (setf (nth (first (reverse permute)) strides) (progn (const 1)))
      ;; Calculate strides for other dimensions in reverse permuted order
      (do ((i (- n 2) (- i 1))) ((< i 0) nil)
	(setf (nth (nth i permute) strides)
              (%mul (const (nth (nth (1+ i) permute) strides)) (const (nth (nth (1+ i) permute) shape)))))
      (mapcar (lambda (i) (nth i strides)) (loop for i from 0 below n collect i)))))

(defun %shape (shape &key (dtype *default-uint*))
  "Initialize the shape."
  (declare (type list shape) (type dtype-t dtype))
  (flet ((const (n) (if (node-p n) n (%load (%salloc :dtype dtype) n))))
    (map 'list #'const shape)))

(defun %make-tensor (shape &key (dtype *default-float*) (order :row) (id (gensym "TID")))
  "A useful wrapper for %alloc. it computes stride based on order.
%make-tensor is used to allocate the initial tensor, later weights are loaded."
  (declare (type list shape)
	   (type dtype-t dtype)
	   (type (member :row :column) order))
  (assert (every #'(lambda (x) (or (symbolp x) (integerp x) (node-p x))) shape)
	  ()
	  "%make-tensor: Shape is designed as symbol (existing in the graph), integer or node.")
  (assert (>= (length shape) 1) () "%make-tensor is only used to initialize a tensor, not a scalar.")
  (let ((permute (range 0 (length shape))))
    (when (eql order :column)
      (setf permute (reverse permute)))
    (%alloc (length shape) (%shape shape) (%stride shape permute) :dtype dtype :id id)))

(defun %index-components (x &key (id (gensym "IID")))
  "the equivalent to doing: `for (int i=x.view.from;i<x.view.to;i+=x.view.by) { id[i] = i; }`"
  (declare (type node x))
  (emit (make-node :Indexing :Index-Components (list id) (list (node->id x)))))
