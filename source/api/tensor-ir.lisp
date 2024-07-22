(in-package :caten/api)

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
;; ================================================================================================
(defparameter *default-float* :float32)
(deftype dtype-t ()
  "A list of available keywords as a dtype"
  `(and keyword
	(member
	 :float64 :float32 :float64
	 :uint64 :uint32 :uint16 :uint8
	 :int64 :int32 :int16 :int8)))

(defun %alloc (nrank shape stride &key (dtype *default-float*) (id (gensym "TID")))
  "TODO: Docs
TODO: Add device"
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
  (emit (make-node :Buffer :Allocate (list id) (append shape stride) :nrank nrank :dtype dtype)))

(defun %scalar (&key (dtype *default-float*) (id (gensym "SID")))
  "Equivalent to: `dtype i;`"
  (declare (type dtype-t dtype)
	   (type symbol id))
  (emit (make-node :Buffer :Allocate (list id) nil :nrank 0 :dtype dtype)))

(defun %load (node value &key (id (gensym "LID")))
  "Equivalent to: `i = initial_value;`"
  (declare (type Node node))
  (assert (eql (node-class node) :Buffer)   ())
  (assert (eql (node-type  node) :Allocate) ())
  (emit (make-node :Buffer :Load (list id) (list (node-id node)) :value value)))

