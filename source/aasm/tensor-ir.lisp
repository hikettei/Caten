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
(defparameter *default-order* (ctx:getenv :DEFAULT_ORDER))
(defparameter *default-float* (ctx:getenv :DEFAULT_FLOAT))
(defparameter *default-int*   (ctx:getenv :DEFAULT_INT))
(defparameter *default-uint*  (ctx:getenv :DEFAULT_UINT))

(defun %alloc (nrank shape stride &key (dtype *default-float*) (id (gensym "TID")) (from nil))
  "Equivalent to `dtype i[shape];`
From can be either of nil or Buffer, if buffer is specified, it loads the content of buffer instead of allocating it."
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
    (emit (make-node :Buffer :Allocate (list id) (append shape stride) :nrank nrank :dtype dtype :from from))))

(defun %salloc (&key (dtype *default-float*) (id (gensym "SID")))
  "Equivalent to: `dtype i;` but nrank=0"
  (declare (type dtype-t dtype)
	   (type symbol id))
  (emit (make-node :Buffer :Allocate (list id) nil :nrank 0 :dtype dtype)))

(defun %load (node value &key (id (gensym "LID")))
  "Equivalent to: `i = initial_value;` where i is a scalar of tensor.
If i is a tensor, %load fills the visible area of i with value."
  (declare (type Node node))
  (assert (eql (node-class node) :Buffer)   ())
  (assert (eql (node-type  node) :Allocate) ())
  (let* ((value (if (numberp value) (dtype/cast value (getattr node :dtype)) value)))
    (emit (make-node :Buffer :Load (list id) (list (node->id node)) :value value))))

(defmethod print-node ((node Node) (id (eql :Allocate)))
  (let ((nrank (getattr node :nrank)))
    (when (and nrank (not (= nrank 0)))
      (format nil "<~a : ~a <- (shape=(~a), stride=(~a)~a)~a>"
	      (node-type node)
	      (render-list (node-writes node))
	      (render-list (subseq (node-reads node) 0 nrank))
	      (render-list (subseq (node-reads node) nrank))
	      (if (getattr node :from)
		  (let ((buffer (getattr node :from)))
		    (if (symbolp buffer)
			(format nil ", from=<~a, Realized Array>" buffer)
			(format nil ", from=<~a Realized Array>" (uiop:symbol-call :caten/avm :buffer-shape buffer))))
		  "")
	      (render-attrs node :except-for `(:from :_loop_bound_Nodes :_loop_bound_nodes_type :_no_group_realize_on_vm))))))

(defun %store (x y &key (id (gensym "LID")) (reduction nil))
  "Equivalent to x = y;"
  (declare (type node x y))
  (emit (make-node :Buffer :Store (list id) (list (node->id x) (node->id y)) :reduction reduction)))

(defun %uconst (value &key (dtype *default-uint*))
  "Creates an unsigned integer"
  (%load (%salloc :dtype dtype) value))
(defun %iconst (value &key (dtype *default-int*))
  "Creates a signed integer"
  (%load (%salloc :dtype dtype) value))
(defun %fconst (value &key (dtype *default-float*))
  "Creates a float const"
  (%load (%salloc :dtype dtype) value))

(defun %stride (shape order)
  "Compute the stride based on permute and shape."
  (declare (type list shape)
	   (type (member :row :column) order))
  (if (eql order :row)
      (%row-major-calc-strides shape)
      (%column-major-calc-strides shape)))

(defun %shape (shape &key (dtype *default-uint*))
  "Initialize the shape."
  (declare (type list shape) (type dtype-t dtype))
  (flet ((const (n) (if (node-p n) n (%load (%salloc :dtype dtype) n))))
    (map 'list #'const shape)))

(defun %make-tensor (shape &key (dtype *default-float*) (order *default-order*) (id (gensym "TID")) (from nil))
  "A useful wrapper for %alloc. it computes stride based on order.
%make-tensor is used to allocate the initial tensor, later weights are loaded.
Typed: <Allocate OUT_ID <- (,@shape ,@stride) where from=from dtype=dtype nrank=nrank>"
  (declare (type list shape)
	   (type dtype-t dtype)
	   (type (member :row :column) order))
  (assert (every #'(lambda (x) (or (symbolp x) (integerp x) (node-p x))) shape)
	  ()
	  "%make-tensor: Shape is designed as symbol (existing in the graph), integer or node.~%butgot ~a" shape)
  (when (= 0 (length shape))
    (assert (null from) () ":from for a scalar creation is ignored. Use %load instead.")
    (return-from %make-tensor (%salloc :dtype dtype :id id)))
  (%alloc (length shape) (%shape shape) (%stride shape order) :dtype dtype :id id :from from))

(defun %index-components (x shape &key (id (gensym "IID")))
  "the equivalent to doing: `for (int i=x.view.from;i<x.view.to;i+=x.view.by) { id[i] = i; }`"
  (declare (type node x) (type list shape))
  (emit (make-node :Indexing :Index-Components (list id) `(,(node->id x) ,@(map 'list #'node->id (%stride shape :row))))))
