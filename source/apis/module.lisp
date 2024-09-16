(in-package :caten/apis)

(defclass Module (Func)
  ((outputs :initform nil :accessor module-outputs)
   (lower-outputs :initform nil :accessor module-lower-outputs)
   (impl-iseq :initform nil :accessor module-impl-iseq)
   (attrs :initform nil :initarg :attrs :accessor module-attrs)
   (sv4bw :initform nil :accessor module-sv4bws))
  (:documentation "See: defmodule"))
(defgeneric impl (module &rest inputs) (:documentation "See: defmodule"))
(defmethod forward :around ((module Module) &rest inputs)
  (declare (ignore inputs))
  (let ((outputs (multiple-value-list (call-next-method))))
    (setf (module-outputs module) outputs)
    (apply #'values outputs)))
(defmethod impl :around ((module Module) &rest inputs)
  (declare (ignore inputs))
  (let ((outputs (multiple-value-list (call-next-method))))
    (setf (module-lower-outputs module) outputs)
    (when (and (every #'(lambda (x) (every #'numberp (shape x))) (module-outputs module))
	       (every #'(lambda (x) (every #'numberp (shape x))) (module-lower-outputs module)))
      (when (not (every #'(lambda (x y) (equal (shape x) (shape y))) (module-outputs module) (module-lower-outputs module)))
	(warn "Detected the inconsistent shape-inference during lowering ~a.~%Please resolve this first before investigating runtime-error.~%Expected(Forward):~%~a~%Lowered:~%~a"
	      module
	      (module-outputs module)
	      (module-lower-outputs module))))
    (apply #'values outputs)))
(defmacro defmodule ((name ((&rest constructor-args) &rest attrs) &key (where nil) (direct-superclasses nil))
		     (&rest slots)
		     &key (documentation "") (impl nil) (forward nil) (backward nil))
  "## [macro] defmodule
Define a module named `name`.

In Caten, `Module` is a CLOS class that represents a set of Funcs and is defined as a subclass of `Func` itself. It is used to represent computational nodes that can be expressed through compositions of Funcs. Consequently, as a subclass of Func, Module utilizes the following three methods for manipulation:

### [method] impl

`(impl (op Module) &rest tensors)`

- tensors[list] a list of the input tensor.

In the `impl` method, please describe the process for constructing the computational graph of the `Module` using a combination of `Func` and `Module`.
The computational graph must begin with the inputs.
If there are multiple outputs, bind them with `cl:values`.
If you need to record Tensors for the backward process, now is the time to do so.

### [method] forward

`(forward (op Module) &rest tensors)`

- tensors[List] a list of the input tensor.

In the forward method, describe the operation to create a Tensor after computation.
Be mindful of its lazy evaluation nature; do not perform the actual computation at this stage.
The `st` macro in ShapeTracker is quite useful for creating the Tensor after the operation. If necessary, you may also include checks for additional attributes or slots here.
If you specify ShapeTracker in `:where`, the defmodule macro will automatically generate the forward.
Therefore, you must describe either `:where` or `:forward`.

### [method] backward (optional)

`(backward (op Module) prev-grad) -> (values input_1.grad input_2.grad ...)`

- prev-grad[Tensor]

In the `backward` method, describe the gradient computation for the Module using a combination of `Func` and `Module`.
The arguments are fixed as `(op prev-grad)`, where op = module instance, prev-grad is a tensor.
If you need the value of the Tensor at the input stage for the gradient calculation, temporarily store it using the `module-sv4bws` accessor while calling the `impl` method.
The compiler does not depend on `module-sv4bws`, so you are free to choose how to store the Tensor.
In Caten, since save-for-backward is automatically determined, there is no need to be concerned about in-place operations.

Note that `backward` is **optional**. If it is not provided, AD will be applied based on the computational graph from `impl`.

### [method] lower

The lower method is automatically written by the `defmodule`, so there is no need to consider it when describing the module.
However, it is necessary to understand how it is lowered for when defining simplifiers for the `Module`.

`lower` produces the following node:
`(make-node :Graph (intern (symbol-name (symb 'graph/ name)) \"KEYWORD\") outputs inputs &rest attrs)`

Nodes whose class is `:Graph` are completely eliminated during lower by `impl`.

### Syntax

`forward`, `backward`, `impl` are described in one of the following format.

```
forward := ((op &rest args) &body body)
forward := (lambda (&rest args) &body body)
forward := fname
```

### Effects

- 1. defines a class named `name`.

- 2. defines a function named `name`. it works as a constructor.

### Arguments

- name[symbol] the name of module

- constructor-args[list] arguments for the constructor

- attrs[list] define attrs for the lowered graph based on the constructor-args variables using the following format: `(:key1 value1 :key1 value2 ...)`.

- slots[list] slots for the defined class.

- where[nil or string] ShapeTracker

- documentation[string] documentation

### Notes

- The methods are called in the order of `forward->impl->backward` during compilation

- `impl` is performed recursively, so modules must not be co-dependent within the `impl` method. (e.g.: do not define a module `A` that depends on `B` that depends on `A` ...)
"
  (assert (or (stringp where) forward) () "defmodule: Provide ShapeTracker to :where, or provide :forward.")
  (assert impl () "defmodule: :impl is required to lower a module.")
  (labels ((assert-bw-args (args rest)
	     (assert (= (length rest) 1) () "defmodule: The argument for backward is fixed as `(op dout)`~%but got ~a." args))
	   (impl-form (method form bw-p)
	     (with-gensyms (op-bind inputs-bind)
	       (match form
		 ((list* (list* op rest) body)
		  (when bw-p (assert-bw-args `(,op @rest) rest))
		  `(defmethod ,method ((,op ,name) &rest ,inputs-bind)
		     (assert (= (length ,inputs-bind) ,(length rest))
			     ()
			     "Error in the ~a ~a, the number of inputs does not match the declared one.~%decl=~a" ',method ',name ',rest)
		     (multiple-value-bind (,@rest) (apply #'values ,inputs-bind) ,@body)))
		 ((list* 'cl:lambda (list* rest) body)
		  (when bw-p (assert-bw-args rest rest))
		  `(defmethod ,method ((,op-bind ,name) &rest ,inputs-bind)
		     (apply #'(lambda (,@rest) ,@body) ,op-bind ,inputs-bind)))
		 ((guard x (symbolp x))
		  `(defmethod ,method ((,op-bind ,name) &rest ,inputs-bind)
		     (apply #',x ,op-bind ,inputs-bind)))
		 (_ (error "defmodule: The ~(~a~) method should be implemented in one of the following patterns:
  - :~(~a~) := ((op &rest args) &body body)
  - :~(~a~) := (lambda (&rest args) &body body)
  - :~(~a~) := function-name
The provided form does not match any of them:~%~a" method method method method form))))))
    `(progn
       (defclass ,name (Module ,@direct-superclasses) ,slots (:documentation ,documentation))
       ,(if forward
	    (impl-form 'forward forward nil)
	    `(defmethod forward ((op ,name) &rest inputs) (st ,where (inputs))))
       ,(if backward
	    (impl-form 'backward backward t)
	    `(defmethod backward ((op ,name) &optional prev-grad) (declare (ignore prev-grad)) :module/skip-bw))
       ,(impl-form 'impl impl nil)
       (defnode (:Module ,(intern (symbol-name (symb 'graph/ name)) "KEYWORD")) () ""
		:slots (,@(loop for attr in attrs
				for nth upfrom 0
				if (and (= 0 (mod nth 2)) (keywordp attr))
				  collect (list (intern (symbol-name attr))))
			(metadata :type ,name)))
       (defmethod lower ((op ,name) &rest inputs)
	 (make-graph
	  (apply #'make-node :Module (intern (symbol-name (symb 'graph/ ',name)) "KEYWORD")
		 (map 'list #'tensor-id (module-outputs op))
		 (map 'list #'node->id inputs) (append (module-attrs op) (list :metadata op)))))
       (defun ,name (,@constructor-args) (make-instance ',name :attrs (list ,@attrs))))))

;; ~~~ reduce ops ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun st/reduction (op x)
  (with-attrs ((axis :axis) (keepdims :keepdims)) op
    (multiple-value-bind (new-shape new-view) (parse-reduce-axes x axis)
      (let* ((out (apply #'!view (make-tensor new-shape :dtype (dtype-of x) :order (order x) :initial-element 0.0) new-view))
	     (out (st "A[~] B[~] -> A[~]" (out x)))
	     (out (if keepdims
		      out
		      (apply #'!view out (map 'list #'(lambda (x) (if (and (listp x) (eql (car x) :~)) `(:~ 1) t)) new-view)))))
	(setf (tensor-op out) nil (tensor-variables out) nil)
	out))))

(macrolet ((defreduce (model description op)
	     `(defmodule (,model ((&key (axis t) (keepdims nil)) :axis axis :keepdims keepdims))
		  ()
		  :documentation ,description
		  :forward st/reduction
		  :impl
		  ((op x)
		   (with-attrs ((axis :axis) (keepdims :keepdims)) op
		     (multiple-value-bind (new-shape new-view) (parse-reduce-axes x axis)
		       (let* ((out (make-tensor new-shape :dtype (dtype-of x) :order (order x) :initial-element 0.0))
			      (out (apply #'!view out new-view))
			      (out (,op out x :reduce t))
			      (out (if keepdims
				       out
				       (apply #'!view out (map 'list #'(lambda (x) (if (and (listp x) (eql (car x) :~)) `(:~ 1) t)) new-view)))))
			 out)))))))
  (defreduce SumNode "Sum tensors along axis" !add)
  (defreduce MaxReduce "Max" !maximum)
  (defreduce MinReduce "Min" !minimum))

(defmodule (MeanNode ((&key (axis t) (keepdims nil)) :axis axis :keepdims keepdims))
    ()
    :documentation "Means the tensor."
    :forward st/reduction
    :impl ((mean x)
	   (with-attrs ((axis :axis) (keepdims :keepdims)) mean
	     (let ((total (fconst 1)))
	       (loop for new-axis in (parse-reduce-axes x axis)
		     for base in (shape x)
		     if (eql new-axis 1) do (setf total (!* total (->fconst base))))
           (!div (!sum x :axis axis :keepdims keepdims) (!cast total (dtype-of x)))))))

(macrolet ((defreduce (f model)
	     `(progn
		(declaim (ftype (Function (Tensor &key (:axis t) (:keepdims boolean)) (values Tensor &optional)) ,f))
		(defun ,f (x &key (axis t) (keepdims nil)) (forward (,model :axis axis :keepdims keepdims) x)))))
  (defreduce !sum SumNode)
  (defreduce !mean MeanNode)
  (defreduce !max MaxReduce)
  (defreduce !min MinReduce))
;; ~~~ gemm ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodule (Matmul (()) :where "A[~ i j] B[~ j k] -> A[~ i k]")
    ()
    :documentation "Gemm (TODO)"
    :impl ((mm x y)
	   (multiple-value-bind (n1 n2) (values (ndim x) (ndim y))
	     (let* ((mid (loop for i upfrom 0 below (min (- n1 1) (- n2 1) 1) collect 1))
		    (x (!reshape x `(,@(butlast (shape x) 1) ,@mid ,(car (last (shape x))))))
		    (y (!reshape y `(,@(butlast (shape y) 2) ,@mid ,@(last (shape y) (min n2 2))))))
	       (let ((z (!mul x (!contiguous (!t y) :force t))))
		 (!reshape (!sum z :axis -1) (butlast (shape z))))))))
(defun !matmul (a b)
  (multiple-value-bind (a b) (bc "A[~ i j] B[~ j k] -> A[~ i j] B[~ j k]" (a b))
    (forward (make-instance 'Matmul) a b)))
;; ~~ math ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodule (SinHNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "SinH"
    :impl ((sinh x) (!div (!sub (!exp x) (!exp (!neg x))) (!const x 2.0))))

(defmodule (CoshNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "CosH"
    :impl ((cosh x) (!div (!add (!exp x) (!exp (!neg x))) (!const x 2.0))))

(defmodule (TanhNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "TanH"
    :impl ((tanh x)
	   (let ((two (!const x 2.0)))
	     (!sub (!mul two (uiop:symbol-call :caten/nn :!sigmoid (!mul two x))) (!const x 1.0)))))

(defmodule (CosNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Cos"
    :impl ((cos x) (!sin (!add x (fconst (/ pi 2) :dtype (dtype-of x))))))

(defmodule (TanNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Tan"
    :impl ((cos x) (!div (!sin x) (!cos x))))

(declaim (ftype (function (Tensor) (values Tensor &optional)) !sinh !cosh !tanh !cos !tan !log2 !exp2))
(defun !sinh (x) (forward (SinhNode) x))
(defun !cosh (x) (forward (CoshNode) x))
(defun !tanh (x) (forward (TanhNode) x))
(defun !cos (x) (forward (CosNode) x))
(defun !tan (x) (forward (TanNode) x))

(defmodule (Exp2Node (()) :where "A[~] -> A[~]")
    ()
    :documentation "Exp2"
    :impl ((exp2 x) (!exp (!mul x (fconst (log 2) :dtype (dtype-of x))))))

(defmodule (Log2Node (()) :where "A[~] -> A[~]")
    ()
    :documentation "Log2"
    :impl ((log2 x) (!div (!log x) (fconst (log 2) :dtype (dtype-of x)))))

(defun !log2 (x) (forward (Log2Node) x))
(defun !exp2 (x) (forward (Exp2Node) x))

;; ~~ Trunc/Ceil/Floor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodule (TruncateNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Truncate(x)"
    :impl ((trunc x) (!cast (!cast x :uint32) (dtype-of x))))

(defun !truncate (x) (forward (TruncateNode) x))

(defmodule (CeilingNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Ceiling(x)"
    :impl ((ceil x) (let ((b (!truncate x))) (!where (!> x b) (!add b (!const b 1)) b))))

(defun !ceiling (x) (forward (CeilingNode) x))

(defmodule (FloorNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Floor(x)"
    :impl ((ceil x) (let ((b (!truncate x))) (!where (!< x b) (!sub b (!const b 1)) b))))

(defun !floor (x) (forward (FloorNode) x))
