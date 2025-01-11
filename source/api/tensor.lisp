(in-package :caten/api)

(defstruct (Tensor
	    (:constructor %internal-make-tensor (op shape
						 &key
						   (dtype *default-float*) (order *default-order*) (id (gensym "TID"))
						   (variables nil) (views nil) (requires-grad nil)
						   (grad (when requires-grad (make-tensor shape :dtype dtype :order order :requires-grad nil :id (gensym "GRAD"))))
						   (grad-id (when requires-grad (gensym "TGRAD")))
                                                   (tracker (start-tracking shape :order order)))))
  "
A struct `tensor` is a multi-dimensional, and strided matrix (and nrank=0 to scalar value) containing elements of the single dtype.
Also, the tensor has the following slots:

- shape[list] The shape as a list of elements of: number, symbol, or `Tensor`.
- buffer[AbstractBuffer] The buffer of the tensor. Realized arrays are stored here.
- dtype[keyword] The dtype of the tensor.
- order[order] The memory layout of the tensor, selected from either :row or :column.
- id[symbol] The unique identifier of the tensor. (usually created via gensym)
- op[Func] The `Func` object that represents the operation of the tensor.
- views[list] A list of `ViewRange` objects, determining the bound of loops.
- requires-grad[boolean] A flag to determine whether the tensor requires a gradient.
- grad[Tensor] A gradient and realized tensor of the tensor.
- grad-id[symbol] A unique identifier of the gradient tensor.
- variables[list] A list of `Tensor` objects that are used in the operation of the tensor. Tensors listed here are involved in the compilation.
"
  (shape shape :type list)
  (tr tracker :type Tracker)
  (buffer nil :type (or null AbstractBuffer))
  (dtype dtype :type dtype-t)
  (order order :type (member :row :column))
  (id id :type symbol)
  (op op :type (or null Func)) ;; Type Func or Module
  (views views :type list)
  (requires-grad requires-grad :type boolean)
  (grad grad :type (or null Tensor))
  (grad-id grad-id :type symbol)
  (variables variables :type list)
  (cache-canonicalize nil)
  (nth-output 0 :type fixnum))

(defun grad (tensor)
  "
```
(grad tensor)
```
Returns a gradient of the tensor"
  (tensor-grad tensor))

(defun shape (tensor)
  "
```
(shape tensor)
```
Returns a copy of the shape of the tensor"
  (copy-list (tensor-shape tensor)))

(defun ndim (tensor)
  "
```
(ndim tensor)
```
Returns a rank of the tensor"
  (length (shape tensor)))

(defun dtype-of (tensor)
  "
```
(dtype-of tensor)
```
Returns a dtype of the tensor"
  (tensor-dtype tensor))

(defun order (tensor)
  "
```
(order tensor)
```
Returns a memory-layout of the tensor."
  (tensor-order tensor))

(defmethod print-object ((tensor Tensor) stream)
  (format stream "{Tensor~a[~(~a~)] :shape ~a :id ~a
~a
  :op ~a
  :requires-grad ~a
  :variables ~a
  :tracker ~a}"
          (if (tensor-buffer tensor)
              (format nil "{~a}" (class-name (class-of (tensor-buffer tensor))))
              "")
	  (tensor-dtype tensor)
	  (loop for s in (tensor-shape tensor) collect (if (tensor-p s) (tensor-id s) s))
	  (tensor-id tensor)
	  (if (tensor-buffer tensor)
	      (pprint-buffer (tensor-buffer tensor) :indent 2)
	      "  :buffer nil")
	  (tensor-op tensor)
	  (tensor-requires-grad tensor)
	  (map 'list #'tensor-id (tensor-variables tensor))
          (tensor-tr tensor)))

(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (id (gensym "TID")) (requires-grad nil) (initial-element nil) (views nil) (from nil) (tr nil))
  "
```
(make-tensor shape &key (dtype *default-float*) (order *default-order*) (id (gensym \"TID\")) (requires-grad nil) (initial-element nil) (views nil) (from nil))
```
Create a new lazy tensor.

- shape[list] The shape as a list of elements of: number, symbol, or `Tensor`.
- dtype[keyword] The dtype of the tensor.
- order[order] The memory layout of the tensor, selected from either :row or :column.
- id[symbol] The unique identifier of the tensor. (usually created by gensym)
- requires-grad[boolean] A flag to determine whether the tensor requires a gradient.
- initial-element[null|number|symbol|ScalarTensor] An initial value of the tensor.
- views[list] A list of `ViewRange` objects, determining the bound of loops.
- from[null|AbstractBuffer|Symbol] A buffer used to initialize the tensor. If a symbol is given, then the buffer is taken from the variable table.
"
  (declare (type list shape)
	   (type dtype-t dtype)
	   (type (member :column :row) order)
	   (type symbol id)
	   (type (or null number symbol) initial-element)
	   (type (or null symbol AbstractBuffer) from))
  (dolist (s shape)
    (assert (or (and (integerp s) (>= s 1)) (tensor-p s) (symbolp s))
	    ()
	    "make-tensor: Cannot initialize a tensor.~%~%Shape should be specified as an integer (>1), tensor, or symbol.~%  But got: ~a~%  Shape=~a" s shape))
  (let ((lazy-tensor (%internal-make-tensor nil shape :dtype dtype :order order :id id :requires-grad requires-grad :views views :tracker (or tr (start-tracking shape :order order)))))
    (setf (tensor-op lazy-tensor) (make-instance 'Allocate :buffer lazy-tensor :initial-element initial-element :from from)
          (tensor-shape lazy-tensor) (map 'list #'(lambda (x) (if (tensor-p x) (or (try-fold-constant x) x) x)) (tensor-shape lazy-tensor))
          (tensor-variables lazy-tensor) (loop for s in (tensor-shape lazy-tensor) if (tensor-p s) collect s)
          (func-variables (tensor-op lazy-tensor)) (tensor-variables lazy-tensor))
    lazy-tensor))

(defun make-scalar (value &key (dtype *default-float*) (order *default-order*) (id (gensym "SID")) (requires-grad nil))
  "
```
(make-scalar value &key (dtype *default-float*) (order *default-order*) (id (gensym \"SID\")) (requires-grad nil))
```
Create a new scalar tensor. A ScalarTensor in Caten is a tensor with a rank of 0.

- value[number|symbol] The value of the scalar tensor.
- dtype[keyword] The dtype of the tensor.
- order[order] The memory layout of the tensor, selected from either of :row or :column.
- id[symbol] A unique identifier of the tensor (usually created by gensym).
- requires-grad[boolean] A flag to determine whether the tensor requires a gradient.

Hint: You can use `fconst`, `uconst`, and `iconst` to create a scalar tensor with a default dtype (arguments are the same as `make-scalar`.)
"
  (make-tensor nil :dtype dtype :order order :id id :requires-grad requires-grad :initial-element value))

;; create some defaults
(macrolet ((def (name dtype)
	     `(defun ,name (value &key (dtype ,dtype) (order *default-order*) (id (gensym "SID")) (requires-grad nil))
		(if (tensor-p value)
		    value
		    (make-tensor nil :dtype dtype :order order :id id :requires-grad requires-grad :initial-element value)))))
  (def fconst *default-float*)
  (def uconst *default-uint*)
  (def iconst *default-int*))

(defun make-view-internal (base subscripts &key (allow-merge t) (dtype (tensor-dtype base)) (order (tensor-order base)) (id (gensym "VID")) (stride nil))
  "
An internal function for making a view from tensor.
View is a tensor which shares the buffer of the original tensor, but features different shapes, strides, offsets, or dtype.
"
  (declare (type Tensor base)
	   (type list subscripts)
	   (type dtype-t dtype)
	   (type (member :row :column) order)
           (ignore stride))
  (handler-bind
      ((error
	 #'(lambda (c) (error 'caten-forward-error :op 'make-view-internal :inputs (list base) :c c))))
    (flet ((is-broadcast (x) (and (listp x) (eql (car x) :~))))
      (let* ((views (merge-views base subscripts allow-merge))
	     (buff (%internal-make-tensor nil (map 'list #'vrange-size views) :dtype dtype :order order :id id :views views))
	     (broadcast-mode-p (some #'is-broadcast subscripts)))
	(when broadcast-mode-p
	  (assert (every #'(lambda (x) (or (is-broadcast x) (eql x t))) subscripts)
		  ()
		  "Do not slice other axes when using broadcast. ~a" subscripts))
	(setf (tensor-variables buff) (list base)
	      (tensor-op buff) (make-instance 'View :views views :broadcast-mode broadcast-mode-p :subscripts subscripts :nrank (length views))
              (func-variables (tensor-op buff)) (tensor-variables buff))
	(assert (every #'tensor-p (tensor-variables buff)) ())
	;; Fold Constants in Shape (detached from the graph, no side effects)
	(setf (tensor-shape buff) (map 'list #'(lambda (x) (if (tensor-p x) (or (try-fold-constant x) x) x)) (tensor-shape buff)))
        (if broadcast-mode-p
            (setf (tensor-tr buff) (tr-apply-broadcast
                                    base
                                    (map 'list #'(lambda (x) (if (and (listp x) (eql (car x) :~)) (second x) nil)) subscripts)))
            (setf (tensor-tr buff) (tr-apply-slice base views (map 'list #'vrange-size views))))
        (setf (view-tr (tensor-op buff)) (tensor-tr buff))
	buff))))

(defun tensor-graph (&rest tensors)
  "
```
(tensor-graph tensor)
```

Lowers the given tensors into an aasm graph, only constant folding is applied.

For some convenience, this function returns the first tensor if the input is not a tensor.
"
  (when (not (tensor-p (car tensors)))
    (return-from tensor-graph (car tensors)))
  (assert (every #'tensor-p tensors))
  (let ((graph (->fast-graph (apply #'%tensor->aasm tensors))))
    (optimize-aasm graph)
    graph))

(defun tensor-lowered-graph (&rest tensors)
  "
```
(tensor-lowered-graph &rest tensors)

Creates a lowered graph from the given tensors (i.e., an input graph to JIT=1).
"
  (when (not (tensor-p (car tensors)))
    (return-from tensor-lowered-graph (car tensors)))
  (assert (every #'tensor-p tensors))
  (ctx:with-contextvar (:BACKEND "LISP")
    (runtime-graph (caten tensors))))

;; ~~ Floating Point Features ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun inf (&key (dtype *default-float*))
  "
```
(inf &key (dtype *default-float*))
```
Returns positive infinity of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-positive-infinity)
    (:float32 float-features:single-float-positive-infinity)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO)"))))

(defun -inf (&key (dtype *default-float*))
  "
```
(-inf &key (dtype *default-float*))
```

Returns negative infinity of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-negative-infinity)
    (:float32 float-features:single-float-negative-infinity)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO)"))))

(defun nan (&key (dtype *default-float*))
  "
```
(nan &key (dtype *default-float*))
```

Returns NaN of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-nan)
    (:float32 float-features:single-float-nan)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO)"))))

(defun float-infinity-p (x)
  (declare (type (or symbol number) x))
  (typecase x
    (float
     (float-features:float-infinity-p x))
    (t
     nil)))

(defun float-nan-p (x)
  (declare (type (or symbol number) x))
  (typecase x
    (float (eql x (nan)))
    (t
     nil)))

(declaim (ftype (function ((or symbol number)) (member :inf :-inf :nan t)) float-type-of))
(defun float-type-of (x)
  "
```
(float-type-of x)
```

Returns `:INF` if the number is negative infinity, `:-INF` if the number is negative infinity, `:nan` if the number is NaN, or T otherwise.
"
  (declare (type (or symbol number) x))
  (cond
    ((float-infinity-p x)
     (if (> x 0) :inf :-inf))
    ((float-nan-p x) :nan)
    (t t)))
;; ~~~ Temporary Runtime Management ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *global-runtime* (make-hash-table))

(defun get-global-runtime ()
  "
```
(get-global-runtime)
```
Returns a temporary runtime object just used for allocation global buffer."
  (or (gethash (ctx:getenv :BACKEND) *global-runtime*)
      (setf (gethash (ctx:getenv :BACKEND) *global-runtime*)
            (make-runtime (make-graph) :runtime (caten/codegen/backend:get-runtime-type) :buffer-type (caten/codegen/backend:get-buffer-type)))))
