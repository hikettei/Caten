(in-package :caten/apis)

(defstruct (Tensor
	    (:constructor %internal-make-tensor (op shape
						 &key
						   (dtype *default-float*) (order *default-order*) (id (gensym "TID"))
						   (variables nil) (views nil) (requires-grad nil)
						   (grad (when requires-grad (make-tensor shape :dtype dtype :order order :requires-grad nil :id (gensym "GRAD"))))
						   (grad-id (when requires-grad (gensym "TGRAD"))))))
  "
A struct `tensor` is a multi-dimensional, and strided matrix (and nrank=0 to scalar value) containing elements of the single dtype.
Also the tensor has following slots:

- shape[list] A list of number, symbol, or `Tensor`.
- buffer[Buffer] A buffer of the tensor. Realized arrays are stored here.
- dtype[keyword] A dtype of the tensor.
- order[order] A memory layout of the tensor, selected from either of :row or :column.
- id[symbol] A unique identifier of the tensor. (usually created by gensym)
- op[Func] `Func` object that represents the operation of the tensor.
- views[list] A list of `ViewRange` objects, determining the bound of loops.
- requires-grad[boolean] A flag to determine whether the tensor requires a gradient.
- grad[Tensor] A gradient and realized tensor of the tensor.
- grad-id[symbol] A unique identifier of the gradient tensor.
- variables[list] A list of `Tensor` objects that are used in the operation of the tensor. Tensors listed here are involved in the compilation.
"
  (shape shape :type list)
  (buffer nil :type (or null Buffer))
  (dtype dtype :type dtype-t)
  (order order :type (member :row :column))
  (id id :type symbol)
  (op op :type (or null Func)) ;; Type Func or Module
  (views views :type list)
  (requires-grad requires-grad :type boolean)
  (grad grad :type (or null Tensor))
  (grad-id grad-id :type symbol)
  (variables variables :type list))

(defmethod make-load-form ((tensor Tensor) &optional env &aux (debug-p (= 1 (ctx:getenv :AOT_VERBOSE))))
  (declare (ignore env))
  (when (and (tensor-buffer tensor) debug-p)
    (warn "Removing the buffer of ~a when dumping it." tensor))
  (values
   `(%internal-make-tensor
     nil ',(tensor-shape tensor)
     :dtype ,(tensor-dtype tensor) :order ',(tensor-order tensor)
     :id ',(tensor-id tensor) :variables ',(tensor-variables tensor)
     :views nil :requires-grad ,(tensor-requires-grad tensor)
     :grad ,(tensor-grad tensor) :grad-id ',(tensor-grad-id tensor))))

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
  (format stream "{Tensor[~(~a~)] :shape ~a :id ~a
~a
  :op ~a
  :requires-grad ~a
  :variables ~a}"
	  (tensor-dtype tensor)
	  (loop for s in (tensor-shape tensor) collect (if (tensor-p s) (tensor-id s) s))
	  (tensor-id tensor)
	  (if (tensor-buffer tensor)
	      (pprint-buffer (tensor-buffer tensor) :indent 2)
	      "  :buffer nil")
	  (tensor-op tensor)
	  (tensor-requires-grad tensor)
	  (map 'list #'tensor-id (tensor-variables tensor))))

(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (id (gensym "TID")) (requires-grad nil) (initial-element nil) (views nil) (from nil))
  "
```
(make-tensor shape &key (dtype *default-float*) (order *default-order*) (id (gensym \"TID\")) (requires-grad nil) (initial-element nil) (views nil) (from nil))
```
Create a new lazy tensor.

- shape[list] A list of number, symbol, or `Tensor`.
- dtype[keyword] A dtype of the tensor.
- order[order] A memory layout of the tensor, selected from either of :row or :column.
- id[symbol] A unique identifier of the tensor. (usually created by gensym)
- requires-grad[boolean] A flag to determine whether the tensor requires a gradient.
- initial-element[null|number|symbol|ScalarTensor] An initial value of the tensor.
- views[list] A list of `ViewRange` objects, determining the bound of loops.
- from[null|Buffer|Symbol] A buffer used to initialize the tensor. If symbol is given, the buffer is taken from the variable table.
"
  (declare (type list shape)
	   (type dtype-t dtype)
	   (type (member :column :row) order)
	   (type symbol id)
	   (type (or null number symbol) initial-element)
	   (type (or null symbol Buffer) from))
  (dolist (s shape)
    (assert (or (and (integerp s) (>= s 1)) (tensor-p s) (symbolp s))
	    ()
	    "make-tensor: Cannot initialize a tensor.~%~%Shape should be specified as an integer (>1), tensor, or symbol.~%  Butgot: ~a~%  Shape=~a" s shape))
  (let ((buff (%internal-make-tensor nil shape :dtype dtype :order order :id id :requires-grad requires-grad :views views)))
    (setf (tensor-op buff) (make-instance 'Allocate :buffer buff :initial-element initial-element :from from)
          ;(tensor-shape buff) (map 'list #'(lambda (x) (if (tensor-p x) (or (try-fold-constant x) x) x)) (tensor-shape buff))
          )
    buff))

(defun make-scalar (value &key (dtype *default-float*) (order *default-order*) (id (gensym "SID")) (requires-grad nil))
  "
```
(make-scalar value &key (dtype *default-float*) (order *default-order*) (id (gensym \"SID\")) (requires-grad nil))
```
Create a new scalar tensor. ScalarTensor in Caten is a tensor with a rank of 0.

- value[number|symbol] A value of the scalar tensor.
- dtype[keyword] A dtype of the tensor.
- order[order] A memory layout of the tensor, selected from either of :row or :column.
- id[symbol] A unique identifier of the tensor. (usually created by gensym)
- requires-grad[boolean] A flag to determine whether the tensor requires a gradient.

Tips: You can use `fconst`, `uconst`, and `iconst` to create a scalar tensor with a default dtype. (arguments are the same as `make-scalar`.)
"
  (make-tensor nil :dtype dtype :order order :id id :requires-grad requires-grad :initial-element value))

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
View is a tensor which shares the buffer from the original tensor, but having different shapes, strides, offsets, or dtype
"
  (declare (type Tensor base)
	   (type list subscripts)
	   (type dtype-t dtype)
	   (type (member :row :column) order))
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
	(setf (tensor-variables buff)
	      (append
	       (list base)
	       (map 'list (compose #'sfold #'vrange-size) views)
	       (map 'list (compose #'sfold #'viewrange-from) views)
	       (map 'list (compose #'sfold #'viewrange-to) views)
	       (map 'list (compose #'sfold #'viewrange-by) views)
	       (map 'list (compose #'sfold #'viewrange-size) views)
	       (loop for s in stride collect (if (node-p s) s (iconst s))))
	      (tensor-op buff) (make-instance 'View :views views :broadcast-mode broadcast-mode-p :subscripts subscripts :nrank (length views)))
	(setf (func-variables (tensor-op buff)) (tensor-variables buff))
	(assert (every #'tensor-p (tensor-variables buff)) ())
	;; Fold Constants in Shape (detached from the graph, no side effects)
	(setf (tensor-shape buff) (map 'list #'(lambda (x) (if (tensor-p x) (or (try-fold-constant x) x) x)) (tensor-shape buff)))
	buff))))

;; ~~ Floating Features ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun inf (&key (dtype *default-float*))
  "
```
(inf &key (dtype *default-float*))
```
Returns a positive infinity of the dtype for the current Common Lisp implementation.

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

Returns a negative infinity of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-negative-infinity)
    (:float32 float-features:single-float-negative-infinity)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO"))))

(defun nan (&key (dtype *default-float*))
  "
```
(nan &key (dtype *default-float*))
```

Returns a NaN of the dtype for the current Common Lisp implementation.

This feature is supported by [float-features](https://shinmera.github.io/float-features/)
"
  (ecase dtype
    (:float64 float-features:double-float-nan)
    (:float32 float-features:single-float-nan)
    (:float16 (error "Not ready (TODO)"))
    (:bfloat16 (error "Not ready (TODO"))))

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

Returns `:INF` if the number is a negative infinity, `:-INF` if the number is a negative infinity, `:nan` if the number is NaN, or T otherwise.
"
  (declare (type (or symbol number) x))
  (cond
    ((float-infinity-p x)
     (if (> x 0) :inf :-inf))
    ((float-nan-p x) :nan)
    (t t)))
