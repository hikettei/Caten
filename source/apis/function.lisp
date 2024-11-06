(in-package :caten/apis)
;; Function creating a lazy computation node, should start with the prefix !.

(defclass Func () ((variables :initarg :variables :initform nil :accessor func-variables))
  (:documentation "A CLOS class that represents a computation.

```
[Func] -> <Lower> -> [FastGraph] -> <Simplifier> -> [Completed]
```

`Func` is a syntax-sugar for generating lowered instructions defined in the `caten/aasm` package.

To properly lower the `Func`, You need to implement the following three methods:

- lower: Lower the `Func` into a list of `caten/air:node`. This should return `caten/air:graph`.
- forward: Create the type for the Tensor after computation. Be mindful of its lazy evaluation nature; do not perform the actual computation. `ShapeTracker` might help you. (use the `st` macro)
- backward: Create the graph for backward of op given prev-grad. Return: `(values input_1.grad input_2.grad ...)`."))

(defgeneric lower (op &rest nodes)
  (:documentation "
```
(lower op &rest nodes)
```

Lowers the Func into a list of `caten/air:node`. This should return caten/air:graph.
- op[Func] Func to lower.
- nodes[list] list of previous nodes (each position corresponds to the position of the variables in the Func).
"))

(defgeneric forward (op &rest tensors)
  (:documentation "
```
(forward op &rest tensors)
```

Create the type for the Tensor after computation. Be mindful of its lazy evaluation nature; do not perform the actual computation. Use the `st` macro to create a new tensor.

- op[Func] Func to forward.
- tensors[list] list of input tensors."))

(defgeneric backward (op &optional prev-grad)
  (:documentation "
```
(backward op &optional prev-grad)
```

Create the graph for backward of op given prev-grad. Return: `(values input_1.grad input_2.grad ...)`.
save-for-backward is determined automatically, so you do not have to consider about in-place operation.

- op[Func] Func to backward.
- prev-grad[Tensor] previous gradient tensor.
"))

(defmethod forward :around ((op Func) &rest tensors)
  (let ((outs (handler-bind
		  ((error
		     #'(lambda (c) (error 'caten-forward-error :op op :inputs tensors :c c))))
		(multiple-value-list (call-next-method))))
	(tensors (loop for tensor in tensors if (tensor-p tensor) collect tensor)))
    (setf (func-variables op) tensors)
    (dolist (o outs)
      (assert (tensor-p o) ())
      (setf (tensor-variables o) tensors
	    (tensor-op o) op))
    (apply #'values outs)))
;; ~~ differentiable ops ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass IdentityNode (Func) nil)
(defmethod forward ((op IdentityNode) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op IdentityNode) &optional prev-grad) (values prev-grad))
(defmethod lower ((op IdentityNode) &rest inputs) (with-context (_ (%store (car inputs) (car inputs) :reduction t))))

(defun !identity (tensor)
  "
```
(!identity tensor)
```

Equivalent to #'identity, but it is used to create a lazy computation node.
"
  (forward (make-instance 'IdentityNode) tensor))

(defclass Allocate (Func)
  ((buffer :initarg :buffer :type Tensor :accessor alloc-buffer)
   (initial-element :initarg :initial-element :initform nil :accessor alloc-initial-element)
   (id :initform nil :accessor alloc-id)
   (from :initform nil :initarg :from :accessor alloc-from)))

(defmethod forward ((op Allocate) &rest tensors) (declare (ignore tensors)) (alloc-buffer op))

(defmethod backward ((op Allocate) &optional dout)
  (let ((buff (alloc-buffer op)))
    (when (tensor-requires-grad buff)
      ;; op.grad += buff
      (let ((id (gensym "ACC")))
	(setf (alloc-id op) id)
	(values (!add (tensor-grad buff) dout :reduce t :id id))))))

(defmethod lower ((op Allocate) &rest inputs)
  (declare (ignore inputs))
  (let ((buff (alloc-buffer op))
	(nodes))
    (flet ((->lower (obj) ;; If the shape includes a tensor, it also needs to be lowered
	     (if (or (numberp obj) (symbolp obj)) obj
		 (let ((g (%tensor->aasm obj)))
		   (and (push g nodes) (car (last (graph-nodes g))))))))
      (let ((g
	      (with-context
		(s (map 'list #'->lower (tensor-shape buff)))
		(a (%make-tensor s :dtype (tensor-dtype buff) :order (tensor-order buff) :id (tensor-id buff) :from (alloc-from op)))
		(a (when (alloc-initial-element op) (%load a (alloc-initial-element op)))))))
	(push g nodes)
	(apply #'make-graph (apply #'append (map 'list #'graph-nodes (reverse nodes))))))))

(defclass View (Func)
  ((views :initarg :views :type list :accessor view-views)
   (subscripts :initarg :subscripts :accessor view-subscripts)
   (broadcast-mode :initarg :broadcast-mode :accessor view-broadcast-mode)
   (nrank :initarg :nrank :accessor view-nrank)
   (tr :accessor view-tr)))

(defmethod backward ((op View) &optional dout)
  (with-slots ((nrank nrank) (broadcast-mode broadcast-mode) (views views) (subscripts subscripts)) op
    (let* ((base (clone-like (car (func-variables op)))))
      (apply #'!view-from-base (!move (apply #'!view base subscripts) dout) (loop for s in (shape base) collect `(0 ,s))))))

(defmethod lower ((op View) &rest inputs)
  (%make-view-from-tracker (view-tr op) (gensym "TID") (car inputs)))

(defun !view (base &rest subscripts)
  "
```
(!view base &rest subscripts)
```

Create a view node from the base tensor and subscripts.

We refer to `VIEW` as a node creating tensor whose buffers are shared with the `base` tensor, but shapes, strides, dtypes, offsets, or dtypes are different.

Subscripts has the following notation:

- `t` keep using the base view.
- `fixnum` refers to the specified element. e.g.: `A[3]`
- `(a b)` slices in the range of `[a, b)`
- `(a b c)` slices in the range of `[a, b)` with step `c`. `c` can be negative. In that case, b must be larger than a. For example: `(10 0 -1)` to reverse the elements in the axis.
- `(:~ n)` to broadcast the axis, with the size of `n`

It is supported to compose mutliple views; the viewed tensors can be created from the viewed tensors.
"
  (make-view-internal base subscripts))

(defun !view-from-base (base &rest subscripts)
  "Equivalent to !view but it ignores the base view object."
  (make-view-internal base subscripts :allow-merge nil))

(defclass Permute (Func)
  ((nrank :initarg :nrank :accessor permute-nrank)
   (order :initarg :order :accessor permute-order)
   (tr :accessor permute-tr)))

(defmethod permute-list ((op Permute) list)
  (loop for nth in (permute-order op)
	collect (nth nth list)))

(defmethod forward ((op Permute) &rest inputs)
  (let ((x (car inputs))
	(order (permute-order op)))
    (assert (= (length order) (ndim (car inputs)) (length (intersection (range 0 (ndim (car inputs))) order)))
	    ()
	    "Permute: order is not a valid permutation, getting ~a.~%axes are chosen from ~a" order (range 0 (ndim (car inputs))))
    (let ((out (make-tensor (permute-list op (shape x)) :dtype (dtype-of x) :order (order x) :views (and (tensor-views x) (permute-list op (tensor-views x))))))
      (setf (tensor-tr out) (tr-apply-permute x order)
            (permute-tr op) (tensor-tr out))
      out)))

(defmethod backward ((op Permute) &optional dout) (!permute dout (permute-order op)))

(defmethod lower ((op Permute) &rest inputs)
  (%make-view-from-tracker (permute-tr op) (gensym "PERMUTE") (car inputs)))

(defun !permute (tensor &rest order)
  "
```
(!permute tensor &rest order)
```

Returns a tensor that is a permutation of the original tensor. The new tensor has the same data as the original tensor but with the dimensions permuted according to the `order` specified. order can be passed as a list or separated arguments. That is, both of `(!permute x 0 1)` or `(!permute x (list 0 1))` are valid.
"
  (forward (make-instance 'Permute :order (flatten order)) tensor))

(defun !t (tensor)
  "
```
(!t tensor)
```

Transposes the last two axes of the tensor
"
  (let ((range (range 0 (ndim tensor)))
	(n (ndim tensor)))
    (setf (nth (- n 2) range) (nth (- n 1) range)
	  (nth (- n 1) range) (1- (nth (- n 2) range)))
    (!permute tensor range)))

(defun !transpose (tensor &optional (dim0 1) (dim1 0))
  "
```
(!transpose tensor &optional (dim0 1) (dim1 0))
```

Transposes the `dim0` and `dim1`.
"
  (declare (type tensor tensor))
  (let* ((range (range 0 (ndim tensor)))
	 (tmp (nth1 dim0 range)))
    (setf (nth1 dim0 range) (nth1 dim1 range)
	  (nth1 dim1 range) tmp)
    (!permute tensor range)))

(defun !contiguous (x &key (force nil))
  "
```
(!contiguous x &key (force nil))
```

If the tensor is viewed, creates a copy of tensor with contiguous memory. Otherwise, returns the original tensor. If `force` is set to T, it always creates a copy.
"
  (declare (type tensor x))
  (if (or force (tensor-views x))
      (let ((out (make-tensor (tensor-shape x) :dtype (tensor-dtype x) :order (tensor-order x))))
	(!move out x))
      x))

(defun !copy (x)
  "
```
(!copy x)
```
Creates a copy of the tensor. In Caten, the in-place operations are automatically determined, so in general, you do not have to consider using it.
"
  (!contiguous x :force t))

(defclass Reshape (Func)
  ((shape-bf :initarg :shape-bf :accessor reshape-shape-bf)
   (shape-af :initarg :shape-af :accessor reshape-shape-af)
   (order    :initarg :order    :accessor reshape-order)
   (tr       :accessor reshape-tr)))

(defmethod forward ((op Reshape) &rest tensors)
  (when (and (every #'numberp (reshape-shape-bf op)) (every #'numberp (reshape-shape-af op)))
    (assert (= (apply #'* (reshape-shape-bf op)) (apply #'* (reshape-shape-af op)))
	    ()
	    "Assertion Failed: Cannot reshape from ~a to ~a. The number of total elements should correspond."
	    (reshape-shape-bf op) (reshape-shape-af op)))
  (let ((out (make-tensor (reshape-shape-af op) :dtype (tensor-dtype (car tensors)) :order (tensor-order (car tensors)))))
    (setf (tensor-tr out) (tr-apply-reshape (car tensors) (reshape-shape-af op))
          (reshape-tr op) (tensor-tr out))
    out))

(defmethod backward ((op Reshape) &optional prev-grad) (!reshape prev-grad (reshape-shape-bf op)))

(defmethod lower ((op Reshape) &rest nodes)
  (%make-view-from-tracker (reshape-tr op) (gensym "RESHAPE") (car nodes)))

(defun !reshape (x &rest shape)
  "
```
(!reshape x &rest shape)
```

Returns a same tensor but `shape` is changed. shape can be passed as a list or separated arguments. That is, both of `(!reshape x '(1 2 3))` or `(!reshape x 1 2 3)` are valid.

Shape is a list of integers, symbols, or tensors.

If `x` is a viewed tensor, it creates a copy of the tensor with contiguous memory (but later JIT will try to eliminate this).
"
  (declare (type tensor x) (type list shape))
  (let ((shape (flatten shape)))
    (forward (make-instance 'Reshape :shape-bf (tensor-shape x) :shape-af shape :order (tensor-order x))
             (if (tr-reshapeable-p x shape) x (!contiguous x :force t)))))

(defun !uprank (x n)
  "
```
(!uprank x n)
```

Returns a tensor with one is inserted at the beginning of the shape of `x` for n times.
"
  (declare (type tensor x) (type (integer 0) n))
  (!reshape x (append (loop for i upfrom 0 below n collect 1) (tensor-shape x))))

(defun !repeat (x &rest repeats)
  "
```
(!repeat x &rest repeats)
```

Returns a tensor with the shape of `x` broadcasted by `repeats`.
"
  (let* ((base-shape (append (loop repeat (- (length repeats) (ndim x)) collect 1) (shape x)))
	 (new-shape (loop for s in (shape x) append (list 1 s)))
	 (expand-shape (loop for r in repeats for b in base-shape append (list `(:~ ,r) t)))
	 (final-shape (loop for s in (shape x) for r in repeats collect (!mul (->iconst s) (->iconst r)))))
    (apply #'!view (!reshape (!contiguous (apply #'!view (!reshape x new-shape) expand-shape)) final-shape) (loop for f in final-shape collect t))))

(defun !expand (x &rest shape &aux (shape (flatten shape)))
  "
```
(!expand x &rest shape)
```

Returns a tensor that is expanded to the shape that is specified. Expand can also increase the number of dimensions that a tensor has.
"
  (multiple-value-bind (view-index reshape-to) (apply #'values (pad-left (shape x) shape))
    (let ((x (if (= (ndim x) (length shape)) x (!reshape x reshape-to))))	  
      (apply #'!view x (map 'list #'(lambda (x y) (if (eql x y) t `(:~ ,x))) view-index reshape-to)))))
;; ~~ binary ops ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Move (Func) ((reduction :initarg :reduction :accessor move-reduction)))
(defmethod forward ((op Move) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op Move) &optional dout) (values dout dout))
(defmethod lower ((op Move) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%move a b :reduction (move-reduction op))))))

(defun !move (a b &key (reduce nil))
  "
```
(!move a b &key (reduce nil))
```

Moves the element of b into a, returning a. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (declare (type tensor a b))
  (apply #'forward (make-instance 'Move :reduction reduce) (broadcast-elwise a b)))

(defclass Add (Func)
  ((reduce :initarg :reduce :initform nil :accessor func-reduce)
   (id :initarg :id :initform nil :accessor func-id)
   (wrap-around :initarg :wrap-around :accessor add-wrap-around)))
(defmethod forward ((op Add) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op Add) &optional dout) (values dout dout))
(defmethod lower ((op Add) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%add a b :reduction (func-reduce op) :id (or (func-id op) (gensym "BID")) :wrap-around (add-wrap-around op))))))

(defclass Mul (Func)
  ((reduce :initarg :reduce :initform nil :accessor func-reduce)
   (wrap-around :initarg :wrap-around :accessor mul-wrap-around)))
(defmethod forward ((op Mul) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op Mul) &optional dout)
  (multiple-value-bind (x y) (apply #'values (func-variables op))
    (values (!mul y dout) (!mul x dout))))
(defmethod lower ((op Mul) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%mul a b :reduction (func-reduce op) :wrap-around (mul-wrap-around op))))))

(defclass IDiv (Func)
  ((reduce :initarg :reduce :initform nil :accessor func-reduce)))
(defmethod forward ((op IDiv) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op IDiv) &optional prev-grad))
(defmethod lower ((op IDiv) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%idiv a b :reduction (func-reduce op))))))

(defclass MaxOp (Func) ((reduce :initarg :reduce :initform nil :accessor func-reduce)))
(defmethod forward ((op MaxOp) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op MaxOp) &optional dout)
  ;;(warn "WIP: MaxOp")
  )
(defmethod lower ((op MaxOp) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%max a b :reduction (func-reduce op))))))

(defclass GCDOp (Func) ((reduce :initarg :reduce :initform nil :accessor func-reduce)))
(defmethod forward ((op GCDOp) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op GCDOp) &optional dout) (values nil nil))
(defmethod lower ((op GCDOp) &rest inputs)
  (multiple-value-bind (a b) (apply #'values inputs)
    (with-context (out (%gcd a b :reduction (func-reduce op))))))
;; Unary
(defclass Neg (Func) nil)
(defmethod forward ((op Neg) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op Neg) &optional dout) (values (!neg dout)))
(defmethod lower ((op Neg) &rest inputs) (with-context (a (%neg (car inputs)))))

(defclass SinNode (Func) nil)
(defmethod forward ((op SinNode) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op SinNode) &optional dout) (values (!mul dout (!cos (car (func-variables op))))))
(defmethod lower ((op SinNode) &rest inputs) (with-context (a (%sin (car inputs)))))
(defun !sin (x) (forward (make-instance 'SinNode) x))

(defclass ExpNode (Func) nil)
(defmethod forward ((op ExpNode) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op ExpNode) &optional dout) (values (!mul (car (func-variables op)) dout)))
(defmethod lower ((op ExpNode) &rest inputs)
  (with-context
    (m (%mul (car inputs) (%fconst (/ (log 2)) :dtype (dtype-of (car (func-variables op))))))
    (a (%exp2 m))))

(defclass LogNode (Func) nil)
(defmethod forward ((op LogNode) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op LogNode) &optional dout) (values (!mul dout (!recip (car (func-variables op))))))
(defmethod lower ((op LogNode) &rest inputs)
  (with-context
    (a (%log2 (car inputs)))
    (b (%mul a (%fconst (log 2) :dtype (dtype-of (car (func-variables op))))))))

(defclass SqrtNode (Func) nil)
(defmethod forward ((op SqrtNode) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op SqrtNode) &optional prev-grad) (!mul prev-grad (!recip (!mul (!sqrt (car (func-variables op))) (!const prev-grad 2)))))
(defmethod lower ((op SqrtNode) &rest inputs) (with-context (a (%sqrt (car inputs)))))

(defun !exp (x)
  "
```
(!exp x)
```

Computes `(exp x)`.
"
  (forward (make-instance 'ExpNode) x))
(defun !log (x)
  "
```
(!log x)
```

Computes `(log x)`.
"
  (forward (make-instance 'LogNode) x))
(defun !sqrt (x)
  "
```
(!sqrt x)
```
Computes `(sqrt x)`.
"
  (forward (make-instance 'SqrtNode) x))

(defclass Recip (Func) nil)
(defmethod forward ((op Recip) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op Recip) &optional dout)
  (let ((ret (!recip (car (func-variables op)))))
    (values (!mul (!mul (!neg dout) ret) ret)))) ;; -dout / x^2
(defmethod lower ((op Recip) &rest inputs) (with-context (a (%recip (car inputs)))))

(defclass Cast (Func)
  ((dtype-frm :initarg :dtype-frm :accessor cast-dtype-frm)
   (dtype-to :initarg :dtype-to   :accessor cast-dtype-to)))
(defmethod forward ((op Cast) &rest tensors) (st "A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op Cast) &optional prev-grad) (values prev-grad (!cast prev-grad (cast-dtype-frm op))))
(defmethod lower ((op Cast) &rest inputs) (with-context (a (%cast (first inputs) (second inputs) (cast-dtype-to op)))))
(defun !cast (x dtype &key (out (make-tensor (tensor-shape x) :dtype dtype :order (tensor-order x))))
  (declare (type tensor x out) (type dtype-t dtype))
  (if (eql (tensor-dtype x) dtype)
      x
      (forward (make-instance 'Cast :dtype-frm (tensor-dtype x) :dtype-to dtype) out x)))
;; ~~ wrappers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(declaim (ftype (function (Tensor Tensor &key (:reduce boolean) (:id t)) (values Tensor &optional)) !add))
(declaim (ftype (function (Tensor Tensor &key (:reduce boolean)) (values Tensor &optional)) !sub !mul !div !maximum !minimum !idiv))
(defun !add (a b &key (reduce nil) (id nil))
  "
```
(!add a b &key (reduce nil))
;; or
(!+ &rest tensors)
```

Adds `a` and `b`. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (apply #'forward (make-instance 'Add :reduce reduce :id id :wrap-around *wrap-around-mode*) (broadcast-elwise a b)))
(defun !mul (a b &key (reduce nil))
  "
```
(!mul a b &key (reduce nil))
;; or
(!* &rest tensors)
```

Multiplies `a` and `b`. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (apply #'forward (make-instance 'Mul :reduce reduce :wrap-around *wrap-around-mode*) (broadcast-elwise a b)))
(defun !sub (a b &key (reduce nil))
  "
```
(!sub a b &key (reduce nil))
;; or
(!- &rest tensors)
```

Subtracts `b` from `a`. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (!add a (!neg b) :reduce reduce))
(defun !div (a b &key (reduce nil))
  "
```
(!div a b &key (reduce nil))
;; or
(!/ &rest tensors)
```

Divides `a` by `b`. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (!mul a (!recip b) :reduce reduce))
(defun !idiv (a b &key (reduce nil))
  "
```
(!idiv a b &key (reduce nil))
```

Assuming both of a and b are the integer, divides `a` by `b` and returns the integer part. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (assert (and (caten/common.dtype:dtype/integerp (dtype-of a)) (caten/common.dtype:dtype/integerp (dtype-of a))) ()
	  "!idiv only supports for integer but got ~a and ~a" a b)
  (apply #'forward (make-instance 'IDiv :reduce reduce) (broadcast-elwise a b)))
(defun !maximum (a b &key (reduce nil))
  "
```
(!maximum a b &key (reduce nil))
```
Returns the maximum of `a` and `b`. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (apply #'forward (make-instance 'MaxOp :reduce reduce) (broadcast-elwise a b)))
(defun !minimum (a b &key (reduce nil))
  "
```
(!minimum a b &key (reduce nil))
```
Returns the minimum of `a` and `b`. If `reduce` is T, it will reduce the result. (Broadcast)
"
  (!neg (!maximum (!neg a) (!neg b) :reduce reduce)))
(defun !gcd (a b &key (reduce nil))
  "
```
(!gcd a b &key (reduce nil))
```
Returns the greatest common divisor of `a` and `b`. If `reduce` is T, it will reduce the result. (Broadcast)

`a`, `b` are expected to be integer scalars. (dedicated to the view computation)
"
  (apply #'forward (make-instance 'GCDOp :reduce reduce) (broadcast-elwise a b)))
(defun !lcm (a b)
  "
```
(!lcm a b)
```
Returns the least common multiple of `a` and `b`.

`a`, `b` are expected to be integer scalars.
"
  (!div (!mul a b) (!gcd a b)))
(macrolet ((def (name b) `(defun ,name (&rest args) (reduce ,b args))))
  (def !+ #'!add)
  (def !- #'!sub)
  (def !* #'!mul)
  (def !/ #'!div))
(macrolet ((def (name cls doc)
	     `(progn
		(declaim (ftype (function (Tensor) (values Tensor &optional)) ,name))
		(defun ,name (x)
                  ,(format nil "
```
(~(~a~) tensor)
```
~(~a~) computes the ~a of the tensor.
"
                           name name doc)
                  (declare (type Tensor x)) (forward (make-instance ',cls) x)))))
  (def !neg Neg "negative value")
  (def !recip Recip "reciprocal"))
(declaim (ftype (function (Tensor) (values Tensor &optional)) !signum !abs))
(defun !signum (x)
  "
```
(!signum x)
```

Returns the sign of the tensor. If the tensor is positive, it returns 1. If the tensor is negative, it returns -1. If the tensor is zero, it returns 0.
"
  (flet ((->const (val) (make-scalar val :dtype (tensor-dtype x))))
    (let ((zeros (!where (!eq x (->const 0)) (->const 0) (->const 1))))
      (!mul zeros (!where (!>= x (->const 0)) (->const 1) (->const -1))))))
(defun !abs (x)
  "
```
(!abs x)
```

Returns the absolute value of the tensor.
"
  (!mul (!signum x) x))

;; ~~ Compare Ops ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(macrolet ((def (name cls aop)
	     `(progn
		(defclass ,cls (Func) nil)
		(defmethod forward ((op ,cls) &rest tensors) (st "OUT[~] A[~] B[~] -> OUT[~]" (tensors)))
		(defmethod lower ((op ,cls) &rest inputs)
		  (with-context (out (,aop nil nil (nth 1 inputs) (nth 2 inputs) :out (nth 0 inputs)))))
		(defun ,name (x y)
                  ,(format nil "
```
(~a x y)
```

Compares x and y element-wise and returns the result as a boolean tensor.
" name)
		  (declare (type Tensor x y))
		  (multiple-value-bind (x y)
		      (bc "A[~] B[~] -> A[~] B[~]" (x y))
		    (forward (make-instance ',cls) (make-tensor (tensor-shape x) :dtype :bool :order (tensor-order x)) x y))))))
  (def !<  LessThan     %<)
  (def !<= LessEqual    %<=)
  (def !>  GreaterThan  %>)
  (def !>= GreaterEqual %>=)
  (def !eq TensorEqual %=)
  (def !neq NotEqual %!=))
;; ~~ TernaryOps ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Where (Func) nil)
(defmethod forward ((op Where) &rest tensors)
  (assert (eql (tensor-dtype (nth 1 tensors)) (tensor-dtype (nth 2 tensors)))
	  ()
	  "Assertion Failed: A.dtype != B.dtype")
  (st "MAP[~] A[~] B[~] -> A[~]" (tensors)))
(defmethod backward ((op Where) &optional prev-grad)
  (multiple-value-bind (c) (apply #'values (func-variables op))
    (values
     nil
     (!where c prev-grad (zeros-like prev-grad))
     (!where c (zeros-like prev-grad) prev-grad))))
(defmethod lower ((op Where) &rest inputs) (with-context (out (%where (nth 0 inputs) (nth 1 inputs) (nth 2 inputs)))))
(defun !where (condition x y)
  "
```
(!where condition x y)
```

Selects elements from `x` or `y` based on the `condition`. If the condition is true, it selects the element from `x`, otherwise from `y`.
"
  (declare (type Tensor condition x y))
  (multiple-value-bind (condition x y)
      (bc "C[~] X[~] Y[~] -> C[~] X[~] Y[~]" (condition x y))
    (forward (make-instance 'Where) condition (!contiguous x) y)))

(declaim (ftype (function (Tensor (or number symbol)) (values Tensor &optional)) !const))
(defun !const (tensor value)
  "
```
(!const tensor value)
```
Creates a constant tensor with the specified value from the tensor.
"
  (declare (type tensor tensor) (type (or number symbol) value))
  (make-scalar value :dtype (dtype-of tensor)))

;; ~~ Proceed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass ProceedNode (Func) nil)
(defmethod forward ((op ProceedNode) &rest inputs) (st "A[~] -> A[~]" (inputs)))
(defmethod backward ((op ProceedNode) &optional prev-grad) prev-grad)
(defmethod lower ((op ProceedNode) &rest inputs)
  (declare (ignore inputs))
  (with-context (_ (%make-tensor (%shape (shape (car (func-variables op)))) :dtype (dtype-of (car (func-variables op))) :order (order (car (func-variables op))) :id (tensor-id (car (func-variables op))) :from (tensor-buffer (car (func-variables op)))))))
(defun %apply-proceed (proceed-output)
  "Proceed-Output[Tensor] - a realized tensor."
  (declare (type tensor proceed-output))
  (let* ((detached-tensor (st "A[~] -> A[~]" (proceed-output))))
    (setf (tensor-buffer detached-tensor) (tensor-buffer proceed-output)
          (tensor-id detached-tensor) (tensor-id proceed-output))
    (let ((output (forward (make-instance 'ProceedNode) detached-tensor)))
      (setf (tensor-buffer output) (tensor-buffer detached-tensor))
      output)))

(defclass IndexComponents (Func) nil)
(defmethod forward ((op IndexComponents) &rest inputs) (st "A[~] -> A[~]" (inputs)))
(defmethod backward ((op IndexComponents) &optional prev-grad))
(defmethod lower ((op IndexComponents) &rest inputs)
  (with-context (_ (%index-components (car inputs) (%shape (shape (car (func-variables op))))))))
(defgeneric !index-components (tensor) (:documentation "
```
(!index-components object)
```

Returns the index components of the tensor. object can be either of tensor or list.
"))

(defmethod !index-components ((tensor Tensor))
  (forward (make-instance 'IndexComponents) tensor))
(defmethod !index-components ((shape list))
  (forward (make-instance 'IndexComponents) (make-tensor shape)))

;; ~~~ Bitwise ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(macrolet ((def (name op lisp doc)
	     `(progn
		(defclass ,name (Func) nil)
		(defmethod forward ((op ,name) &rest inputs) (st "A[~] B[~] -> A[~]" (inputs)))
		(defmethod backward ((op ,name) &optional prev-dout) (declare (ignore prev-dout)) nil)
		(defmethod lower ((op ,name) &rest inputs) (with-context (_ (,op (car inputs) (second inputs)))))
		(defun ,lisp (x y)
                  ,(format nil "
```
(~a x y)
```

Computes the ~a of the tensor."
                           lisp doc)
		  (declare (type tensor x y))		
		  (multiple-value-bind (x y)
		      (bc "A[~] B[~] -> A[~] B[~]" (x y))
		    (forward (make-instance ',name) x y))))))
  (def OrNode %or !or "logical/bitwise or")
  (def XorNode %xor !xor "logical/bitwise xor")
  (def AndNode %and !and "logical/bitwise and"))
