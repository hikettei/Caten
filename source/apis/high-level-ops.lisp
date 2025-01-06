(in-package :caten/apis)

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

(macrolet ((defreduce (model description op &key (initial-element 0.0))
	     `(defmodule (,model ((&key (axis t) (keepdims nil)) :axis axis :keepdims keepdims))
		  ((x :accessor reduce-x) (ret :accessor reduce-ret))
		  :documentation ,description
		  :forward st/reduction
		  :impl
		  ((op x)
		   (with-attrs ((axis :axis) (keepdims :keepdims)) op
		     (multiple-value-bind (new-shape new-view) (parse-reduce-axes x axis)
		       (let* ((out (make-tensor new-shape :dtype (dtype-of x) :order (order x) :initial-element ,initial-element))
			      (out (apply #'!view out new-view))
			      (out (,op out x :reduce t))
			      (out (if keepdims
				       out
				       (apply #'!view out (map 'list #'(lambda (x) (if (and (listp x) (eql (car x) :~)) `(:~ 1) t)) new-view)))))
                         (setf (reduce-x op) x (reduce-ret op) out)
                         out)))))))
  (defreduce SumNode "Sum tensors along axis" !add)
  (defreduce MaxReduce "Max" !maximum :initial-element (-inf))
  (defreduce MinReduce "Min" !minimum :initial-element (inf)))
;; TODO(hikettei) replace the backward with _reduce_bw (as commented out below)
(defun _reduce_bw (op prev-grad)
  (with-slots ((x x) (ret ret)) op
    (with-attrs ((axis :axis)) op
      (let* ((max-is-1s (!where (!eq x ret) (!const x 1.0) (!const x 0.0)))
             (div (!sum max-is-1s :axis axis :keepdims t)))
        (!mul (!div max-is-1s div) (!contiguous prev-grad))))))

;; (defmethod backward ((op MaxReduce) &optional prev-grad) (_reduce_bw op prev-grad))
;; (defmethod backward ((op MinReduce) &optional prev-grad) (_reduce_bw op prev-grad))

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

(macrolet ((defreduce (f model doc)
	     `(progn
		(declaim (ftype (Function (Tensor &key (:axis t) (:keepdims boolean)) (values Tensor &optional)) ,f))
		(defun ,f (x &key (axis t) (keepdims nil))
                  ,(format nil "
```
(~(~a~) x &key (axis t) (keepdims nil))
```

Compute the ~a of the tensor.
" f doc)
                  (forward (,model :axis axis :keepdims keepdims) x)))))
  (defreduce !sum SumNode "sum")
  (defreduce !mean MeanNode "mean")
  (defreduce !max MaxReduce "maximum")
  (defreduce !min MinReduce "minimum"))

;; ~~~ gemm ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodule (Matmul (()) :where "A[~ i j] B[~ j k] -> A[~ i k]")
    ()
    :documentation "Gemm"
    :impl ((mm x y)
	   (multiple-value-bind (n1 n2) (values (ndim x) (ndim y))
             (assert (= n1 n2) () "Cannot multiply matrices with different dimensions. Are they properly broadcasted?~%A: ~a~%B: ~a" x y)
	     (let* ((mid (loop for i upfrom 0 below (min (- n1 1) (- n2 1) 1) collect 1))
		    (x (!reshape x `(,@(butlast (shape x) 1) ,@mid ,(car (last (shape x))))))
		    (y (!reshape y `(,@(butlast (shape y) 2) ,@mid ,@(last (shape y) (min n2 2))))))
	       (let ((z (!mul x (!transpose y -1 (- (min n2 2))))))
                 (!reshape (!sum z :axis -1) (butlast (shape z))))))))

(defun !matmul (a b)
  "
```
(!matmul a b)
```

Performs matrix multiplication between the two tensors `a` and `b`.
"
  (if (= (ndim a) (ndim b))
      (forward (make-instance 'Matmul) a b)
      (multiple-value-bind (a b) (bc "A[~ i j] B[~ j k] -> A[~ i j] B[~ j k]" (a b))
        (forward (make-instance 'Matmul) a b))))

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
(defun !sinh (x)
  "
```
(!sinh x)
```
"
  (forward (SinhNode) x))
(defun !cosh (x)
  "
```
(!cosh x)
```
"
  (forward (CoshNode) x))
(defun !tanh (x)
  "
```
(!tanh x)
```
"
  (forward (TanhNode) x))
(defun !cos (x)
  "
```
(!cos x)
```
"
  (forward (CosNode) x))
(defun !tan (x)
  "
```
(!tan x)
```
"
  (forward (TanNode) x))

(defmodule (Exp2Node (()) :where "A[~] -> A[~]")
    ()
    :documentation "Exp2"
    :impl ((exp2 x) (!exp (!mul x (fconst (log 2) :dtype (dtype-of x))))))

(defmodule (Log2Node (()) :where "A[~] -> A[~]")
    ()
    :documentation "Log2"
    :impl ((log2 x) (!div (!log x) (fconst (log 2) :dtype (dtype-of x)))))

(defun !log2 (x)
  "
```
(!log2 x)
```
"
  (forward (Log2Node) x))
(defun !exp2 (x)
  "
```
(!exp2 x)
```
"
  (forward (Exp2Node) x))

(defmodule (ExptNode (()) :where "Base[~] Power[~] -> Base[~]")
    ()
    :documentation "Expt(base power)"
    :impl ((expt base power)
           ;; Ref: https://github.com/tinygrad/tinygrad/blob/master/tinygrad/tensor.py#L3132-L3168
           (let* ((ret (!exp (!mul (!log (!abs base)) power)))
                  (neg-base-map (!< base (!const base 0)))
                  (neg-base (!where neg-base-map (!const base 1) (!const base 0)))
                  (correct-sign (!add (!const power 1) (!mul neg-base (!- (!cos (!mul power (!const power pi))) (!const base 1)))))
                  (inject-nan (!where (!and neg-base-map (!neq power (!truncate power))) (!const base (nan)) (!const base 1))))
             (!where (!and (!eq base (!const base 0)) (!eq power (!const power 0))) (!const base 1) (!* ret correct-sign inject-nan)))))

(defun !expt (base power &aux (power (if (and (tensor-p power) (= 0 (ndim power)))
                                         (let ((folded (canonicalize-int power)))
                                           (if (numberp folded) folded power))
                                         power)))
  "
```
(!expt base power)
```
Computes the power of base with power."
  (declare (type tensor base) (type (or number tensor) power))
  (when (numberp power)
    (cond
      ((< power 0)
       (return-from !expt (!expt (!recip base) (- power))))
      ((= power 0)
       (return-from !expt (!add (!const base 1) (!mul base (!const base 0)))))
      ((= power (+ 0.5 (truncate (- power 0.5))))
       (return-from !expt (!mul (!sqrt base) (!expt base (truncate (- power 0.5))))))
      ((= power (truncate power))
       (return-from !expt (!mul (!square (!expt base (floor power 2))) (if (= 0 (mod power 2)) (!const base 1) base))))))
  (let ((power (if (numberp power) (!const base power) power)))
    (apply #'forward (ExptNode) (broadcast-elwise base power))))

;; ~~ Trunc/Ceil/Floor ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodule (TruncateNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Truncate(x)"
    :impl ((trunc x) (!cast (!cast x *default-int*) (dtype-of x))))

(defun !truncate (x)
  "
```
(!truncate x)
```
"
  (forward (TruncateNode) x))

(defmodule (CeilingNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Ceiling(x)"
    :impl ((ceil x) (let ((b (!truncate x))) (!where (!> x b) (!add b (!const b 1)) b))))

(defun !ceiling (x)
  "
```
(!ceiling x)
```"
  (forward (CeilingNode) x))

(defmodule (FloorNode (()) :where "A[~] -> A[~]")
    ()
    :documentation "Floor(x)"
    :impl ((ceil x) (let ((b (!truncate x))) (!where (!< x b) (!sub b (!const b 1)) b))))

(defun !floor (x)
  "
```
(!floor x)
```
"
  (forward (FloorNode) x))

;; ~~ Linalg ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodule (TrilNode ((&key (diagonal 0)) :diagonal diagonal) :where "A[~ n m] -> A[~ n m]")
    ()
    :documentation "Returns the lower triangular part of the tensor (>= 2D) or batch of matrices input"
    :impl ((tril x)
           (multiple-value-bind (n m) (apply #'values (last (shape x) 2))
             (with-attrs ((diagonal :diagonal)) tril
               (let* ((~ (loop repeat (- (ndim x) 2) collect 1))
                      (i (!index-components `(,@~ ,n 1)))
                      (j (!index-components `(,@~ 1 ,m)))
                      (k (->iconst diagonal)))
                 (!where (!>= i (!- j k)) x (!const x 0)))))))

(defmodule (TriuNode ((&key (diagonal 0)) :diagonal diagonal) :where "A[~ n m] -> A[~ n m]")
    ()
    :documentation "Returns the upper triangular part of the tensor (>= 2D) or batch of matrices input"
    :impl ((tril x)
           (multiple-value-bind (n m) (apply #'values (last (shape x) 2))
             (with-attrs ((diagonal :diagonal)) tril
               (let* ((~ (loop repeat (- (ndim x) 2) collect 1))
                      (i (!index-components `(,@~ ,n 1)))
                      (j (!index-components `(,@~ 1 ,m)))
                      (k (->iconst diagonal)))
                 (!where (!<= i (!- j k)) x (!const x 0)))))))

(defun !tril (x &key (diagonal 0))
  "
```
(!tril x &key (diagonal 0))
```

Returns the lower triangular part of the tensor (>= 2D) or batch of matrices input.
"
  (forward (TrilNode :diagonal diagonal) x))

(defun !triu (x &key (diagonal 0))
  "
```
(!triu x &key (diagonal 0))
```

Returns the upper triangular part of the tensor (>= 2D) or batch of matrices input."
  (forward (TriuNode :diagonal diagonal) x))

(defun st/reduction1 (op x)
  (let ((out (st/reduction op x)))
    (setf (tensor-dtype out) *default-int*)
    out))

(defmodule (ArgMaxNode ((&key (axis 0) (keepdims nil)) :axis axis :keepdims keepdims))
    ()
    :documentation ""
    :forward st/reduction1
    :impl ((argmax x)
	   (with-attrs ((axis :axis) (keepdims :keepdims)) argmax
             (let* ((axis (normalize-axis x axis))
                    (idx (!gid x axis))
                    ;; idx = shape-1, shape-2, ..., 2, 1, 0
                    (idx (!add (!- (!const idx (nth axis (shape x))) (!const idx 1)) (!mul idx (!const idx -1))))
                    (map (!where (!eq x (!max x :axis axis)) idx (!const idx 0))))
               (!cast (!- (!const idx (nth axis (shape x))) (!max map :axis axis :keepdims keepdims) (!const idx 1)) *default-int*)))))

(defmodule (ArgMinNode ((&key (axis 0) (keepdims nil)) :axis axis :keepdims keepdims))
    ()
    :documentation ""
    :forward st/reduction1
    :impl ((argmin x)
           (with-attrs ((axis :axis) (keepdims :keepdims)) argmin
             (!argmax (!neg x) :axis axis :keepdims keepdims))))

(defun !argmax (x &key (axis -1) (keepdims nil))
  "
```
(!argmax x &key (axis -1) (keepdims nil))
```

Returns the indices of the maximum values along an axis.
"
  (forward (ArgMaxNode :axis axis :keepdims keepdims) x))

(defun !argmin (x &key (axis -1) (keepdims nil))
  "
```
(!argmin x &key (axis -1) (keepdims nil))
```

Returns the indices of the minimum values along an axis.
"
  (forward (ArgMinNode :axis axis :keepdims keepdims) x))

;; ~~~ Statical Ops ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodule (VarianceNode ((&key (axis -1) (keepdims nil) (correction 1)) :axis axis :keepdims keepdims :correction correction))
    ()
    :documentation ""
    :forward st/reduction1
    :impl ((variance x)
           (with-attrs ((axis :axis) (keepdims :keepdims) (correction :correction)) variance
             (let* ((squares (!square (!- x (!mean x :axis axis :keepdims t))))
                    (n (apply #'!*
                              (loop for si in (shape x)
                                    for so in (shape (!sum squares :axis axis))
                                    if (not (eql si so)) collect (->fconst si)))))
               (!div (!sum squares :axis axis :keepdims keepdims) (!maximum (fconst 0) (!- n (->fconst correction))))))))

(defun !variance (x &key (axis -1) (keepdims nil) (correction 1))
  "
```
(!variance x &key (axis -1) (keepdims nil))
```

Returns the variance of the tensor elements along an axis.
"
  (declare (type tensor x))
  (forward (VarianceNode :axis axis :keepdims keepdims :correction correction) x))

(defmodule (StdNode ((&key (axis -1) (keepdims nil) (correction 1)) :axis axis :keepdims keepdims :correction correction))
    ()
    :documentation ""
    :forward st/reduction1
    :impl ((std x)
           (with-attrs ((axis :axis) (keepdims :keepdims) (correction :correction)) std
             (!sqrt (!variance x :axis axis :keepdims keepdims :correction correction)))))

(defun !std (x &key (axis -1) (keepdims nil) (correction 1))
  "
```
(!std x &key (axis -1) (keepdims nil) (correction 1))
```
Returns the standard deviation of the tensor elements along an axis.
"
  (declare (type tensor x))
  (forward (StdNode :axis axis :keepdims keepdims :correction correction) x))

;; ~~~ Dimension Manipulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodel (SplitNode (sizes &key (dim 0)))
    ((sizes sizes :type (or fixnum list))
     (dim dim :type fixnum)))

(defmethod call ((op SplitNode) &rest inputs)
  (with-attrs ((dim :dim) (sizes :sizes)) op
    (let* ((x (first inputs))
           (dim (normalize-axis (first inputs) dim))
           (_ (assert (integerp (nth dim (shape x))) () "SplitNode: The dimension to split must be an integer."))
           (sizes (if (integerp sizes) ;; Normalize to the list
                      (append
                       (loop for i upfrom 0 below (nth dim (shape x)) by sizes collect sizes)
                       (if (= 0 (mod (nth dim (shape x)) sizes)) ;; Compute Reminder
                           nil
                           (list (mod (nth dim (shape x)) sizes))))
                      sizes)))
      (declare (ignore _))
      (assert (= (length inputs) 1) () "SplitNode only accepts one input.")
      (flet ((~ (last-size size)
               (append
                (loop for i upfrom 0 below dim collect t)
                (list (list last-size (+ last-size size)))
                (loop for i upfrom (1+ dim) below (ndim x) collect t))))
        (assert (= (apply #'+ sizes) (nth dim (shape x))) () "SplitNode: the sum of sizes must be equal to the size of the split dimension.")
        (apply
         #'values
         (loop with total = 0
               for size in sizes
               collect (apply #'!view x (~ total size))
               do (incf total size)))))))

(defun !split (x sizes &key (dim 0))
  "
```
(!split x sizes &key (dim 0))
```

Splits the tensor into multiple tensors along the specified dimension.

If `sizes` is an integer, then it splits into equally sized chunks if possible, otherwise the last chunk will be smaller.
If `sizes` is a list, then it splits into `(length sizes)` chunks with size in `dim` according to `size`.

Note: The dimension to split must be an integer.
"
  (declare (type tensor x)
           (type (or fixnum list) sizes)
           (type fixnum dim))
  (forward (SplitNode sizes :dim dim) x))

(defun !chunk (x chunks &key (dim 0))
  "
```
(!chunk x chunks &key (dim 0))
```
Splits the tensor into `chunks` number of tensors along the specified dimension.
"
  (declare (type tensor x)
           (type (integer 0 *) chunks)
           (type fixnum dim))
  (let ((dim (normalize-axis x dim)))
    (assert (nth dim (shape x)) () "The dimension to split must be an integer.")
    (forward (SplitNode (ceiling (/ (nth dim (shape x)) chunks)) :dim dim) x)))

(defmodel (ConcatenateNode (dim)) ((dim dim :type fixnum)))
(defmethod call ((op ConcatenateNode) &rest inputs)
  (let ((dim (normalize-axis (car inputs) (slot-value op 'dim))))
    (flet ((s (tensor)
             (let ((shape (copy-list (tensor-shape tensor))))
               (setf (nth dim shape) 0)
               shape)))
      (assert (every #'(lambda (x) (equal (s x) (s (first inputs)))) (cdr inputs)) () "ConcatenateNode: All tensors must have the same shape except in the concatenation dimension.")
      (let* ((cat-dims (map 'list #'(lambda (x) (nth dim (shape x))) inputs))
             (_ (assert (every #'integerp cat-dims) () "ConcatenateNode: All concatenation dimensions must be integers."))
             (cat-tensor (make-tensor
                          (loop for s in (shape (car inputs))
                                for nth upfrom 0
                                if (= nth dim)
                                  collect (apply #'+ cat-dims)
                                else
                                  collect s)
                          :dtype (dtype-of (car inputs)))))
        (declare (ignore _))
        (flet ((~ (offset nth)
                 (loop with cat-dim = (nth nth cat-dims)
                       for s in (shape (car inputs))
                       for nth upfrom 0
                       if (= nth dim)
                         collect (list offset (+ offset cat-dim))
                       else
                         collect t)))
          (loop with offset = 0
                for nth upfrom 0
                for cat-dim in cat-dims
                for in in inputs do
                  (setf cat-tensor (!assign (apply #'!view-from-base cat-tensor (~ offset nth)) in))
                  (incf offset cat-dim))
          (apply #'!view-from-base cat-tensor
                 (loop for i upfrom 0 below (ndim cat-tensor)
                       if (= i dim) collect `(0 ,(apply #'+ cat-dims))
                         else collect t)))))))

(defun !concatenate (dim &rest tensors)
  "
```
(!concatenate dim &rest tensors)
```
Concatenates the tensor along the specified dimension. Note that all tensors must have the same shape except in the concatenation dimension, which must be an integer.
"
  (declare (type fixnum dim))
  (apply #'forward (ConcatenateNode dim) tensors))
;; ~~ Strides ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~"
(defgeneric !stride (order shape) (:documentation "
```
(!stride order shape)
```
Computes the strides of the tensor given the order and shape.

order is one of :column or :row. Shape is a list consisted of integers, symbols, and tensors. The list of tensor is returned.
"))

(defmethod !stride ((order (eql :column)) shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element (iconst 1))))
    (loop for i from 1 to (- num-dims 1) do
      (setf (nth i strides) (!* (nth (- i 1) strides) (->iconst (nth (- i 1) shape)))))
    strides))

(defmethod !stride ((order (eql :row)) shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element (iconst 1))))
    (loop for i downfrom (- num-dims 2) to 0 do
      (setf (nth i strides) (!* (nth (+ i 1) strides) (->iconst (nth (+ i 1) shape)))))
    strides))

;; ~~ Etc ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodel (ClipNode () :where "A[~] MIN[~] MAX[~] -> A[~]") ())
(defcall (clip ClipNode) (A[~] MIN[~] MAX[~])
  ;; c.f.: https://github.com/onnx/onnx/blob/main/docs/Changelog.md#clip-13
  (!minimum (!maximum a min) max))

(defun !clip (x min max)
  "
```
(!clip x min max)
```

!clip limits the given input within an interval. The interval is specified by the inputs 'min' and 'max'. min/max is either a number, a symbol, or a tensor.

The implementation follows the ONNX specification. https://github.com/onnx/onnx/blob/main/docs/Changelog.md#clip-13
"
  (declare (type Tensor x)
           (type (or number symbol tensor) max min))
  (let ((max (if (tensor-p max) max (!const x max)))
        (min (if (tensor-p min) min (!const x min))))
    (multiple-value-bind (x min max)
        (bc "A[~] MIN[~] MAX[~] -> A[~] MIN[~] MAX[~]" (x min max))
      (forward (ClipNode) x min max))))

(defmodel (ErfNode () :where "A[~] -> A[~]") ())
(defcall (erf ErfNode) (X[~])
  ;; erf(x) =~ x.sign() * (1 - ((((1.061405429 * t + -1.453152027) * t + 1.421413741) * t + -0.284496736) * t + 0.254829592) * t * (-(x.square())).exp())
  ;; Ref: https://stackoverflow.com/questions/457408/is-there-an-easily-available-implementation-of-erf-for-python
  (flet ((mla (x a b) (!add (!mul x (->fconst a)) (->fconst b))))
    (let* ((sign (!signum x))
           (x (!abs x))
           (t1 (!recip (!+ (!const x 1) (!mul (!const x 0.3275911) x))))
           (y (mla (mla (mla (mla t1 1.061405429 -1.453152027) t1 1.421413741) t1 -0.284496736) t1 0.254829592))
           (y (!* y t1 (!exp (!neg (!square x))))))
      (!mul sign (!- (!const x 1.0) y)))))

(defun !erf (x)
  "
```
(!erf x)
```
Computes the error function of x.
"
  (declare (type tensor x))
  (forward (ErfNode) x))
