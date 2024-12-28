(in-package :caten/nn)

(defmodel (BatchNorm (dims &key (eps 1e-5) (affine t) (bias t)) :documentation "
```
(BatchNorm dims &key (eps 1e-5) (affine t) (bias t))
```
Batch Normalization Layer
")
    ((affine (if (eql affine t) (linspace `(,@dims) 0 1 :requires-grad t) affine))
     (bias   (if (eql bias t)   (linspace `(,@dims) 0 0 :requires-grad t) bias))
     (eps eps)
     (axes (map 'list #'- (map 'list #'1+ (range 0 (length dims)))))))

(defmethod call ((op BatchNorm) &rest inputs)
  (multiple-value-bind (x running-mean running-invstd) (apply #'values inputs)
    (setf running-mean (or running-mean (!mean x :axis (slot-value op 'axes) :keepdims t))
          running-invstd (or running-invstd (!recip (!add (!std x :axis (slot-value op 'axes) :keepdims t) (fconst (slot-value op 'eps))))))
    (st "A[~] Mean[~] InvStd[~] -> A[~]" (x running-mean running-invstd))
    (with-slots ((gamma affine) (beta bias) (eps eps)) op
      (let* ((x (!sub x running-mean))
             (x (if gamma (!mul x gamma) x))
             (ret (!mul x running-invstd)))
        (if beta (!add ret beta) ret)))))

(defun !batch-norm (x &optional weight bias mean invstd (axis 1) (eps 1e-5))
  "
```
(!batch-norm x &optional weight bias mean invstd (axis 1) (eps 1e-5))
```
Computes the batch normalization of a tensor `x` with optional `weight` and `bias` tensors.
"
  (declare (type tensor x)
           (type (or null tensor) weight bias mean invstd))
  (let* ((axes (normalize-axes x axis))
         (dims (map 'list #'(lambda (n) (nth n (shape x))) axes))
         (model (BatchNorm dims :eps eps :affine weight :bias bias)))
    (forward model x mean invstd)))

(defmodel (LayerNorm (dims &key (eps 1e-5) (affine t) (bias t)) :documentation "
```
(LayerNorm dims &key (eps 1e-5) (affine t) (bias t))
```
Layer Normalization Layer
")
    ((affine (if (eql affine t) (linspace `(,@dims) 0 1 :requires-grad t) affine))
     (bias   (if (eql bias t) (linspace `(,@dims) 0 0 :requires-grad t) bias))
     (eps eps)))
(defmethod call ((op LayerNorm) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((gamma affine) (beta bias) (eps eps)) op
      (let* ((u (!mean x :axis -1 :keepdims t))
	     (d (!sub x u))
	     (s (!mean (!mul d d) :axis -1 :keepdims t))
	     (x (!div d (!sqrt (!add s (!const x eps)))))
	     (x (if gamma (!mul x gamma) x))
	     (x (if beta  (!add x beta) x)))
	x))))
(defun !layer-norm (x normalized-shape &key (weight) (bias) (eps 1e-5))
  "
```
(!layer-norm x normalized-shape &key (weight) (bias) (eps 1e-5))
```
Computes the layer normalization of a tensor `x` with optional `weight` and `bias` tensors.
"
  (declare (type tensor x)
           (type (or null tensor) weight bias))
  (forward (LayerNorm normalized-shape :eps eps :affine weight :bias bias) x))

(defmodel (RMSNorm (dims &key (eps 1e-5) (weight t)) :documentation "
```
(RMSNorm dims &key (eps 1e-5) (weight t))
```
Root Mean Square Normalization Layer
")
    ((weight (if (eql weight t) (linspace `(,@dims) 0 1 :requires-grad t) weight))
     (eps eps)))
(defmethod call ((op RMSNorm) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (with-slots ((weight weight) (eps eps)) op
    (let* ((x (car inputs))
	   (norm (!mul x (!recip (!sqrt (!add (!const x eps) (!mean (!mul x x) :axis -1 :keepdims t)))))))
      (if weight
          (!mul norm weight)
          norm))))
(defun !rms-norm (x normalized-shape &key (weight) (eps 1e-5))
  "
```
(!rms-norm x normalized-shape &key (weight) (eps 1e-5))
```
Computes the root mean square normalization of a tensor `x` with optional `weight` tensor.
"
  (declare (type tensor x)
           (type (or null tensor) weight))
  (forward (RMSNorm normalized-shape :eps eps :weight weight) x))
