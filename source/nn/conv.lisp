(in-package :caten/nn)

(defun conv-out-size (in padding dilation kernel-size stride)
  ;; TODO: Support Symbolic (needs !floor function)
  (floor (+ 1 (/ (+ in (* 2 padding) (* (- dilation) (- kernel-size 1)) -1) stride))))

(defun maybe-list (value kernel-size)
  (if (numberp value)
      (if (numberp kernel-size)
	  value
	  (loop repeat (length kernel-size) collect value))
      (progn
	(assert (and (listp value) (listp kernel-size) (= (length value) (length kernel-size))))
	value)))
;; TODO: Conv, Embedding, Batch/Layer Normalization
(defmodule (ConvND ((in-channels out-channels kernel-size &key (groups 1) (stride 1) (dilation 1) (padding 0) (bias t))
		    :in-channels in-channels
		    :out-channels out-channels
		    :kernel-size kernel-size
		    :groups groups
		    :stride (maybe-list stride kernel-size)
		    :dilation (maybe-list dilation kernel-size)
		    :padding (maybe-list padding kernel-size)
		    :bias bias))
    ((weight :initform nil :accessor convnd-weight)
     (bias :initform nil :accessor convnd-bias))
    :documentation "Applies a convolution over a tensor with a given `weight` and optional `bias`.
NOTE: unlike PyTorch, this implementation is not limited to only 2d convolutions and instead works for any number of dimensions.
"
    :impl impl/conv
    :forward
    ((conv x)
     (with-attrs ((in-channels :in-channels) (out-channels :out-channels) (kernel-size :kernel-size) (stride :stride) (dilation :dilation) (padding :padding)) conv
       (st "X[N C_in ~ND_Weights] -> X[N C_out ~ND_Weights]" (x)
	   (:C_in  . in-channels)
	   (:C_out . out-channels)
	   (:~ND_Weights . (loop with weight-sizes = (cddr (shape x))
				 for pad in padding for dil in dilation for k in kernel-size for str in stride
				 for s in weight-sizes
				 collect (conv-out-size s pad dil k str)))))))

(defmethod impl/conv :before ((conv ConvND) x)
  (with-slots ((weight weight) (bias bias)) conv
    (with-attrs ((groups :groups) (kernel-size :kernel-size) (in-channels :in-channels) (out-channels :out-channels) (requires-bias :bias)) conv
      (when (null weight)
	(let ((k (/ groups (* in-channels (apply #'* kernel-size)))))
	  (setf (convnd-weight conv) (uniform `(,out-channels ,(/ in-channels groups) ,@kernel-size) :low (- (sqrt k)) :high (sqrt k) :requires-grad t))
	  (when (and requires-bias (null bias))
	    (setf (convnd-bias conv) (uniform`(,out-channels) :low (- (sqrt k)) :high (sqrt k) :requires-grad t))))))))

(defmethod impl/conv ((conv ConvND) x)
  (let* ((weight (convnd-weight conv))
	 (hw (subseq (shape weight) 2)))
    (with-attrs ((out-channels :out-channels) (groups :groups) (stride :stride) (dilation :dilation) (padding :padding) (bias :bias)) conv
      (multiple-value-bind (bs cin_ cout cin)
	  (apply #'values `(,@(subseq (shape x) 0 2) ,@(subseq (shape weight) 0 2)))
	;; assert groups*cin == cin_ and len(self.shape) == len(weight.shape)
	(when (and (numberp groups) (numberp cin) (numberp cin_)))
	(assert (= cin (* groups cin_))
		()
		"Input Tensor shape ~a do not match the shape of the weights ~a. ~a vs ~a (= cin (* groups cin_))"
		(shape x) (shape weight) cin (* groups cin_))
	(assert (= (ndim weight) (ndim x)) () "Input Tensor Shape ~a do not match the shape of the weights ~a" (shape x) (shape weight))
	;; x = [bs, groups*cin, oy, ox, H, W]
	(let* ((x (_pool (!padding2d x (padding2d-shape padding (length hw))) hw stride dilation))
	       (rcout (floor (/ cout groups)))
	       (oyx   (slice (shape x) 2 (- (length hw)))))
	  ;; TODO: use winograd when fails to satisfy (or (not (some #'(lambda (x) (= x 3)) hw)) (not (eql stride 1)) (not (eql dilation 1)))
	  ;; x = x.reshape(bs, groups, cin, 1, *oyx, *HW).expand(bs, groups, cin, rcout, *oyx, *HW)
	  ;; x = x.permute(0,1,3,*[4+i for i in range(len(oyx))],2,*[4+len(oyx)+i for i in range(len(HW))])
	  (let* ((x (!reshape x (flatten (list bs groups cin 1 oyx hw))))
		 (x (apply #'!view x (append `(t t t (:~ ,rcout)) (loop for o in oyx collect t) (loop for h in hw collect t))))
                 (x (print x))
		 (x (!permute x (append (list 0 1 3) (map 'list #'(lambda (x) (+ 4 x)) (range 0 (length oyx))) (list 2) (map 'list #'(lambda (x) (+ 4 (length oyx) x)) (range 0 (length hw))))))
		 ;; x = (x * weight.reshape(1, groups, rcout, *[1] * len(oyx), cin, *HW))
		 (x (!mul x (!reshape (convnd-weight conv) (append (list 1 groups rcout) (loop repeat (length oyx) collect 1) (list cin) hw))))
		 ;; x = x.sum([-1-i for i in range(1+len(oyx))], keepdim=True, acc_dtype=acc_dtype)
		 (x (!sum x :axis (loop for i in (range 0 (+ 1 (length oyx))) collect (+ -1 (- i)))))
		 ;; x = x.reshape(bs, cout, *oyx)
		 (x (!reshape x (append (list bs cout) oyx))))
	    (if bias
		(progn
		  (assert (tensor-p (convnd-bias conv)) () "Bias for ~a should be a Tensor, getting ~a" conv (convnd-bias conv))
		  (!add x (!reshape (convnd-bias conv) (append (list 1) (list out-channels) (loop repeat (length hw) collect 1)))))
		x)))))))

(defun !convnd (x weight &key (bias nil) (groups 1) (stride 1) (dilation 1) (padding 0))
  "
```
(!convnd x weight &key (bias nil) (groups 1) (stride 1) (dilation 1) (padding 0))
```
Applies a convolutional layer over a tensor `x` with a given `weight` and optional `bias`."
  (declare (type Tensor x weight) (type (or null Tensor) bias))
  (assert (>= (ndim weight) 3) () "!convnd: weight should have at least 3 dimensions, got ~a" (ndim weight))
  (multiple-value-bind (c-in c-out kernel-size)
      (values (second (shape x)) (first (shape weight)) (subseq (shape weight) 2))
    (st "X[N C_in ~HW] Weight[C_out C_in/Group ~KERNEL_SIZE] -> X[N C_out ~KERNEL_SIZE]" (x weight) (:c-in/group . (/ c-in groups)))
    (when bias (st "Weight[C_out C_in/Group ~KERNEL_SIZE] Bias[C_out] -> Weight[]" (weight bias) (:c-in/group . (/ c-in groups))))
    (let ((convnd (ConvND c-in c-out kernel-size :groups groups :stride stride :dilation dilation :padding padding :bias (if bias t nil))))
      (setf (convnd-weight convnd) weight (convnd-bias convnd) bias)
      (forward convnd x))))
;; TODO: (defmethod export-to-onnx ((conv ConvND) x) ...)
(in-package :caten/nn.test)
