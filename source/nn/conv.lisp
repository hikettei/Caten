(in-package :caten/nn)
;; TODO: Implement !idiv 
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
	;; TODO: Initialize the xaviar distribution
	;; -> How can we deal w/ dense random module in the portable way?
	(setf (convnd-weight conv) (make-tensor `(,out-channels ,(/ in-channels groups) ,@kernel-size) :requires-grad t))
	(when (and requires-bias (null bias))
	    (setf (convnd-bias conv) (make-tensor `(,out-channels) :requires-grad t)))))))

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
	(let* ((x (_pool (pad2d x (padding2d-shape padding (length hw))) hw stride dilation))
	       (rcout (floor (/ cout groups)))
	       (oyx   (slice (shape x) 2 (- (length hw)))))
	  (if t;;(or (not (some #'(lambda (x) (= x 3)) hw)) (not (eql stride 1)) (not (eql dilation 1)))
	      ;; x = x.reshape(bs, groups, cin, 1, *oyx, *HW).expand(bs, groups, cin, rcout, *oyx, *HW)
	      ;; x = x.permute(0,1,3,*[4+i for i in range(len(oyx))],2,*[4+len(oyx)+i for i in range(len(HW))])
	      (let* ((x (!reshape x (flatten (list bs groups cin 1 oyx hw))))
		     (x (apply #'!view x (append `(t t t (:~ ,rcout)) (loop for o in oyx collect t) (loop for h in hw collect t))))
		     (x (!permute x (append (list 0 1 3) (loop for i in (range 0 (length oyx)) collect (+ i 4)) (list 2) (loop for i in (range 0 (length hw)) collect (+ 4 i (length oyx))))))
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
		    x))
	      ;;(progn
		;;(error "not ready: winograd")
	  ;;    )
	  ))))))

;; TODO: (defmethod export-to-onnx ((conv ConvND) x) ...)

(in-package :caten/nn.test)
