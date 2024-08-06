(in-package :caten/nn)

(defmethod st/unfold ((conv ConvND) x)
  )

;; TODO: Conv, Embedding, Batch/Layer Normalization
(defmodule (ConvND ((&key (groups 1) (stride 1) (dilation 1) (padding 0) (bias t))
		    :groups groups :stride stride :dilation dilation :padding padding :bias bias))
    ((weight :initform nil :accessor convnd-weight)
     (bias :initform nil :accessor convnd-bias))
    :documentation "Applies a convolution over a tensor with a given `weight` and optional `bias`.
NOTE: unlike PyTorch, this implementation is not limited to only 2d convolutions and instead works for any number of dimensions.
"
    :impl impl-winograd
    :forward st/unfold)

(defmethod impl-winograd ((conv ConvND) x)
  (with-slots ((weight weight) (bias bias)) conv
    (when (null weight)
      
      )))

(defmethod impl-winograd ((conv ConvND) x)
  (let* ((weight (convnd-weight conv))
	 (hw (subseq (shape weight) 2))
	 (attrs (module-attrs conv)))
    (multiple-value-bind (groups stride dilation padding bias)
	(values (getattr attrs :groups) (getattr attrs :stride) (getattr attrs :dilation) (getattr attrs :padding) (getattr attrs :bias))
      (multiple-value-bind (cs cin_ cout cin)
	  (apply #'values `(,@(subseq (shape x) 0 2) ,@(subseq (shape weight) 0 2)))
	;; assert groups*cin == cin_ and len(self.shape) == len(weight.shape)
	(when (and (numberp groups) (numberp cin) (numberp cin_)))
	  (assert (= cin (* groups cin_))
		  ()
		  "Input Tensor shape ~a do not match the shape of the weights ~a. ~a vs ~a (= cin (* groups cin_))"
		  (shape x) (shape weight) cin (* groups cin_)))
      (assert (= (ndim weight) (ndim x)) () "Input Tensor Shape ~a do not match the shape of the weights ~a" (shape x) (shape weight))
      (let ((x (pool2d (pad2d x (padding2d-shape x padding (length hw))) hw stride dilation)))
	
	))))

;; TODO: (defmethod export-to-onnx ((conv ConvND) x) ...)

(in-package :caten/nn.test)
