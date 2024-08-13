(in-package :caten/nn)
;; must be compatible with: https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/normalization.py
;; Batch Layer Group Norm
;; RMSNorm

(defmodel (LayerNorm (dims &key (eps 1e-5) (affine t) (bias t)))
    ((affine (when affine (make-tensor `(,dims) :requires-grad t)))
     (bias   (when bias (make-tensor `(,dims) :requires-grad t)))
     (eps eps)))
(defmethod call ((op LayerNorm) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((gamma affine) (beta bias) (eps eps)) op
      (let* ((u (!mean x :axis -1 :keepdims t))
	     (d (!sub x u))
	     (s (!mean (!mul d d) :axis -1 :keepdims t))
	     (x (!div d (!sqrt (!add (!contiguous s) (!const x eps)))))
	     (x (if gamma (!mul x gamma) x))
	     (x (if beta  (!add x beta) x)))
	x))))

(in-package :caten/nn.test)
