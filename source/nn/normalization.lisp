(in-package :caten/nn)
;; must be compatible with: https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/normalization.py
;; Batch Layer Group Norm
;; RMSNorm
(defmodel (BatchNorm (dims &key (eps 1e-5) (affine t) (bias t)))
    ((affine (when affine (make-tensor `(,dims) :requires-grad t)))
     (bias   (when bias (make-tensor `(,dims) :requires-grad t)))
     (eps eps)))
(defmethod call ((op BatchNorm) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((gamma affine) (beta bias) (eps eps)) op
      (let* ((axes (loop for s in (range 0 (ndim x)) unless (eql s 1) collect s))
	     (u (!mean x :axis axes :keepdims t))
	     (d (!sub x u))
	     (s (!mean (!mul d d) :axis axes :keepdims t))
	     (x (!div d (!sqrt (!add s (!const x eps)))))
	     (x (if gamma (!mul x gamma) x))
	     (x (if beta  (!add x beta) x)))
	x))))

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
	     (x (!div d (!sqrt (!add s (!const x eps)))))
	     (x (if gamma (!mul x gamma) x))
	     (x (if beta  (!add x beta) x)))
	x))))

(defmodel (RMSNorm (dims &key (eps 1e-5)))
    ((weight (make-tensor `(,dims) :requires-grad t))
     (eps eps)))
(defmethod call ((op RMSNorm) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (with-slots ((weight weight) (eps eps)) op
    (let* ((x (car inputs))
	   (norm (!mul x (!recip (!sqrt (!add (!const x eps) (!mean (!mul x x) :axis -1 :keepdims t)))))))
      (!mul norm weight))))

(in-package :caten/nn.test)
