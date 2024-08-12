(cl:in-package :caten/nn)

;; Goal: compatible with MLX
;; from: https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/activations.py
;; [a list of activations to implement]
;; !sigmoid
;; !relu
;; leaky-relu
;; log_softmax
;; elu
;; relu6
;; sofmtax
;; softplus
;; softsign
;; softshrink
;; celu
;; silu
;; log_sigmoid
;; gelu
;; gelu_approx
;; glu
;; selu
;; prelu
;; mish
;; hardswish
;; hard_tanh
;; softmin
;; Tanh

(defun !relu (x)
  "ReLU"
  (declare (type Tensor x))
  (!max x (fconst 0)))

(in-package :caten/nn.test)

(deftest test-relu
  (ok (every #'(lambda (x) (>= x 0)) (elements (proceed (!relu (ax+b `(10 10) 1 -5)))))))
