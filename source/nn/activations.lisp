(in-package :caten/nn)
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

(defmodel (Sigmoid () :where "A[~] -> A[~]") ((ret)))
(defmethod call ((op Sigmoid) &rest inputs)
  (let ((x (car inputs)))
    (setf (slot-value op 'ret) (!recip (!add (fconst 1 :dtype (dtype-of x)) (!exp2 (!mul x (fconst (/ -1 (log 2)) :dtype (dtype-of x)))))))
    (slot-value op 'ret)))
(defmethod backward ((op Sigmoid) &optional prev-dout)
  (let ((ret (slot-value op 'ret)))
    (!mul (!mul ret (!add (fconst 1 :dtype (dtype-of ret)) (!neg ret))) prev-dout)))
(defun !sigmoid (x) (call (Sigmoid) x))

(defun !relu (x)
  "ReLU"
  (declare (type Tensor x))
  (!max x (fconst 0)))

(in-package :caten/nn.test)

(deftest test-relu
  (ok (every #'(lambda (x) (>= x 0)) (elements (proceed (!relu (ax+b `(10 10) 1 -5)))))))
