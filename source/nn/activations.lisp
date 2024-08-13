(in-package :caten/nn)
;; 方針：最適化に手をつける前に動作例を色々記述しておく
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

(defmodel (ReLU () :where "A[~] -> A[~]") ())
(defmethod call ((op ReLU) &rest inputs)
  (!maximum (car inputs) (!const (car inputs) 0)))
(defun !relu (x) (call (ReLU) x))

(defmodel (LeakyReLU (&key (neg-slope 1e-3)) :where "A[~] -> A[~]") ((neg-slope neg-slope)))
(defmethod call ((op LeakyReLU) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((neg-slope neg-slope)) op
      (!sub (!relu x) (!relu (!mul x (!neg (fconst neg-slope :dtype (dtype-of x)))))))))
(defun !leaky-relu (x &key (neg-slope 1e-3)) (call (LeakyReLU :neg-slope neg-slope) x))

(defun _softmax (x &key (axis -1))
  (let* ((m (!sub x (!max x :axis axis :keepdims t)))
	 (e (!exp m)))
    (values m e (!sum e :axis axis :keepdims t))))

(in-package :caten/nn.test)

;; test-sigmoid
(deftest test-relu
  (ok (every #'(lambda (x) (>= x 0)) (elements (proceed (!relu (ax+b `(10 10) 1 -5)))))))
