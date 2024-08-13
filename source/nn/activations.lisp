(in-package :caten/nn)

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

(defmodel (LogSoftmax (&key (axis -1)) :where "A[~] -> A[~]") ((axis axis)))
(defmethod call ((op LogSoftmax) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((axis axis)) op
      (multiple-value-bind (m e ss) (_softmax x :axis axis)
	(declare (ignore e))
	(!sub m (!log ss))))))
(defun !log-softmax (x &key (axis -1)) (call (LogSoftmax :axis axis) x))

(defmodel (ELU (&key (alpha 1.0)) :where "A[~] -> A[~]") ((alpha alpha)))
(defmethod call ((op ELU) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((alpha alpha)) op
      (!sub (!relu x) (!relu (!mul (!const x alpha) (!sub (!const x 1) (!exp x))))))))
(defun !elu (x &key (alpha 1.0)) (call (ELU :alpha alpha) x))

(defmodel (ReLU6 () :where "A[~] -> A[~]") ())
(defmethod call ((op ReLU6) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (!sub (!relu x) (!relu (!sub x (!const x 6))))))
(defun !relu6 (x) (call (ReLU6) x))

(defmodel (Softmax (&key (axis -1)) :where "A[~] -> A[~]") ((axis axis :accessor softmax-axis)))
(defmethod call ((op Softmax) &rest inputs)
  (multiple-value-bind (m e ss) (_softmax (car inputs) :axis (softmax-axis op))
    (declare (ignore m))
    (!div e ss)))
(defun !softmax (x &key (axis -1)) (call (Softmax :axis axis) x))

(defmodel (Softplus (&key (beta 1.0)) :where "A[~] -> A[~]") ((beta beta)))
(defmethod call ((op Softplus) &rest inputs)
  (with-slots ((beta beta)) op
    (!mul (!const (car inputs) (/ 1 beta)) (!log (!add (!const (car inputs) 1) (!exp (!mul (car inputs) (!const (car inputs) beta))))))))
(defun !softplus (x &key (beta 1.0)) (call (SoftPlus :beta beta) x))

(defmodel (Softsign () :where "A[~] -> A[~]") ())
(defmethod call ((op Softsign) &rest inputs)
  (let ((x (car inputs)))
    (!div x (!+ (!const x 1) (!abs x)))))
(defun !softsign (x) (call (Softsign) x))
;; TODO: SoftShrink
(defmodel (CeLU (&key (alpha 1.0)) :where "A[~] -> A[~]") ((alpha alpha)))
(defmethod call ((op CeLU) &rest inputs)
  (let ((x (car inputs))
	(alpha (slot-value op 'alpha)))
    (declare (type tensor x))
    (!add (!maximum x (!const x 0)) (!minimum (!const x 0) (!mul (!const x alpha) (!sub (!exp (!/ x (!const x alpha))) (!const x 1)))))))
(defun !celu (x &key (alpha 1.0)) (call (CeLU :alpha alpha) x))

(defmodel (SiLU () :where "A[~] -> A[~]") ())
(defmethod call ((op SiLU) &rest inputs &aux (x (car inputs))) (!mul x (!sigmoid x)))
(defun !silu (x) (call (SiLU) x))
;; TODO: LogSigmoid
(defmodel (GeLU (&key (approx :tanh)) :where "A[~] -> A[~]") ((approx approx)))
(defmethod gelu/call ((op GeLU) (approx (eql :tanh)) x)
  (!* (!const x 0.5) x (!+ (!const x 1) (!tanh (!* (!const x (sqrt (/ 2.0 pi))) (!+ x (!* (!const x 0.044715) (!* x x x))))))))
(defmethod gelu/call ((op GeLU) (approx (eql :sigmoid)) x)
  (!mul x (!sigmoid (!* (!const x 1.702) x))))
(defmethod call ((op GeLU) &rest inputs) (gelu/call op (slot-value op 'approx) (car inputs)))
(defun !gelu (x &key (approx :tanh)) (call (GeLU :approx approx) x))
;; TODO: SeLU
;; TODO: Mish (needs tanh)
(defmodel (HardSwish () :where "A[~] -> A[~]") ())
(defmethod call ((op HardSwish) &rest inputs &aux (x (car inputs)))
  (!* x (!relu6 (!add x (!const x 3))) (!const x (/ 1 6))))
(defun !hardswish (x) (call (HardSwish) x))
;; TODO: Hard_Tanh
;; TODO: Softmin
(in-package :caten/nn.test)

;; TODO: TestOps
(deftest test-relu
  (ok (every #'(lambda (x) (>= x 0)) (elements (proceed (!relu (ax+b `(10 10) 1 -5)))))))
