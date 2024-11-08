(in-package :caten/nn)

(defmodel (Sigmoid () :where "A[~] -> A[~]") ((ret)))
(defmethod call ((op Sigmoid) &rest inputs)
  (let ((x (car inputs)))
    (setf (slot-value op 'ret) (!recip (!add (fconst 1 :dtype (dtype-of x)) (!exp2 (!mul x (fconst (/ -1 (log 2)) :dtype (dtype-of x)))))))
    (slot-value op 'ret)))
(defmethod backward ((op Sigmoid) &optional prev-dout)
  (let ((ret (slot-value op 'ret)))
    (!mul (!mul ret (!add (fconst 1 :dtype (dtype-of ret)) (!neg ret))) prev-dout)))
(defun !sigmoid (x) (forward (Sigmoid) x))

(defmodel (ReLU () :where "A[~] -> A[~]") ())
(defmethod call ((op ReLU) &rest inputs)
  (!maximum (car inputs) (!const (car inputs) 0)))
(defun !relu (x) (forward (ReLU) x))

(defmodel (LeakyReLU (&key (neg-slope 1e-3)) :where "A[~] -> A[~]") ((neg-slope neg-slope)))
(defmethod call ((op LeakyReLU) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((neg-slope neg-slope)) op
      (!sub (!relu x) (!relu (!mul x (!neg (fconst neg-slope :dtype (dtype-of x)))))))))
(defun !leaky-relu (x &key (neg-slope 1e-3)) (forward (LeakyReLU :neg-slope neg-slope) x))

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
(defun !log-softmax (x &key (axis -1)) (forward (LogSoftmax :axis axis) x))

(defmodel (ELU (&key (alpha 1.0)) :where "A[~] -> A[~]") ((alpha alpha)))
(defmethod call ((op ELU) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((alpha alpha)) op
      (!sub (!relu x) (!relu (!mul (!const x alpha) (!sub (!const x 1) (!exp x))))))))
(defun !elu (x &key (alpha 1.0)) (forward (ELU :alpha alpha) x))

(defmodel (ReLU6 () :where "A[~] -> A[~]") ())
(defmethod call ((op ReLU6) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (!sub (!relu x) (!relu (!sub x (!const x 6))))))
(defun !relu6 (x) (forward (ReLU6) x))

(defmodel (Softmax (&key (axis -1)) :where "A[~] -> A[~]") ((axis axis :accessor softmax-axis)))
(defmethod call ((op Softmax) &rest inputs)
  (multiple-value-bind (m e ss) (_softmax (car inputs) :axis (softmax-axis op))
    (declare (ignore m))
    (!div e ss)))
(defun !softmax (x &key (axis -1)) (forward (Softmax :axis axis) x))

(defmodel (Softplus (&key (beta 1.0)) :where "A[~] -> A[~]") ((beta beta)))
(defmethod call ((op Softplus) &rest inputs)
  (with-slots ((beta beta)) op
    (!mul (!const (car inputs) (/ 1 beta)) (!log (!add (!const (car inputs) 1) (!exp (!mul (car inputs) (!const (car inputs) beta))))))))
(defun !softplus (x &key (beta 1.0)) (forward (SoftPlus :beta beta) x))

(defmodel (Softsign () :where "A[~] -> A[~]") ())
(defmethod call ((op Softsign) &rest inputs)
  (let ((x (car inputs)))
    (!div x (!+ (!const x 1) (!abs x)))))
(defun !softsign (x) (forward (Softsign) x))

(defmodel (SoftShrink (&key (lmd 0.5)) :where "A[~] -> A[~]")
  ((lmd lmd)))
(defmethod call ((op SoftShrink) &rest inputs)
  (let* ((x (car inputs))
         (lmd-tensor (!const x (slot-value op 'lmd))))
    (declare (type tensor x))
    (!where (!> (!abs x) lmd-tensor)
            (!sub x (!mul (!signum x) lmd-tensor))
            (!const x 0))))
(defun !softshrink (x &key (lmd 0.5)) (forward (SoftShrink :lmd lmd) x))

(defmodel (CeLU (&key (alpha 1.0)) :where "A[~] -> A[~]") ((alpha alpha)))
(defmethod call ((op CeLU) &rest inputs)
  (let ((x (car inputs))
	(alpha (slot-value op 'alpha)))
    (declare (type tensor x))
    (!add (!maximum x (!const x 0)) (!minimum (!const x 0) (!mul (!const x alpha) (!sub (!exp (!/ x (!const x alpha))) (!const x 1)))))))
(defun !celu (x &key (alpha 1.0)) (forward (CeLU :alpha alpha) x))

(defmodel (SiLU () :where "A[~] -> A[~]") ())
(defmethod call ((op SiLU) &rest inputs &aux (x (car inputs))) (!mul x (!sigmoid x)))
(defun !silu (x) (forward (SiLU) x))

(defmodel (LogSigmoid () :where "A[~] -> A[~]") ())
(defmethod call ((op LogSigmoid) &rest inputs &aux (x (car inputs))) (!neg (!softplus (!neg x ))))
(defun !logsigmoid (x) (forward (LogSigmoid) x))

(defmodel (GeLU (&key (approx :tanh)) :where "A[~] -> A[~]") ((approx approx)))
(defmethod gelu/call ((op GeLU) (approx (eql :tanh)) x)
  (!* (!const x 0.5) x (!+ (!const x 1) (!tanh (!* (!const x (sqrt (/ 2.0 pi))) (!+ x (!* (!const x 0.044715) (!* x x x))))))))
(defmethod gelu/call ((op GeLU) (approx (eql :sigmoid)) x)
  (!mul x (!sigmoid (!* (!const x 1.702) x))))
(defmethod call ((op GeLU) &rest inputs) (gelu/call op (slot-value op 'approx) (car inputs)))
(defun !gelu (x &key (approx :tanh)) (forward (GeLU :approx approx) x))
;; TODO: SeLU
;; TODO: Mish (needs tanh)
(defmodel (HardSwish () :where "A[~] -> A[~]") ())
(defmethod call ((op HardSwish) &rest inputs &aux (x (car inputs)))
  (!* x (!relu6 (!add x (!const x 3))) (!const x (/ 1 6))))
(defun !hardswish (x) (forward (HardSwish) x))
;; TODO: Hard_Tanh
;; TODO: Softmin
(in-package :caten/nn.test)
;; TODO: Implement Assert Close, printing atol/rtol
(defun sigmoid-lisp (x) (/ (+ 1 (expt 2 (* x (/ -1 (log 2)))))))
(define-nn-test Sigmoid
  "Testing w/ Sigmoid([100, 100])"
  :compile (caten (!sigmoid (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'sigmoid-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun relu-lisp (x) (max x 0.0))
(define-nn-test ReLU
  "Testing w/ ReLU([100, 100])"
  :compile (caten (!relu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) 1 -300)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'relu-lisp x))))
  :assert-close ((x y) (every #'= x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun leaky-relu-lisp (x) (- (relu-lisp x) (relu-lisp (* x (- 1e-3)))))
(define-nn-test Leaky-ReLU
  "Testing w/ Leaky-ReLU([100, 100])"
  :compile (caten (!leaky-relu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'leaky-relu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(define-nn-test Softmax
  "Testing w/ Softmax([512, 256])"
  :compile (caten (!softmax (make-tensor `(512 256) :from 'x)))
  :inputs (ctx:with-contextvar (:jit 0 :avm :lisp)
	    (list (proceed (!rand `(512 256)))))
  :caten ((model x) (forward model `(x . ,x)))
  :lisp  ((model x) (proceed (!softmax x)))
  :assert-close ((x y)
		 (let ((sum (proceed (!contiguous (!sum x :axis -1)))))
		   (every #'(lambda (x) (<= (abs (- x 1.0)) 1e-1)) (elements sum)))
		 (every (~= 1e-6) (elements x) (elements y)))
  :in-place ((model) (and
		      (= 2 (n-args `(512 256) model))
		      (= 0 (n-args `(512 1) model))))
  :kernel   ((model) (= 1 (n-kernels model))))

(define-nn-test LogSoftmax
  "Testing w/ LogSoftmax([512, 256])"
  :compile (caten (!log-softmax (make-tensor `(512 256) :from 'x)))
  :inputs (ctx:with-contextvar (:jit 0 :avm :lisp)
	    (list (proceed (!rand `(512 256)))))
  :caten ((model x) (forward model `(x . ,x)))
  :lisp  ((model x) (proceed (!log-softmax x)))
  :assert-close ((x y)
		 (every (~= 1e-6) (elements x) (elements y)))
  :in-place ((model) (= 2 (n-args `(512 256) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun elu-lisp (x &aux (alpha 1.0)) (- (relu-lisp x) (relu-lisp (* alpha (- 1 (exp x))))))
(define-nn-test ELU
  "Testing w/ ELU([100, 100])"
  :compile (caten (!elu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'elu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun relu6-lisp (x) (- (relu-lisp x) (relu-lisp (- x 6))))
(define-nn-test ReLU6
  "Testing w/ ReLU6([100, 100])"
  :compile (caten (!relu6 (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'relu6-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun softplus-lisp (x &aux (beta 1.0))
  (* (/ 1 beta) (log (+ 1 (exp (* x beta))))))
(define-nn-test SoftPlus
  "Testing w/ SoftPlus([100, 100])"
  :compile (caten (!softplus (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'softplus-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun softsign-lisp (x) (/ x (+ 1 (abs x))))
(define-nn-test SoftSign
  "Testing w/ SoftSign([100, 100])"
  :compile (caten (!softsign (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.01 1.0)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'softsign-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun softshrink-lisp (x &aux (lmd 0.5))(cond ((> x lmd) (- x lmd))((< x (- lmd)) (+ x lmd))(t 0)))
(define-nn-test SoftShrink
  "Testing w/ SoftShrink([100, 100])"
  :compile (caten (!softshrink (make-tensor `(100 100) :from 'x)))
  :inputs (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'softshrink-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun celu-lisp (x &aux (alpha 1.0)) (+ (max x 0.0) (min 0 (* alpha (- (exp (/ x alpha)) 1)))))
(define-nn-test CeLU
  "Testing w/ CeLU([100, 100])"
  :compile (caten (!celu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 1)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'celu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun silu-lisp (x) (* x (sigmoid-lisp x)))
(define-nn-test SiLU
  "Testing w/ SiLU([100, 100])"
  :compile (caten (!silu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'silu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))


(defun logsigmoid-lisp (x)(log (/ 1 ( + 1 (exp (- x))))))
(define-nn-test LogSigmoid
  "Testing w/ LogSigmoid([100, 100])"
  :compile (caten (!logsigmoid (make-tensor `(100 100) :from 'x)))
  :inputs (list (proceed (ax+b `(100 100) 0.01 -10)))
  :caten   ((model x) (elements (print (forward model `(x . ,x)))))
  :lisp    ((model x) (elements (print (proceed (lazy-lisp #'logsigmoid-lisp x)))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun gelu-lisp (x) (* 0.5 x (+ 1 (tanh (* (sqrt (/ 2.0 (coerce pi (type-of x)))) (+ x (* 0.044715 (* x x x))))))))
(define-nn-test GeLU
  "Testing w/ GeLU([100, 100])"
  :compile (caten (!gelu (make-tensor `(100 100) :from 'x) :approx :tanh))
  :inputs  (list (proceed (ax+b `(100 100) 0.0001 -0.2)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'gelu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun hardswish-lisp (x) (* x (relu6-lisp (+ x 3.0)) (/ 1 6)))
(define-nn-test HardSwish
  "Testing w/ HardSwish([100, 100])"
  :compile (caten (!hardswish (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) 0.0001 -0.2)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'hardswish-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))
