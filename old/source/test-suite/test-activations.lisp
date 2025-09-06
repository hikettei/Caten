(in-package :caten/test-suite)
;; TODO: Implement Assert Close, printing atol/rtol
;; ~~ Custom Kernel for calling element-wise lisp kernel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defnode (:Testing :Test/Lisp-Lazy-Apply) () "" :slots ((f)))
(defclass Custom/LazyApply (Func) ((f :initarg :f :accessor lazyapply-f))
  (:documentation "This custom op is dedicated to testing, only supported in Lisp VM"))
(defmethod forward ((op Func) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op Func) &optional dout) (declare (ignore dout)) nil)
(defmethod lower ((op Func) &rest nodes)
  (with-context (_ (emit (make-node :Testing :Test/Lisp-Lazy-Apply (list (gensym)) (map 'list #'node->id nodes) :f (lazyapply-f op))))))
(defmethod realize-node ((node-id (eql :Test/Lisp-Lazy-Apply)) runtime node args) (apply #'caten/runtime/runtime::map-view runtime nil (getattr node :f) args))
(defun lazy-lisp (f tensor)
  (declare (type function f) (type tensor tensor))
  (forward (make-instance 'Custom/LazyApply :f f) tensor))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

(defun hardsigmoid-lisp (x alpha beta) (max 0 (min 1 (+ (* alpha x) beta))))
(define-nn-test HardSigmoid
  "Testing w/ HardSigmoid([100, 100])"
  :compile (caten (!hard-sigmoid (make-tensor `(100 100) :from 'x) :alpha 0.2 :beta 0.5))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'relu-lisp x))))
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
  :inputs (list (proceed (!rand `(512 256))))
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
  :inputs (list (proceed (!rand `(512 256))))
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
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'logsigmoid-lisp x))))
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

(defun selu-lisp (x &aux (lambda 1.0507) (alpha 1.67326))(* lambda (if (>= x 0) x (* alpha (- (exp x) 1)))))
(define-nn-test SeLU
  "Testing w/ SeLU([100, 100])"
  :compile (caten (!selu (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 -10)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'selu-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(defun mish-lisp (x) (* x (tanh (log (+ 1 (exp x))))))
(define-nn-test Mish
  "Testing w/ Mish([100, 100])"
  :compile (caten (!mish (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) -0.001 7)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'mish-lisp x))))
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

(defun hardtanh-lisp (x &aux (min_val -1.0) (max_val 1.0)) (cond ((> x max_val) max_val) ((< x min_val) min_val) (t x)))
(define-nn-test HardTanh
  "Testing w/ HardTanh([100, 100])"
  :compile (caten (!hardtanh (make-tensor `(100 100) :from 'x)))
  :inputs  (list (proceed (ax+b `(100 100) 0.0001 -0.2)))
  :caten   ((model x) (elements (forward model `(x . ,x))))
  :lisp    ((model x) (elements (proceed (lazy-lisp #'hardtanh-lisp x))))
  :assert-close ((x y) (every (~= 1e-6) x y))
  :in-place ((model) (= 2 (n-args `(100 100) model)))
  :kernel   ((model) (= 1 (n-kernels model))))

(define-nn-test Softmin
  "Testing w/ Softmin([512, 256])"
  :compile (caten (!softmin (make-tensor `(512 256) :from 'x)))
  :inputs (list (proceed (!rand `(512 256))))
  :caten ((model x) (forward model `(x . ,x)))
  :lisp  ((model x) (proceed (!softmin x)))
  :assert-close ((x y)
                 (let ((sum (proceed (!contiguous (!sum x :axis -1)))))
                   (every #'(lambda (x) (<= (abs (- x 1.0)) 1e-1)) (elements sum)))
                 (every (~= 1e-6) (elements x) (elements y)))
  :in-place ((model) (and
                      (= 2 (n-args `(512 256) model))
                      (= 0 (n-args `(512 1) model))))
  :kernel   ((model) (= 1 (n-kernels model))))
