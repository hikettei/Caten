(in-package :caten/test-suite)

;; A place for testing all the functions, activations for all dtypes
(defun exp2 (x) (expt 2 x))
(defun log2 (x) (log x 2))

(macrolet ((unary-dtype-test (name op lisp-op &key (non-zero nil) (ulp) (max) (fuzz t))
	     `(deftest ,name
		(dolist (dtype `(:float32 :float64))
		  (let ((model (caten (,op (make-tensor `(1) :initial-element 'a :dtype dtype))))
			(ulp (or ,ulp (1.0ulp dtype))))
		    (forall (x dtype :fuzzing nil)
		      (when (if ,non-zero (> x 0.0) t)
			(when (or (null ,max) (<= (abs x) ,max))
			  (assert (<= (abs (- (,lisp-op x) (aref (elements (forward model `(a . ,x))) 0))) ulp)
				  ()
				  "~(~a~)(x=~a)=~a is wrong, expecting ~a. ULP=~a, Dtype=~a"
				  ',lisp-op x (aref (elements (forward model `(a . ,x))) 0)
				  (,lisp-op x) ulp dtype))))
		    (forall (x dtype :fuzzing ,fuzz)
		      (when (if ,non-zero (> x 0.0) t)
			(when (or (null ,max) (<= (abs x) ,max))
			  (assert (<= (abs (- (,lisp-op x) (aref (elements (forward model `(a . ,x))) 0))) ulp)
				  ()
				  "~(~a~)({x+(random 2.0)}=~a)=~a is wrong, expecting ~a. ULP=~a, Dtype=~a"
				  ',lisp-op x (aref (elements (forward model `(a . ,x))) 0)
				  (,lisp-op x) ulp dtype))))
		    (ok t))))))
  ;; Trig
  (unary-dtype-test sin-test !sin sin)
  (unary-dtype-test cos-test !cos cos :ulp 1e-3 :max 121255)
  (unary-dtype-test tan-test !tan tan :ulp 1e-1 :max 20 :fuzz nil)
  ;; Hyperbolic
  (unary-dtype-test sinh-test !sinh sinh :ulp 1e-1 :max 10)
  (unary-dtype-test cosh-test !cosh cosh :ulp 1e-1 :max 10)
  (unary-dtype-test tanh-test !tanh tanh :ulp 1e-1 :max 10)
  ;; [TODO] asin acos atan asinh acosh atanh
  (unary-dtype-test exp-test !exp exp :ulp 1e-3 :max 7)
  (unary-dtype-test log-test !log log :non-zero t :ulp 1e-4)
  
  (unary-dtype-test exp2-test !exp2 exp2 :ulp 1e-3 :max 7)
  (unary-dtype-test log2-test !log2 log2 :non-zero t :ulp 1e-4)
  
  (unary-dtype-test abs-test !abs abs)
  (unary-dtype-test signum-test !signum signum)
  (unary-dtype-test sqrt-test !sqrt sqrt :ulp 1e-6 :non-zero t :max 1e6)
  (unary-dtype-test recip-test !recip / :ulp 1e-6 :non-zero t :max 1e6)
  
  (unary-dtype-test truncate-test !truncate truncate :ulp 1e-6 :max 1e5)
  (unary-dtype-test floor-test !floor floor :ulp 1e-6 :max 1e5)
  (unary-dtype-test ceiling-test !ceiling ceiling :ulp 1e-6 :max 1e5))
;; [TODO] Binary Ops Test
;; [TODO] Reduce Ops Test
(deftest test-assign
  (let* ((x (linspace `(10 10) 0 0))
         (y (!assign x (fconst 1.0))))
    (proceed y)
    (ok (every #'(lambda (elm) (= elm 1.0)) (elements x)))))
#|
(deftest test-expt
  (testing "Power is a scalar and fixnum (-2.5 < n < 2.5), only for VM."
    (when (= 0 (ctx:getenv :JIT))
      (let ((failed nil))
        (loop for n upfrom 0 below 5.0 by 0.1
              for power = (+ n -2.5)
              for x = (rand `(50 50))
              for answer = (proceed (!expt x power))
              for expected = (map 'list #'(lambda (x) (expt x power)) (change-facet x :simple-array))
              do (unless (every #'(lambda (x y) (<= (abs (- x y)) 1e-5)) (change-facet answer :simple-array) expected)
                   (push (cons (apply #'max (map 'list #'- (change-facet answer :simple-array) expected)) power) failed)))
        (ok (null failed)
            (when failed
              (with-output-to-string (out)
                (loop for (diff . n) in failed do (format out "n=~a, max_atol=~a~%" n diff))))))))
  (testing "Expt(X, N) where N is a dynamic shape."
    
    )
  (testing "Power is a tensor"

    ))
|#
