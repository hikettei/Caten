(in-package :cl-user)

(defpackage :caten/ajit.test
  (:use :cl :rove :caten :caten/nn :caten/air :caten/avm :trivia :caten/ajit))

(in-package :caten/ajit.test)

(defun equal-to (a) #'(lambda (x) (= x a)))
(defun pproceed (params tensor)
  (let ((mdl (caten tensor)))
    (apply #'forward mdl params)))
(defun elements (tensor) (buffer-value (tensor-buffer tensor)))

;; TODO: Render To List?
;; Backward作るのとどっちが先か？

;; TODO: Pooling2D, Conv2D, Gemm, Composed Gemm (count the number of kernels)
;; TODO: Tensor-Shaped-Tesnor Operation
;; TODO: Tensor-Shaped-Tensor Iteration Rendering (The scalar result should be passed via arguments)
;; TODO: Mixed use of dynamic shape and scalar values (e.g.: Adding Float[n, 10] += n)
;; (jit (caten (!add (make-tensor `(a 10)) (!cast (fconst 'a) :float32))) :debug 4)

;; TestCase1. (caten (!mean (make-tensor `(a b c)) :axis t))
;; TestCase2. (caten (!tan (make-tensor `(10 10))))
;; TestCase3. (let ((*external-simplifiers* nil)) (let ((a (pproceed `((a . 2)) (make-tensor `(a 10) :initial-element 'a :dtype :uint32)))) (ok (and (every (equal-to 2) (elements a)) (= (length (elements a)) 20)))))
;; TestCase4. (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb))))

(deftest tensor-shaped-tensor-test
  (let* ((size1 (!add (iconst 'a) (iconst 'a)))
	 (size2 (!add (iconst 'b) (iconst 'b)))
	 (tensor (caten (make-tensor `(,size1 ,size2) :initial-element 'a)))
	 (out (elements (forward tensor `(a . 4) `(b . 8)))))
    (ok (= (length out) 128))
    (ok (every (equal-to 4) out)))
  (let* ((size1 (!add (iconst 'a) (iconst 'a)))
	 (size2 (!add (iconst 'b) (iconst 'b)))
	 (tensor (caten (!sin (make-tensor `(,size1 ,size2) :initial-element 'a))))
	 (out (elements (forward tensor `(a . 4) `(b . 8)))))
    (ok (= (length out) 128))
    (ok (every (equal-to (sin 4)) out))))

;; Softmax ... reductionの依存関係の記述の問題 or Bufferの一次領域の問題
;; TODO: Compilerのレベルで作業したくない， CopyNodeとIn-Place Mutationを実装する
;; Symbolic動かすには？？？
;; DIVが一つのKernelにFuseされないと困るのでは・・・
;; Softmax: needs to be fused into a single kernel.
;; Softmax/Upfromが動かない理由は同じ(In-placeの区別がない)
;; In-place-test

;; Here's TODO List
;; - 1. 最初のSchedulingアルゴリズムを見直す: (recip(x)はmulと同じようにScheduleされるべき)
;; - 2. ノードを跨いで依存がある時は"Compilerが"一次領域を作成する
;; - 3.

#+(or)(progn
(caten (!sin (!matmul (make-tensor `(10 20)) (make-tensor `(20 30)))))
(caten (!matmul (make-tensor `(10 20)) (make-tensor `(20 30))))
(caten (!softmax (make-tensor `(10 10) :initial-element 1.0)))
(caten (!cos (!sin (!padding (make-tensor `(10 10) :initial-element 2.0) `((2 2) (2 2)) :value 0.0))))
(caten (!matmul (make-tensor `(128 32)) (!matmul (make-tensor `(32 64)) (make-tensor `(64 128)))))
(caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb))))
(caten (!mean (make-tensor `(a b c))))
(caten (!tan (make-tensor `(10 10))))
(let ((*external-simplifiers* nil)) (let ((a (pproceed `((a . 2)) (make-tensor `(a 10) :initial-element 'a :dtype :uint32)))) (ok (and (every (equal-to 2) (elements a)) (= (length (elements a)) 20))))))

(deftest in-place-test
  (let* ((a (make-tensor `(10 10) :initial-element 1.0))
	 (b (!exp a))
	 (c (!div a b)))
    ;; 同一のKernelにScheduleされる Or Copyを作成する必要がある
    (caten c))

  ;; Softmaxは同一のKernelにScheduleされるべき
  (!softmax (make-tensor `(3 3)))
  
  )
;; tensor-viewded-tensor-testはどうしようか
(deftest tensor-viewed-tensor-test
  (testing "Upfrom"
    (let* ((v1 (!add (iconst 'a) (iconst 'a)))
	   (v2 (!add (iconst 'b) (iconst 'b)))
	   (c (make-tensor `(10 10) :initial-element 1.0))
	   (out (!contiguous (!view c `(,v1 10) `(,v2 10))))
	   (model (caten out))
	   (result (forward model `(a . 1) `(b . 2))))
      (ok (equal `(8 6) (buffer-shape (tensor-buffer result))))
      (ok (= 48 (length (elements result))))
      (ok (every (equal-to 1.0) (elements result)))))
  (testing "Below"

    )
  (testing "By"

    )
  (testing "Broadcast"

    ))
