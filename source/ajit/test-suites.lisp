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
    (ok (every (equal-to 4) out))))

