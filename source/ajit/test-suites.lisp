(in-package :cl-user)

(defpackage :caten/ajit.test
  (:use :cl :rove :caten :caten/nn :caten/air :caten/avm :trivia :caten/ajit))

(in-package :caten/ajit.test)

(defun equal-to (a) #'(lambda (x) (= x a)))
(defun pproceed (params tensor)
  (let ((mdl (caten tensor)))
    (apply #'forward mdl params)))
(defun elements (tensor) (buffer-value (tensor-buffer tensor)))
(defun get-jit-info (avm)
  (declare (type avm avm))
  (let ((jit-info (car (last (graph-nodes (avm-graph avm))))))
    (assert (eql (node-type jit-info) :JIT_KERNEL) () "the kernel didn't returned a jit_kernel")
    jit-info))
(defun n-kernels (avm)
  (declare (type avm avm))
  (jit-info-n-kernels (getattr (get-jit-info avm) :jit-info)))
(defun n-args (avm)
  (declare (type avm avm))
  (length (node-reads (get-jit-info avm))))
(defun check-kernels (n avm)
  (if (= 1 (ctx:getenv :JIT))
      (ok (= n (n-kernels avm)))
      (skip "Needs JIT")))
(defun check-args (n avm)
  (if (= 1 (ctx:getenv :JIT))
      (ok (= n (n-args avm)))
      (skip "Needs JIT")))

(deftest check-kernel-counts
  (with-no-grad
    (check-kernels 1 (caten (!matmul (make-tensor `(10 20)) (make-tensor `(20 10)))))
    (check-kernels 1 (caten (caten/nn:!softmax (ax+b `(10 10) 1 1))))
    (check-kernels 1 (caten (!cos (!sin (!padding (make-tensor `(10 10) :initial-element 2.0) `((2 2) (2 2)) :value 0.0)))))
    (check-kernels 1 (caten (!sin (!matmul (make-tensor `(10 20)) (make-tensor `(20 30))))))
    (check-kernels 2 (caten (!matmul (make-tensor `(128 32)) (!matmul (make-tensor `(32 64)) (make-tensor `(64 128))))))
    (check-kernels 1 (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb)))))
    (check-kernels 1 (caten (!tan (make-tensor `(10 10)))))
    (check-kernels 2 (caten (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25)))))
    (check-kernels 1 (caten (!mean (make-tensor `(a b c)))))))

(deftest check-in-place-mutation
  (with-no-grad
    (check-args 2 (caten (!tan (make-tensor `(3 3)))))
    (check-args 4 (caten (!tan (!tan (!tan (make-tensor `(3 3)))))))
    ;; [TODO] Fuse <= 1 args
    (check-args 4 (caten (!softmax (make-tensor `(3 3)))))
        
    ))

(deftest matmul-is-small
  (with-no-grad
    (unless (= 1 (ctx:getenv :JIT))
      (skip "Needs JIT"))
    (let* ((m (caten (!matmul (make-tensor `(3 10)) (make-tensor `(10 20)))))
	   (allocs (loop for node in (graph-nodes (avm-graph m))
			 if (eql (node-type node) :Allocate) collect node)))
      (ok (= (length allocs) 3) "gemm(a, b, c)")
      (ok (every #'(lambda (x) (if (= (getattr x :nrank) 2)
				   t
				   (if (= (getattr x :nrank) 3)
				       (some #'(lambda (x) (= x 1)) (subseq (node-reads x) 0 3))
				       nil)))
		 allocs)
	  "Contiguous array creations are not allowed"))))

(deftest symbolic-function-args-test
  (with-no-grad
    (when (= (ctx:getenv :JIT) 1)
      (let ((args (node-reads (get-jit-info (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb))))))))
	(ok (every #'(lambda (x) (find x args)) `(N FROMB TOB BYB TOA FROMA BYA)))
	(ok (= (length args) 9))))))
      
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

;; TODO: Tensor-Shaped-Tesnor Operation
;; TODO: Tensor-Shaped-Tensor Iteration Rendering (The scalar result should be passed via arguments)
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
