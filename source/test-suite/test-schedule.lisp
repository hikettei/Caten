(in-package :caten/test-suite)

(defmacro with-protect-jit (&body body)
  "Ensures the body is only executed under JIT=1"
  `(if (= 1 (ctx:getenv :JIT))
       (progn ,@body)
       (skip "NEED JIT")))

(deftest matmul-schedule-test
  (with-no-grad
    (with-protect-jit
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
  (testing "Static Matmul"
    (flet ((f () (caten (!matmul (make-tensor `(10 20)) (!matmul (make-tensor `(20 30)) (make-tensor `(30 40)))))))
      (with-no-grad
        (with-protect-jit
          (check-kernels 2 (f))
          (check-args 5 :tensor (f))))))
  (testing "Symbolic Matmul"
    (flet ((f () (caten (!matmul (make-tensor `(a b)) (!matmul (make-tensor `(b c)) (make-tensor `(c d)))))))
      (with-no-grad
        (with-protect-jit
          (check-kernels 2 (f))
          (check-args 5 :tensor (f)))))))

(deftest embedding-schedule-test
  (testing "Embedding < 1 Kernels, < 3 Tensors."
    (with-no-grad
      (with-protect-jit
        (check-kernels 1 (caten (call (Embedding 100 100) (make-tensor `(100 100)))))
        (check-args 3 t (caten (call (Embedding 100 100) (make-tensor `(100 100)))))
        (check-kernels 1 (caten (call (Embedding 100 100) (make-tensor `(batch_size sentence_len)))))
        (check-args 3 :tensor (caten (call (Embedding 100 100) (make-tensor `(batch_size sentence_len)))))))))

(defmacro define-kernel-count-test (name n-excepted description body)
  `(deftest ,name
     (flet ((op () ,body))
       (with-protect-jit
         (testing ,description
           (ok (= ,n-excepted (n-kernels (op)))))))))

(deftest double-reduction-single-kernel
  (flet ((op ()
           (caten (!add (call (Embedding 10 10) (make-tensor `(10 10))) (call (Embedding 10 10) (!cast (!add (iconst 'n) (!index-components `(1 10))) :float32))))))
    (with-protect-jit
      (testing "Embedding + Embedding is a single kernel" (ok (= 2 (n-kernels (op)))))
      (testing "Embedding + Embedding is a single kernel (no-grad)" (with-no-grad (ok (= 1 (n-kernels (op)))))))))

(define-kernel-count-test schedule-matmul 1
  "Matmul = Single Kernel"
  (caten (!matmul (make-tensor `(10 20)) (make-tensor `(20 30)))))

(define-kernel-count-test softmax-fusion 1
  "Softmax(Softmax(Softmax(x))) is a single kernel"
  (caten (!softmax (!softmax (!softmax (make-tensor `(3 3)))))))

(define-kernel-count-test padding-fusion 3
  "Sin(Padding) Fusion (TODO: Fuse them in a single kernel!)"
  (caten (!cos (!sin (!padding (make-tensor `(10 10) :initial-element 2.0) `((2 2) (2 2)) :value 0.0)))))

(define-kernel-count-test elwise-after-reduction 1
  "Activation(Matmul(X, Y)) is a single kernel"
  (caten (!gelu (!matmul (make-tensor `(10 10)) (make-tensor `(10 10))))))

(define-kernel-count-test fuse-squared-matmul 2
  "Matmul(Matmul(X, Y), Z) should separate into two kernels"
  (caten (!matmul (!matmul (make-tensor `(10 10)) (make-tensor `(10 10))) (make-tensor `(10 10)))))

(define-kernel-count-test embedding-single-kernel 1
  "Embedding is a single kernel"
  (with-no-grad (caten (call (Embedding 10 10) (make-tensor `(b c))))))

(define-kernel-count-test conv-schedule 4
  "ConvND = 4 Kernels (TODO: 1 Kernels)"
  (with-no-grad (caten (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25))))))

(define-kernel-count-test conv-relu-schedule 4
  "ConvND+ReLU = 4 Kernels (TODO: 1 Kernels)"
  (with-no-grad (caten (!relu (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25)))))))

(define-kernel-count-test conv-gelu-schedule 4
  "ConvND+GeLU = 4 Kernels (TODO: 1 Kernels)"
  (with-no-grad (caten (!gelu (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25)))))))

(define-kernel-count-test tril-triu-matmul 4
  "!matmul+Tril+Triu is a single kernel"
  (caten (!matmul (!tril (make-tensor `(10 1 1 10))) (!triu (make-tensor `(10 1))))))

(define-kernel-count-test serialize-argmax-single-kernel 2
  "!argmax + !matmul should be separated"
  (caten (!argmax (!matmul (make-tensor `(10 10)) (make-tensor `(10 10))))))

(define-kernel-count-test serialize-double-reduction-softmax 1
  "Scheduler should be able to serialize double sofmtax"
  (caten (!add (!softmax (make-tensor `(3 3))) (!softmax (make-tensor `(3 3))))))

(define-kernel-count-test sum-after-double-matmul 3
  "Scheduler should be able to separate sum after matmul"
  (caten (!sum (!matmul (make-tensor `(10 10)) (!matmul (make-tensor `(10 10)) (make-tensor `(10 10)))))))

(define-kernel-count-test activation-after-embedding 1
  "Embedding+GeLU should be fused"
  (with-no-grad (caten (!gelu (call (Embedding 10 10) (make-tensor `(b c)))))))

(define-kernel-count-test symbolic-gemm-fused 2
  "Symbolic Composed Matmul"
  (caten (!matmul (make-tensor `(a b)) (!matmul (make-tensor `(b c)) (make-tensor `(c d))))))

(define-kernel-count-test scaled-dot-product-attention 3
  "ScaledDotProductAttention is 3 kernels"
  (caten (scaled-dot-product-attention (rand `(4 8 8)) (rand `(4 8 8)) (rand `(4 8 8)))))

(define-kernel-count-test threefry-single-kernel 1
  "threefry2x32 in a single kernel"
  (caten (!rand `(3 3))))

(define-kernel-count-test batch-norm-single-kernel 1
  "BatchNorm = 1 Kernel"
  (caten (!batch-norm (make-tensor `(3 3)))))

(define-kernel-count-test layer-norm-single-kernel 1
  "LayerNorm = 1 Kernel"
  (caten (!layer-norm (make-tensor `(3 3)) `(3))))

(define-kernel-count-test rms-norm-single-kernel 1
  "RMSNorm = 1 Kernel"
  (caten (!rms-norm (make-tensor `(3 3)) `(3))))
