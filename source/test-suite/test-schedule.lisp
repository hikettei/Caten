(in-package :caten/test-suite)

(defmacro with-protect-jit (&body body)
  "Ensures the body is only executed under JIT=1"
  `(if (= 1 (ctx:getenv :JIT))
       (progn ,@body)
       (skip "NEED JIT")))

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
      (testing "Embedding + Embedding is a single kernel" (ok (= 3 (n-kernels (op)))))
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

(define-kernel-count-test tril-triu-matmul 1
  "!matmul+Tril+Triu is a single kernel"
  (caten (!matmul (!tril (make-tensor `(10 1 1 10))) (!triu (make-tensor `(10 1))))))

(define-kernel-count-test serialize-argmax-single-kernel 1
  "!argmax + !matmul can be fused into a single kernel"
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
