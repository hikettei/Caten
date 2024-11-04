(in-package :caten/test-suite)

(defmacro with-protect-jit (&body body)
  `(if (= 1 (ctx:getenv :JIT))
       (progn ,@body)
       (skip "NEED JIT")))

(defmacro define-kernel-count-test (name n-excepted body)
  `(deftest ,name
     (flet ((op () ,body))
       (with-protect-jit
         (ok (= ,n-excepted (n-kernels (op))))))))

(deftest double-reduction-single-kernel
  (flet ((op ()
           (caten (!add (call (Embedding 10 10) (make-tensor `(10 10))) (call (Embedding 10 10) (!cast (!add (iconst 'n) (!index-components `(1 10))) :float32))))))
    (with-protect-jit
      (testing "w/ differentiate" (ok (= 3 (n-kernels (op)))))
      (testing "with-no-grad" (with-no-grad (ok (= 1 (n-kernels (op)))))))))

(define-kernel-count-test tril-triu-matmul 1 (caten (!matmul (!tril (make-tensor `(10 1 1 10))) (!triu (make-tensor `(10 1))))))
(define-kernel-count-test serialize-argmax-single-kernel 1 (caten (!argmax (!matmul (make-tensor `(10 10)) (make-tensor `(10 10))))))
(define-kernel-count-test serialize-double-reduction-softmax 1 (caten (!add (!softmax (make-tensor `(3 3))) (!softmax (make-tensor `(3 3))))))
(define-kernel-count-test sum-after-double-matmul 3 (caten (!sum (!matmul (make-tensor `(10 10)) (!matmul (make-tensor `(10 10)) (make-tensor `(10 10)))))))
(define-kernel-count-test activation-after-embedding 1 (with-no-grad (caten (!gelu (call (Embedding 10 10) (make-tensor `(b c)))))))
