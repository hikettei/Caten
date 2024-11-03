(in-package :caten/test-suite)

(deftest double-reduction-single-kernel
  (flet ((op ()
           (caten (!add (call (Embedding 10 10) (make-tensor `(10 10))) (call (Embedding 10 10) (!cast (!add (iconst 'n) (!index-components `(1 10))) :float32))))))
    (if (= 1 (ctx:getenv :JIT))
        (progn
          (testing "w/ differentiate" (ok (= 3 (n-kernels (op)))))
          (testing "with-no-grad" (with-no-grad (ok (= 1 (n-kernels (op)))))))
        (skip "NEED JIT"))))
