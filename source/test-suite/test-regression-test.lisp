(in-package :caten/test-suite)
;; Add more and more failing scheduling case

(python-exec
 "def test_linear(x, weight, bias):
  return (x @ weight.T + bias)")
(import-function "test_linear")

(deftest test-linear-failing-case
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let ((x (rand `(1 3 8)))
            (attn-weight (rand `(24 8)))
            (attn-bias (rand `(24))))
        (assert-equal
            (:atol 1e-5 :rtol 1e-5)
            (with-torch (x attn-weight attn-bias)
              (->caten (test_linear x attn-weight attn-bias)))
            (proceed (!add (!matmul x (!t attn-weight)) attn-bias)))))))
