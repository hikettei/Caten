(in-package :caten/test-suite)

(deftest test-avg-pooling
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let ((input (rand `(10 3 25 25))))
        (assert-equal
            (:atol 1e-5 :rtol 1e-5)
            (with-torch (input)
              (->caten (f:avg_pool2d input `(2 2))))
            (proceed (!AvgPool2D input :kernel-size `(2 2))))))))

(deftest test-max-pooling
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let ((input (rand `(10 3 25 25))))
        (assert-equal
            (:atol 1e-5 :rtol 1e-5)
            (with-torch (input)
              (->caten (f:max_pool2d input `(2 2))))
            (proceed (!MaxPool2D input :kernel-size `(2 2))))))))

;; [TODO] Pooling in a single kernel
