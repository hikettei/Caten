(in-package :caten/test-suite)

(deftest test-convnd-kernel
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (linspace `(10 3 25 25) 0 0.1))
              (weight (rand `(6 3 10 10))))
          (assert-equal
              (:atol 1e-5 :rtol 1e-5)
              (with-torch (input weight)
                (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1)))
              (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1)))))))
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (linspace `(10 3 25 25) 0 0.1))
              (weight (rand `(6 3 10 10)))
              (bias (rand `(6))))
          (assert-equal
              (:atol 1e-5 :rtol 1e-5)
              (with-torch (input weight bias)
                (->caten (f:conv2d input weight bias :stride 1 :padding 0 :dilation 1 :groups 1)))
              (proceed (!convnd input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1))))))))

(deftest test-convnd
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (linspace `(10 3 25 25) 0.001 0.0))
              (weight (rand `(6 3 10 10))))
          (assert-equal
              (:atol 1e-5 :rtol 1e-5)
              (with-torch (input weight)
                (print (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1))))
              (print (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

;; [TODO] Conv+GELU Conv+ReLU
;; Conv Schedule Test
  
