(in-package :caten/test-suite)
;; Broadcast Shape Inference is invaild?
(deftest test-convnd-1
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (if (= 1 (ctx:getenv :JIT))
            (skip "Failing with JIT=1...")
            (let ((input (rand `(1 3 12 12)))
                  (weight (rand `(6 3 4 4))))
              (assert-equal
                  (:atol 1e-5 :rtol 1e-2)
                  (with-torch (input weight)
                    (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1)))
                  (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

(deftest test-convnd-2
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (if (= 1 (ctx:getenv :JIT))
            (skip "Failing with JIT=1...")
            (let ((input (rand `(1 3 25 25)))
                  (weight (rand `(6 3 10 10)))
                  (bias (rand `(6))))
              (assert-equal
                  (:atol 1e-5 :rtol 1e-2)
                  (with-torch (input weight bias)
                    (->caten (f:conv2d input weight bias :stride 1 :padding 0 :dilation 1 :groups 1)))
                  (proceed (!convnd input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

(deftest test-convnd-3
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (if (= 1 (ctx:getenv :JIT))
            (skip "Failing with JIT=1...")
            (let ((input (linspace`(10 3 4 4) 0.001 0.0))
                  (weight (linspace `(6 3 4 4) 0.001 0.0)))
              (assert-equal
                  (:atol 1e-5 :rtol 1e-2)
                  (with-torch (input weight)
                    (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1)))
                  (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

(deftest test-convnd-4
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (if (= 1 (ctx:getenv :JIT))
            (skip "Failing with JIT=1...")
            (let ((input (linspace`(2 3 8 8) 0.001 0.0))
                  (weight (linspace `(2 3 4 4) 0.001 0.0)))
              (assert-equal
                  (:atol 1e-5 :rtol 1e-2)
                  (with-torch (input weight)
                    (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1)))
                  (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

;; [TODO] Conv+GELU Conv+ReLU
;; Conv Schedule Test
