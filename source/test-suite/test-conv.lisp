(in-package :caten/test-suite)
;; In JIT: batch_size=1 is not working ...
(deftest test-convnd-1
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(1 3 12 12)))
              (weight (rand `(6 3 4 4))))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight)
                (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1)))
              (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1))))))))

(deftest test-convnd-2
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(1 3 25 25)))
              (weight (rand `(6 3 10 10)))
              (bias (rand `(6))))
          (assert-equal
              (:atol 1e-3 :rtol 1e-4)
              (with-torch (input weight bias)
                (->caten (f:conv2d input weight bias :stride 1 :padding 0 :dilation 1 :groups 1)))
              (proceed (!convnd input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1))))))))

(deftest test-convnd-3
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (if (caten/codegen/backend:jit-mode-p)
            (skip "[TODO: Fix] InputShape==KernelSize is failing for JIT...")
            (let ((input (linspace`(10 3 4 4) 0.001 0.0))
                  (weight (linspace `(6 3 4 4) 0.001 0.0)))
              (assert-equal
                  (:atol 1e-5 :rtol 1e-4)
                  (with-torch (input weight)
                    (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1)))
                  (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

(deftest test-convnd-4
  (testing "ConvND(1.0, bias=nil)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (linspace`(2 3 8 8) 0.001 0.0))
              (weight (linspace `(2 3 4 4) 0.001 0.0)))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight)
                (->caten (f:conv2d input weight :stride 1 :padding 0 :dilation 1 :groups 1)))
              (proceed (!convnd input weight :stride 1 :padding 0 :dilation 1 :groups 1))))))))

(deftest test-convnd-5
  (testing "ConvND([10, 3, 25, 25], in_channel=3, out_channel=6, kernel_size=[5, 5])"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(10 3 25 25)))
              (weight (rand `(6 3 5 5)))
              (bias (rand `(6))))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight bias)
                (->caten (f:conv2d input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1)))
              (proceed (!convnd input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1))))))))

(deftest test-convnd+relu
  (testing "ReLU(ConvND([10, 3, 25, 25], in_channel=3, out_channel=6, kernel_size=[5, 5]))"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(10 3 25 25)))
              (weight (rand `(6 3 5 5)))
              (bias (rand `(6))))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight bias)
                (->caten (f:relu (f:conv2d input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1))))
              (proceed (!relu (!convnd input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

(deftest test-convnd+gelu
  (testing "GeLU(ConvND([10, 3, 25, 25], in_channel=3, out_channel=6, kernel_size=[5, 5]))"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(10 3 25 25)))
              (weight (rand `(6 3 5 5)))
              (bias (rand `(6))))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight bias)
                (->caten (f:gelu (f:conv2d input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1))))
              (proceed (!gelu (!convnd input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1)))))))))
;; [TODO] kernel_size=1
;; [TODO] Padding, Dilation, Stride
(deftest test-convnd-no-kernel
  (testing "ConvND([10, 3, 25, 25], in_channel=3, out_channel=3, kernel_size[1, 1])"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(10 3 25 25)))
              (weight (rand `(3 3 1 1)))
              (bias (rand `(3))))
          (assert-equal
              (:atol 1e-3 :rtol 1e-3)
              (with-torch (input weight bias)
                (->caten (f:gelu (f:conv2d input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1))))
              (proceed (!gelu (!convnd input weight :bias bias :stride 1 :padding 0 :dilation 1 :groups 1)))))))))

(deftest test-convnd-padded
  (testing "ConvND([10, 3, 25, 25], in_channel=3, out_channel=6, kernel_size=[5, 5], padding=2)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(10 3 25 25)))
              (weight (rand `(6 3 5 5)))
              (bias (rand `(6))))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight bias)
                (->caten (f:gelu (f:conv2d input weight :bias bias :stride 1 :padding 2 :dilation 1 :groups 1))))
              (proceed (!gelu (!convnd input weight :bias bias :stride 1 :padding 2 :dilation 1 :groups 1)))))))))
;; JIT=1 won't work with it.
(deftest test-convnd-grouped
  (testing "ConvND([10, 3, 25, 25], in_channel=3, out_channel=6, kernel_size=[5, 5], padding=1, groups=32)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(10 32 25 25)))
              (weight (rand `(32 1 3 3)))
              (bias (rand `(32))))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight bias)
                (->caten (f:relu (f:conv2d input weight :bias bias :stride 1 :padding 1 :dilation 1 :groups 32))))
              (proceed (!relu (!convnd input weight :bias bias :stride 1 :padding 1 :dilation 1 :groups 32)))))))))

(deftest test-convnd-strided
  (testing "ConvND([10, 3, 25, 25], in_channel=3, out_channel=6, kernel_size=[5, 5], padding=1, groups=1, stride=2)"
    (with-given-dtype ((:float32 . "float32"))
      (with-no-grad
        (let ((input (rand `(10 3 25 25)))
              (weight (rand `(6 3 5 5)))
              (bias (rand `(6))))
          (assert-equal
              (:atol 1e-4 :rtol 1e-4)
              (with-torch (input weight bias)
                (->caten (f:relu (f:conv2d input weight :bias bias :stride 2 :padding 0 :dilation 1 :groups 1))))
              (proceed (!relu (!convnd input weight :bias bias :stride 2 :padding 0 :dilation 1 :groups 1)))))))))

;; Conv Schedule Test [TODO: Single Kernel]
