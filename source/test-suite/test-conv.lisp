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
        (if (= (ctx:getenv :JIT) 1)
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
(defun %arange (shape a b &key (dtype :float32) (order :row))
  "Creates a tensor where each element is generated using alpha * i + beta."
  (with-context
      (m (%make-tensor shape :dtype dtype :order order))
    (i (%index-components m (%shape shape)))
    (alpha (%load (%salloc :dtype dtype) a))
    (beta  (%load (%salloc :dtype dtype) b))
    (t1 (%mul i alpha))
    (t2 (%add t1 beta))
    (c  (%store m t2 :id 'out))))

(defparameter aranged-tensor
  (%arange '(4) 1 0 :dtype :float32 :order :row)) ;; Shape: (4)

(defparameter realized-tensor (%realize aranged-tensor))

(defparameter tensor-with-buffer
  (make-tensor '(4) :dtype :float32 :order :row :from realized-tensor))

(defparameter reshaped-tensor
  (!reshape tensor-with-buffer '(2 2))) ;; Shape: (2, 2)

(defparameter viewed-tensor
  (!view reshaped-tensor '(0 2) '(0 2)))

(format t "Original Tensor Proceeded:~%")
(print (proceed tensor-with-buffer))

(format t "Reshaped Tensor Proceeded (2D):~%")
(print (proceed reshaped-tensor))

(format t "Viewed Tensor Proceeded (2D View):~%")
(print (proceed viewed-tensor))




;; broadcast example
(defparameter tensor
  (make-tensor '(4) :dtype :float32 :order :row :initial-element 1.0))

(defparameter broadcast-test (!add tensor (!const tensor 1)))

(defparameter broadcast-tracker (tensor-tr broadcast-test))

(defparameter broadcasted-tensor (proceed (!add tensor (!reshape (!const tensor 1) '(1)))))



(print (tr-broadcast (tensor-tr broadcasted-tensor)))
(format t "Broadcasted Tensor Proceeded:~%")
(print (proceed broadcast-test))

(format t "Broadcast Metadata: ~A~%" (tr-broadcast broadcast-tracker))

(format t "Type of realized tensor: ~a~%" (type-of realized-tensor))
;;metadata isn't showing :(