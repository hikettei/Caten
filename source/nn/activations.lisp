(cl:in-package :caten/nn)

(defun !relu (x)
  "ReLU"
  (declare (type Tensor x))
  (!max x (fconst 0)))

(in-package :caten/nn.test)

(deftest test-relu
  (ok (every #'(lambda (x) (>= x 0)) (elements (proceed (!relu (ax+b `(10 10) 1 -5)))))))
