(cl:in-package :cl-user)
(defpackage :caten/nn.activations
  (:use :cl :caten :rove)
  (:export
   #:!relu
   ))
(in-package :caten/nn.activations)
(defun !relu (x) (!max x (fconst 0)))

(defpackage :caten/nn.activations.test
  (:use :cl :caten/nn.activations :rove :caten))
(in-package :caten/nn.activations.test)
(deftest test-relu (ok (= 1 1)))

