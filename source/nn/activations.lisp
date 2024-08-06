(cl:in-package :caten/nn)

(defun !relu (x) (!max x (fconst 0)))

;; TODO1. ReLU/BatchNorm/Linear/Convなど一通り実装
;; TODO2 1.0 ULP Testingを追加

(in-package :caten/nn.test)

(deftest test-relu
  (ok (= 1 1)))
