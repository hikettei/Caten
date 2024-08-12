(in-package :caten/nn)

(defmodel (Linear (in-features out-features &key (bias t)))
  ((weight (make-tensor `(,out-features ,in-features)))
   (bias   (when bias (make-tensor `(,out-features))))))

(defmethod call ((model Linear) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((weight weight) (bias bias)) model
      (if bias
	  (!add (!matmul x (!t weight)) bias)
	  (!matmul x (!t weight))))))

;; TODO: BiLinear
