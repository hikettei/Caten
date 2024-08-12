(in-package :caten/nn)

(defmodel (Linear (in-features out-features))
  ((weight (make-tensor `(,out-features ,in-features)))
   (bias   (make-tensor `(,out-features)))))

(defmethod call ((model Linear) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((weight weight) (bias bias)) model
      (!add (!matmul x (!t weight)) bias))))
