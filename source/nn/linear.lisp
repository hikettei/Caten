(in-package :caten/nn)

(defmodel (Linear (in-features out-features &key (bias t)) :documentation "
```
(Linear in-featrues out-features &key (bias t))
```
Implements a linear transformation.
")
    ((weight (xavier-uniform `(,out-features ,in-features) :requires-grad t))
     (bias   (when bias (uniform `(,out-features) :low (- (sqrt (/ out-features))) :high (sqrt (/ out-features)) :requires-grad t)))))

(defmethod call ((model Linear) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((weight weight) (bias bias)) model
      (if bias
	  (!add (!matmul x (!t weight)) bias)
	  (!matmul x (!t weight))))))
;; TODO: BiLinear
