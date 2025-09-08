(in-package :caten/api)

(defclass HLOps () nil)
;; (defnode (:HLOps :SIGMOID))
;; [TODO] Cleanup!

(defun !sum (x &key (axis t) (keepdims nil))
  (multiple-value-bind (new-shape new-view dims) (parse-reduce-axes x axis)
    (let* ((out (make-tensor new-shape :dtype (tensor-dtype x) :initial-element 0.0))
	   (out (apply #'!view out new-view))
	   (out (!add out x :reduction t)))
      (if keepdims
          (apply #'!view out (map 'list #'(lambda (x) (if (and (listp x) (eql (car x) :~)) `(:~ 1) t)) new-view))
          (!drop-dims out dims)))))
