(in-package :caten/api)

(defclass HLOps () nil)
;; (defnode (:HLOps :SIGMOID))
;; [TODO] Cleanup!

(defun !sum (x &key (axis t) (keepdims nil))
  (multiple-value-bind (new-shape new-view dims) (parse-reduce-axes x axis)
    (let* ((out (make-tensor new-shape :dtype (tensor-dtype x) :initial-element 0.0))
	   (out (apply #'!view out new-view))
	   (out (!add out x :reduction t))
           (out (apply #'!view out (map 'list #'(lambda (x) (if (and (listp x) (eql (car x) :~)) `(:~ 1) t)) new-view))))
      (if keepdims
          out
          (!squeeze out dims)))))

(defun !matmul (x y)
  (declare (type Tensor x y))
  (multiple-value-bind (n1 n2) (values (tensor-nrank x) (tensor-nrank y))
    (assert (= n1 n2) () "Cannot multiply matrices with different dimensions. Are they properly broadcasted?~%A: ~a~%B: ~a" x y)
    (let* ((mid (loop for i upfrom 0 below (min (- n1 1) (- n2 1) 1) collect 1))
	   (x (!reshape x `(,@(butlast (tensor-shape x) 1) ,@mid ,(car (last (tensor-shape x))))))
	   (y (!reshape y `(,@(butlast (tensor-shape y) 2) ,@mid ,@(last (tensor-shape y) (min n2 2))))))
      (!sum (!mul x (!transpose y -1 (- (min n2 2)))) :axis -1))))
