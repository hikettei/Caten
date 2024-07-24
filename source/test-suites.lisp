(in-package :cl-user)
(defpackage :caten.test (:use :cl :rove :caten))
(in-package :caten.test)

(deftest test-shape-tracker
  (ok
   ;; TODO: More tests!
   (let ((a (make-tensor `(5 3 5)))
	 (b (make-tensor `(5 5 3))))
     (equal `(5 3 3) (tensor-shape (st "A[~ m n] B[~ n k] -> A[~ m k]" (a b)))))))
