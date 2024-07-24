(in-package :cl-user)

(defpackage :caten/avm.test
  (:use :cl :rove :caten/air :caten/aasm :caten/avm))
(in-package :caten/avm.test)

(defun %seval (evaluated-to graph)
  (ok (= evaluated-to (buffer-value (%realize (fold-constant graph))))))

(defun %eval (evaluated-to graph)
  (ok (every #'= evaluated-to (buffer-value (%realize (fold-constant graph))))))

;; Note: keep in mind that other backends may use this test
(defparameter *dtypes* `(:float64 :float32));; :uint64 :int64 :uint32 :int32 :uint16 :int16 :uint8 :int8))
(deftest simple-scalar-arithmetic
  (macrolet ((testwhen (op x y ans)
	       `(dolist (dtype *dtypes*)
		  (%seval ,ans
			  (with-context
			      (a (%load (%salloc :dtype dtype) ,x))
			    (b (%load (%salloc :dtype dtype) ,y))
			    (c (,op a b)))))))
    (testwhen %add 1 1 2) (testwhen %add 10 10 20)
    (testwhen %sub 2 1 1) (testwhen %sub 1 2 -1)
    (testwhen %mul 2 3 6) (testwhen %mul 10 10 100)
    (testwhen %div 6 2 3) (testwhen %div 10 3 (/ 10 3))))

(deftest simple-tensor-arithmetic
  (macrolet ((testwhen (op x y ans)
	       `(dolist (dtype *dtypes*)
		  (dolist (order `(:row :column))
		    (%eval
		     ,ans
		     (with-context
		       (a (%load (%salloc :dtype dtype) ,x))
		       (b (%load (%salloc :dtype dtype) ,y))
		       (c (%make-tensor `(3 3) :dtype dtype :order order))
		       (d (%make-tensor `(3 3) :dtype dtype :order order))
		       (e (%add c a))
		       (f (%add d b))
		       (e (,op e f))))))))
    (testwhen %add 1 1 #(2 2 2 2 2 2 2 2 2))
    (testwhen %sub 3 1 #(2 2 2 2 2 2 2 2 2))
    (testwhen %mul 5 2 #(10 10 10 10 10 10 10 10 10))
    (testwhen %div 10 2 #(5 5 5 5 5 5 5 5 5))))

