(in-package :cl-user)

(defpackage :caten/avm.test
  (:use :cl :rove :caten/air :caten/aasm :caten/avm))
(in-package :caten/avm.test)

(defun %seval (evaluated-to graph)
  (ok (= evaluated-to (buffer-value (%realize (fold-constant graph))))))

(defun %eval (evaluated-to graph)
  (ok (every #'= evaluated-to (buffer-value (%realize (fold-constant graph))))))

;; Note: keep in mind that other backends may use this test
;; dtype tests should be moved to outside of source
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

(deftest test-index-components
  (macrolet ((testwhen (order shape ans)
	       `(%eval
		 ,ans
		 (with-context
		     (a (%make-tensor ',shape :order ,order))
		   (b (%index-components a))))))
    (testwhen :column (3 3) #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))
    (testwhen :row (3 3) #(0.0 3.0 6.0 1.0 4.0 7.0 2.0 5.0 8.0))
    (testwhen :column (3 4 5) #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 14.0
				15.0 16.0 17.0 18.0 19.0 20.0 21.0 22.0 23.0 24.0 25.0 26.0 27.0
				28.0 29.0 30.0 31.0 32.0 33.0 34.0 35.0 36.0 37.0 38.0 39.0 40.0
				41.0 42.0 43.0 44.0 45.0 46.0 47.0 48.0 49.0 50.0 51.0 52.0 53.0
				54.0 55.0 56.0 57.0 58.0 59.0))
    (testwhen :row (3 4 5) #(0.0 12.0 24.0 36.0 48.0 3.0 15.0 27.0 39.0 51.0 6.0 18.0 30.0 42.0
			     54.0 9.0 21.0 33.0 45.0 57.0 1.0 13.0 25.0 37.0 49.0 4.0 16.0 28.0
			     40.0 52.0 7.0 19.0 31.0 43.0 55.0 10.0 22.0 34.0 46.0 58.0 2.0 14.0
			     26.0 38.0 50.0 5.0 17.0 29.0 41.0 53.0 8.0 20.0 32.0 44.0 56.0 11.0
			     23.0 35.0 47.0 59.0))))

;; [memo]
;;  Goal1 Tensor初期化の実装 (等差数列，randn, beta distribution)
;;  lazy-index-componentとmaskingを組み合わせて，VMの段階で変なアクセスできるようにする
;;  Goal2 Gemm
