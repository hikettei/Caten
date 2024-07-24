(in-package :cl-user)

(defpackage :caten/avm.test
  (:use :cl :rove :caten/air :caten/aasm :caten/avm))
(in-package :caten/avm.test)

(defun %seval (evaluated-to graph &key (test #'=))
  (ok (funcall test evaluated-to (buffer-value (%realize (fold-constant graph))))))

(defun %eval (evaluated-to graph &key (test #'=))
  (ok (every test evaluated-to (buffer-value (%realize (fold-constant graph))))))

;; ~~ helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %arange (shape a b &key (dtype :float32) (order :row))
  (with-asm
    (m (%make-tensor shape :dtype dtype :order order))
    (i (%index-components m))
    (alpha (%load (%salloc :dtype dtype) a))
    (beta  (%load (%salloc :dtype dtype) b))
    ;; a = alpha * i + b
    (t1 (%mul i alpha))
    (t2 (%add t1 beta))
    (c  (%store m t2))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    (testwhen %div 6 2 3) (testwhen %div 10 3 (/ 10 3))
    (let ((*dtypes* `(:uint32)))
      (testwhen %and 10 5 0) (testwhen %and 10 3 2)
      (testwhen %or 10 5 15) (testwhen %or 10 3 11))))

(deftest simple-scalar-logical
  (macrolet ((testwhen (op x y ans)
	       `(dolist (dtype *dtypes*)
		  (%seval ,ans
			  (with-context
			    (a (%load (%salloc :dtype dtype) ,x))
			    (b (%load (%salloc :dtype dtype) ,y))
			    (c (,op nil nil a b)))
			  :test #'eql))))
    (testwhen %> 10 1 t) (testwhen %> 0 0 nil) (testwhen %> 0 1 nil)
    (testwhen %< 10 1 nil) (testwhen %< 0 0 nil) (testwhen %< 0 1 t)
    (testwhen %>= 10 1 t) (testwhen %>= 0 0 t) (testwhen %>= 0 1 nil)
    (testwhen %<= 10 1 nil) (testwhen %<= 0 0 t) (testwhen %<= 0 1 t)))

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

(deftest simple-tensor-logical
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
		       (e (,op '(3 3) order e f)))
		     :test #'eql)))))
    (testwhen %> 2 1 #(t t t t t t t t t))
    (testwhen %>= 2 2 #(t t t t t t t t t ))
    (testwhen %< 2 1 #(nil nil nil nil nil nil nil nil nil))
    (testwhen %<= 2 2 #(t t t t t t t t t))))

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

(deftest test-arange
  (macrolet ((testwhen (order shape a b ans)
	       `(%eval
		 ,ans
		 (with-context (_ (%arange ',shape ,a ,b :order ,order))))))
    ;; constant folding
    (testwhen :column (2 2) 0 0 #(0 0 0 0))
    (testwhen :column (2 2) 0 1 #(1 1 1 1))
    (testwhen :column (2 2) 1 0 #(0 1 2 3))
    (testwhen :row (3 5) 2 1 #(1.0 7.0 13.0 19.0 25.0 3.0 9.0 15.0 21.0 27.0 5.0 11.0 17.0 23.0 29.0))
    (testwhen :column (3 5) 2 1 #(1.0 3.0 5.0 7.0 9.0 11.0 13.0 15.0 17.0 19.0 21.0 23.0 25.0 27.0 29.0))
    (testwhen :row (3 5 5) 3 1 #(1.0 46.0 91.0 136.0 181.0 10.0 55.0 100.0 145.0 190.0 19.0 64.0
				 109.0 154.0 199.0 28.0 73.0 118.0 163.0 208.0 37.0 82.0 127.0 172.0
				 217.0 4.0 49.0 94.0 139.0 184.0 13.0 58.0 103.0 148.0 193.0 22.0
				 67.0 112.0 157.0 202.0 31.0 76.0 121.0 166.0 211.0 40.0 85.0 130.0
				 175.0 220.0 7.0 52.0 97.0 142.0 187.0 16.0 61.0 106.0 151.0 196.0
				 25.0 70.0 115.0 160.0 205.0 34.0 79.0 124.0 169.0 214.0 43.0 88.0
				 133.0 178.0 223.0))
    (testwhen :column (3 5 5) 3 1 #(1.0 4.0 7.0 10.0 13.0 16.0 19.0 22.0 25.0 28.0 31.0 34.0 37.0 40.0
				    43.0 46.0 49.0 52.0 55.0 58.0 61.0 64.0 67.0 70.0 73.0 76.0 79.0
				    82.0 85.0 88.0 91.0 94.0 97.0 100.0 103.0 106.0 109.0 112.0 115.0
				    118.0 121.0 124.0 127.0 130.0 133.0 136.0 139.0 142.0 145.0 148.0
				    151.0 154.0 157.0 160.0 163.0 166.0 169.0 172.0 175.0 178.0 181.0
				    184.0 187.0 190.0 193.0 196.0 199.0 202.0 205.0 208.0 211.0 214.0
				    217.0 220.0 223.0))))

(deftest test-where
  (%eval
   #(0 0 1 0 0 0 0 0 0)
   (with-context
     (a (%arange `(3 3) 1 0 :order :column))
     (b (%arange `(3 3) 0 2 :order :column))
     (c (%= `(3 3) :column a b))
     (m1 (%arange `(3 3) 0 1 :order :column))
     (m2 (%arange `(3 3) 0 0 :order :column))
     (d (%where c m1 m2)))))

(deftest test-view
  (testing "Slicing the tensor"
    (%eval
     #(5 6)
     (with-context
       (a (%arange `(4 4) 1 0 :order :column))
       (b (%arange `(2 2) 0 0 :order :column))
       (v (%view a `(2 2) `(1 1) `(3 3) `(1 1) `(nil nil) (%stride `(4 4) `(1 0))))
       (out (%add b v))))))
       

;; Testing:
;;   -1. Slice
;;   -2. Broadcast
;;   -3 -1 Indexing
;;   -4. Composed View and infer-tensor-info
;;   -5. testing viewed constant folding
;;   - TODO: (arange x 0 0) into a single kernel. (Store 0=0 Fusion)
;; 
;;
;; AASM
;;(defun %triu (input &key (diagonal 0))
;;  (with-asm
;;    
;;    ))
;; [memo]
;;  Goal1 (OK) Tensor初期化の実装 (arange (OK)，randn, beta distribution)
;;    -> implement torch.triu
;;    -> implement logical.lisp, %where
;;  lazy-index-componentとmaskingを組み合わせて，VMの段階でeinsum的なアクセスができるようにする
;; e.g.: A[a * i + b]は (index_components == a*i+b)でmaskを生成 + whereで実装できる
;;  Goal2 Gemm
;;    -> Sum/Reduction, Broadcast/View/Reshapeが動く必要がある
;;  そしたらFrontendをちょっとだけ実装する (Print, Function, Tensor, etc)
