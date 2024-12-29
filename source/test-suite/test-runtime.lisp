(in-package :caten/test-suite)
;; [TODO] Update this test
;; - TODO: Testing transfer, open-buffer, etc.
(defun =~ (x y) (< (abs (- x y)) 1e-6))
(defun %seval (evaluated-to graph &key (test #'=))
  (ok (funcall test evaluated-to (buffer-value (realize-graph (optimize-aasm graph) :buffer-type 'caten/byoc/lisp:LispBuffer)))))

(defun %eval (evaluated-to graph &key (test #'=))
  (ok (every test evaluated-to (buffer-value (realize-graph (optimize-aasm graph) :buffer-type 'caten/byoc/lisp:LispBuffer)))))

;; Note: keep in mind that other backends may use this test
;; dtype tests should be moved to outside of source
(defparameter *dtypes* `(:float64 :float32));; :uint64 :int64 :uint32 :int32 :uint16 :int16 :uint8 :int8))
(deftest simple-scalar-arithmetic
  (macrolet ((testwhen (op x y ans &optional (test '=))
	       `(dolist (dtype *dtypes*)
		  (%seval (coerce ,ans (dtype->lisp dtype))
			  (with-context
			    (a (%load (%salloc :dtype dtype) ,x))
			    (b (%load (%salloc :dtype dtype) ,y))
			    (c (,op a b)))
			  :test #',test))))
    (testwhen %add 1 1 2) (testwhen %add 10 10 20)
    (testwhen %sub 2 1 1) (testwhen %sub 1 2 -1)
    (testwhen %mul 2 3 6) (testwhen %mul 10 10 100)
    (testwhen %div 6 2 3) (testwhen %div 10 3 (/ 10 3) =~)
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
		   (b (%index-components a (%shape ',shape)))))))
    (testwhen :row (3 3) #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0))
    (testwhen :column (3 3) #(0.0 3.0 6.0 1.0 4.0 7.0 2.0 5.0 8.0))
    (testwhen :row (3 4 5) #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 14.0
			     15.0 16.0 17.0 18.0 19.0 20.0 21.0 22.0 23.0 24.0 25.0 26.0 27.0
			     28.0 29.0 30.0 31.0 32.0 33.0 34.0 35.0 36.0 37.0 38.0 39.0 40.0
			     41.0 42.0 43.0 44.0 45.0 46.0 47.0 48.0 49.0 50.0 51.0 52.0 53.0
			     54.0 55.0 56.0 57.0 58.0 59.0))
    (testwhen :column (3 4 5) #(0.0 20.0 40.0 5.0 25.0 45.0 10.0 30.0 50.0 15.0 35.0 55.0 1.0 21.0
				41.0 6.0 26.0 46.0 11.0 31.0 51.0 16.0 36.0 56.0 2.0 22.0 42.0 7.0
				27.0 47.0 12.0 32.0 52.0 17.0 37.0 57.0 3.0 23.0 43.0 8.0 28.0 48.0
				13.0 33.0 53.0 18.0 38.0 58.0 4.0 24.0 44.0 9.0 29.0 49.0 14.0 34.0
				54.0 19.0 39.0 59.0))))
(defun %arange1 (shape a b &key (dtype :float32) (order :row))
  (with-asm
    (m (%make-tensor shape :dtype dtype :order order))
    (i (%index-components m (%shape shape)))
    (alpha (%load (%salloc :dtype dtype) a))
    (beta  (%load (%salloc :dtype dtype) b))
    ;; a = alpha * i + b
    (t1 (%mul i alpha))
    (t2 (%add t1 beta))
    (c  (%store m t2))))

(deftest test-arange
  (macrolet ((testwhen (order shape a b ans)
	       `(%eval
		 ,ans
		 (with-context (_ (%arange1 ',shape ,a ,b :order ,order))))))
    ;; constant folding
    (testwhen :column (2 2) 0 0 #(0 0 0 0))
    (testwhen :column (2 2) 0 1 #(1 1 1 1))
    (testwhen :row (2 2) 1 0 #(0 1 2 3))
    (testwhen :column (3 5) 2 1 #(1.0 11.0 21.0 3.0 13.0 23.0 5.0 15.0 25.0 7.0 17.0 27.0 9.0 19.0 29.0))
    (testwhen :row (3 5) 2 1 #(1.0 3.0 5.0 7.0 9.0 11.0 13.0 15.0 17.0 19.0 21.0 23.0 25.0 27.0 29.0))
    (testwhen :column (3 5 5) 3 1 #(1.0 76.0 151.0 16.0 91.0 166.0 31.0 106.0 181.0 46.0 121.0 196.0
				    61.0 136.0 211.0 4.0 79.0 154.0 19.0 94.0 169.0 34.0 109.0 184.0
				    49.0 124.0 199.0 64.0 139.0 214.0 7.0 82.0 157.0 22.0 97.0 172.0
				    37.0 112.0 187.0 52.0 127.0 202.0 67.0 142.0 217.0 10.0 85.0 160.0
				    25.0 100.0 175.0 40.0 115.0 190.0 55.0 130.0 205.0 70.0 145.0 220.0
				    13.0 88.0 163.0 28.0 103.0 178.0 43.0 118.0 193.0 58.0 133.0 208.0
				    73.0 148.0 223.0))
    (testwhen :row (3 5 5) 3 1 #(1.0 4.0 7.0 10.0 13.0 16.0 19.0 22.0 25.0 28.0 31.0 34.0 37.0 40.0
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
     (a (%arange1 `(3 3) 1 0 :order :row))
     (b (%arange1 `(3 3) 0 2 :order :row))
     (c (%= `(3 3) :row a b))
     (m1 (%arange1 `(3 3) 0 1 :order :row))
     (m2 (%arange1 `(3 3) 0 0 :order :row))
     (d (%where c m1 m2)))))

(deftest test-view
  (testing "Slicing tensors"
    (%eval
     #(5 6 9 10)
     (with-context
       (a (%arange1 `(4 4) 1 0 :order :row))
       (b (%arange1 `(2 2) 0 0 :order :row))
       (v (%view a `(2 2) `(1 1) `(3 3) `(1 1) `(nil nil) (%stride `(4 4) :row)))
       (out (%add b v)))))
  (testing "Slicing tensors, -1 indexing"
    (%eval
     #(10 9 6 5)
     (with-context
       (a (%arange1 `(4 4) 1 0 :order :row))
       (b (%arange1 `(2 2) 0 0 :order :row))
       (v (%view a `(2 2) `(2 2) `(0 0) `(-1 -1) `(nil nil) (%stride `(4 4) :row)))
       (out (%add b v)))))
  (testing "Slicing tensors, read by 3"
    (%eval
     #(0 3 18 21)
     (with-context
       (a (%arange1 `(6 6) 1 0 :order :row))
       (b (%arange1 `(2 2) 0 0 :order :row))
       (v (%view a `(2 2) `(0 0) `(6 6) `(3 3) `(nil nil) (%stride `(6 6) :row)))
       (out (%add b v)))))
  (testing "Slicing tensors, read by -3"
    (%eval
     #(0 3 18 21)
     (with-context
       (a (%arange1 `(6 6) 1 0 :order :row))
       (b (%arange1 `(2 2) 0 0 :order :row))
       (v (%view a `(2 2) `(0 0) `(6 6) `(3 3) `(nil nil) (%stride `(6 6) :row)))
       (out (%add b v)))))
  (testing "Transpose"
    (%eval
     #(0.0 4.0 8.0 4.0 8.0 12.0 8.0 12.0 16.0)
     (with-context
       (a (%arange1 `(3 3) 1 0 :order :row))
       (b1 (%view a `(3 3) `(0 0) `(3 3) `(1 1) `(nil nil) (%stride `(3 3) :column)))
       (b2 (%arange1 `(3 3) 1 0 :order :column))
       (c (%add b1 b2)))))
  (testing "Broadcasting (all-reduce along all axes)"
    (%eval
     #(630.0)
     (with-context
       (a (%arange1 `(6 6) 1 0 :order :row))
       (b (%arange1 `(1 1) 0 0 :order :row))
       (b (%view b `(6 6) `(0 0) `(6 6) `(1 1) `(t t) (%stride `(6 6) :row)))
       (b (%add b a :reduction t)))))
  (testing "Broadcasting (all-reduce where axis=1)"
    (%eval
     #(3 3 3)
     (with-context
       (a (%arange1 `(3 3) 0 1 :order :column))
       (b (%arange1 `(3 1) 0 0 :order :column))
       (b (%view b `(3 3) `(0 0) `(3 3) `(1 1) `(nil t) (%stride `(3 3) :column)))
       (b (%add b a :reduction t)))))
  (testing "Broadcasting (all-reduce where axis=0 ndim=3)"
    (%eval
     #(3 3 3 3 3 3 3 3 3)
     (with-context
       (a (%arange1 `(3 3 3) 0 1 :order :column))
       (b (%arange1 `(3 3 1) 0 0 :order :column))
       (b (%view b `(3 3 3) `(0 0 0) `(3 3 3) `(1 1 1) `(nil nil t) (%stride `(3 3 3) :column)))
       (b (%add b a :reduction t)))))
  (testing "Broadcasting (all-scatter)"
    (%eval
     #(1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
     (with-context
       (a (%arange1 `(3 3 3) 0 0 :order :column))
       (b (%arange1 `(1 1 1) 0 1 :order :column))
       (b (%view b `(3 3 3) `(0 0 0) `(3 3 3) `(1 1 1) `(t t t) (%stride `(3 3 3) :column)))
       (c (%add a b :reduction t))))))

(defun make-squared-gemm (x y n)
  "a @ b.T"
  (optimize-aasm
   (with-context
     (a  (%make-tensor `(,n ,n) :id 'X))
     (b  (%make-tensor `(,n ,n) :id 'Y))
     (a  (%load a x))
     (b  (%load b y))
     (a1 (%view a `(,n ,n ,n) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(nil t nil) (%stride `(,n 1 ,n) :row)))
     (b1 (%view b `(,n ,n ,n) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(t nil nil) (%stride `(1 ,n ,n) :row)))
     (o  (%mul a1 b1))
     (c  (%make-tensor `(,n ,n 1)))
     (c  (%view c `(,n ,n 1) `(0 0 0) `(,n ,n ,n) `(1 1 1) `(nil nil t) (%stride `(,n ,n 1) :row)))
     (c  (%add c o :reduction t))
     (c  (%reshape c `(,n ,n))))))

(deftest test-squared-gemm
  (macrolet ((testcase (ans x y n)
	       `(ok (every #'(lambda (x) (= x ,ans)) (buffer-value (realize-graph (make-squared-gemm ,x ,y ,n) :buffer-type 'caten/byoc/lisp:LispBuffer))))))
    (testcase 3 1.0 1.0 3)
    (testcase 6 1.0 1.0 6)
    (testcase 9 1.0 1.0 9)

    (testcase 18 3.0 1.0 6)
    (testcase 18 1.0 3.0 6)
    (testcase 120 10.0 2.0 6)
    (testcase 120 2.0 10.0 6)
    (testcase 600 10.0 10.0 6)))
