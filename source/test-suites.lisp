(in-package :cl-user)
(defpackage :caten.test (:use :cl :rove :caten :caten/avm :caten/aasm :caten/air))
(in-package :caten.test)

(deftest test-shape-tracker
  (ok
   (let ((a (make-tensor `(5 3 5)))
	 (b (make-tensor `(5 5 3))))
     (equal `(5 3 3) (tensor-shape (st "A[~ m n] B[~ n k] -> A[~ m k]" (a b))))))
  (ok
   (let ((a (make-tensor `(2 5 3 5)))
	 (b (make-tensor `(2 5 5 3))))
     (equal `(2 5 3 3) (tensor-shape (st "A[~ m n] B[~ n k] -> A[~ m k]" (a b))))))
  (ok
   (let ((a (make-tensor `(3 5)))
	 (b (make-tensor `(5 3))))
     (equal `(3 3) (tensor-shape (st "A[~ m n] B[~ n k] -> A[~ m k]" (a b))))))
  (ok (signals
	  (let ((a (make-tensor `(3 5)))
		(b (make-tensor `(5 5))))
	    (st "A[~] B[~] -> A[~]" (a b)))))
  (ok (signals
	  (let ((a (make-tensor `(2 5 3 5)))
		(b (make-tensor `(3 5 5 3))))
	    (st "A[~ m n] B[~ n k] -> A[~ m k]" (a b)))))
  (ok
   (let ((a (make-tensor nil))
	 (b (make-tensor `(3 3))))
     (equal nil (tensor-shape (st "A[] B[a b] -> A[]" (a b))))))
  (ok (signals
	  (let ((a (make-tensor `(3))))
	    (st "A[a b] -> A[a b]" (a)))))
  (ok (let ((a (make-tensor `(a b)))
	    (b (make-tensor `(c d))))
	(equal `(a d) (tensor-shape (st "A[m n] B[n k] -> A[m k]" (a b))))))
  (ok (let ((a (list (make-tensor `(a b)))))
	(equal `(a b) (tensor-shape (st "A[~] -> A[~]" (a)))))))

(deftest test-parse-view-subscript
  (flet ((test (list from1 to1 by1 broadcast1)
	   (with-slots ((from caten::from) (to caten::to) (by caten::by) (broadcast caten::broadcast))
	       (caten::parse-view-subscript 100 list)
	     (flet ((r (x) (caten/avm:buffer-value (caten/avm:%realize (caten::%tensor->aasm x)))))
	       (ok (and (equal (r from) from1) (equal (r to) to1) (equal (r by) by1) (equal broadcast broadcast1)))))))
    ;; A[0]
    (test 0 0 1 1 nil) (test 3 3 4 1 nil)
    ;; A[0:3]
    (test `(0 3) 0 3 1 nil) (test `(2 3) 2 3 1 nil)
    ;; A[4:0:1]
    (test `(4 0 -1) 4 0 -1 nil) (test `(0 5 2) 0 5 2 nil)
    ;; A[:broadcast]
    (test `(:~ 100) 0 100 1 t)))

(deftest test-auto-cast
  (flet ((test (dtype il)
	   (caten/avm:%realize (caten::%tensor->aasm (!add (make-tensor `(3 3) :dtype dtype) (make-tensor `(3 3) :dtype dtype :initial-element il))))))
    (testing "fconst(1) should be valid, iconst(1.0) should be invaild"
      (test :float16 1)
      (ok (test :float32 1))
      (ok (test :float64 1))
      (dolist (dtype `(:uint64 :int64 :uint32 :int32 :uint16 :int16 :uint8 :int8))
	(signals (test dtype 1.0))))))

(defun equal-to (a) #'(lambda (x) (= x a)))
(defun pproceed (params tensor)
  (let ((mdl (caten tensor)))
    (apply #'forward mdl params)))
(defun elements (tensor) (buffer-value (tensor-buffer tensor)))
(deftest test-make-tensor
  (testing "Fixed MakeTensor"
    (ok (every (equal-to 0) (elements (proceed (make-tensor `(10 10))))))
    (ok (every (equal-to 2) (elements (proceed (make-tensor `(10 10) :initial-element 2)))))
    (ok (every (equal-to 2) (elements (pproceed `((a . 2.0)) (make-tensor `(10 10) :initial-element 'a)))))
    ;; w/o inlining
    (let ((*external-simplifiers* nil))
      (ok (every (equal-to 0) (elements (proceed (make-tensor `(10 10))))))
      (ok (every (equal-to 2) (elements (proceed (make-tensor `(10 10) :initial-element 2)))))
      (ok (every (equal-to 2) (elements (pproceed `((a . 2.0)) (make-tensor `(10 10) :initial-element 'a)))))))
  (testing "Symbolic MakeTensor"
    (let ((a (pproceed `((a . 2)) (make-tensor `(a 10) :initial-element 'a :dtype :uint32))))
      (ok (and (every (equal-to 2) (elements a)) (= (length (elements a)) 20))))
    (let ((a (pproceed `((a . 4)) (make-tensor `(a a) :initial-element 'a :dtype :uint32))))
      (ok (and (every (equal-to 4) (elements a)) (= (length (elements a)) 16))))
    (with-no-grad
      (let ((a (pproceed `((a . 4) (b . 2)) (!add (make-tensor `(a b) :initial-element 'a :dtype :uint32) (iconst 'b)))))
	(ok (and (every (equal-to 6) (elements a)) (= (length (elements a)) 8)))))
    ;; w/o inlining
    (let ((*external-simplifiers* nil))
      (let ((a (pproceed `((a . 2)) (make-tensor `(a 10) :initial-element 'a :dtype :uint32))))
	(ok (and (every (equal-to 2) (elements a)) (= (length (elements a)) 20))))
      (let ((a (pproceed `((a . 4)) (make-tensor `(a a) :initial-element 'a :dtype :uint32))))
	(ok (and (every (equal-to 4) (elements a)) (= (length (elements a)) 16))))
      (with-no-grad
	(let ((a (pproceed `((a . 4) (b . 2)) (!add (make-tensor `(a b) :initial-element 'a :dtype :uint32) (iconst 'b)))))
	  (ok (and (every (equal-to 6) (elements a)) (= (length (elements a)) 8))))))))

(defun check-schedule (avm count &aux (graph (avm-graph avm)))
  (declare (type avm avm)
	   (type fixnum count))
  (let ((sched (optimize-aasm graph)))
    (assert
     (= (length (graph-nodes sched)) count)
     ()
     "check-schedule: should satisfy (kernel_count=~a) <= ~a.~%~a" (length (graph-nodes sched)) count sched))
  t)

(deftest test-simplifier-no-grad
  (with-no-grad
    (ok (check-schedule (caten (!neg (!add (iconst 0) (iconst 'a)))) 3))
    (ok (check-schedule (caten (!neg (!add (iconst 'a) (iconst 0)))) 3))
    (ok (check-schedule (caten (!neg (!mul (iconst 0) (iconst 'a)))) 2))
    (ok (check-schedule (caten (!neg (!mul (iconst 'a) (iconst 0)))) 2))
    (ok (check-schedule (caten (!neg (!mul (iconst 1) (iconst 'a)))) 3))
    (ok (check-schedule (caten (!neg (!mul (iconst 'a) (iconst 1)))) 3))

    ;; the top should not folded
    (ok (check-schedule (caten (!add (iconst 0) (iconst 'a))) 5))
    (ok (check-schedule (caten (!mul (iconst 0) (iconst 'a))) 5))
    
    (ok (= 1 (elements (pproceed `((a . 1)) (!add (iconst 0) (iconst 'a))))))
    (ok (= -1 (elements (pproceed `((a . 1)) (!neg (!add (iconst 0) (iconst 'a)))))))
    (ok (= 0 (elements (pproceed `((a . 1)) (!mul (iconst 0) (iconst 'a))))))
    (ok (= 0 (elements (pproceed `((a . 1)) (!neg (!mul (iconst 0) (iconst 'a)))))))
    ;; still depends on 'a
    (ok (signals (proceed (!neg (!add (iconst 0) (iconst 'a)))) 'avm-runtime-error))
    ;; a dependency is purged
    ;; failing
    (ok (= 0 (elements (proceed (!neg (!mul (iconst 0) (iconst 'a)))))))))

(deftest test-simplifier
  (ok (check-schedule (caten (!neg (!add (iconst 0) (iconst 'a)))) 4))
  (ok (check-schedule (caten (!neg (!add (iconst 'a) (iconst 0)))) 4))
  (ok (check-schedule (caten (!neg (!mul (iconst 0) (iconst 'a)))) 3))
  (ok (check-schedule (caten (!neg (!mul (iconst 'a) (iconst 0)))) 3))
  (ok (check-schedule (caten (!neg (!mul (iconst 1) (iconst 'a)))) 4))
  (ok (check-schedule (caten (!neg (!mul (iconst 'a) (iconst 1)))) 4))

  ;; the top should not folded
  (ok (check-schedule (caten (!add (iconst 0) (iconst 'a))) 6))
  (ok (check-schedule (caten (!mul (iconst 0) (iconst 'a))) 6))
  
  (ok (= 1 (elements (pproceed `((a . 1)) (!add (iconst 0) (iconst 'a))))))
  (ok (= -1 (elements (pproceed `((a . 1)) (!neg (!add (iconst 0) (iconst 'a)))))))
  (ok (= 0 (elements (pproceed `((a . 1)) (!mul (iconst 0) (iconst 'a))))))
  (ok (= 0 (elements (pproceed `((a . 1)) (!neg (!mul (iconst 0) (iconst 'a)))))))
  ;; still depends on 'a
  (ok (signals (proceed (!neg (!add (iconst 0) (iconst 'a)))) 'avm-runtime-error))
  ;; a dependency is purged
  ;; failing
  (ok (= 0 (elements (proceed (!neg (!mul (iconst 0) (iconst 'a))))))))

(deftest test-symbolic-shape-inference
  (ok (equal `(A B) (tensor-shape (!add (iconst 'a) (make-tensor `(a b))))))
  (with-no-grad
    (ok (equal `(A B) (tensor-shape (!add (iconst 'a) (make-tensor `(a b))))))))

(deftest test-broadcast
  (ok (equal `(1) (tensor-shape (!add (make-tensor `(1)) (make-tensor `(1))))))
  (ok (equal `(1 1) (tensor-shape (!add (make-tensor `(1)) (make-tensor `(1 1))))))
  (ok (equal `(1 1) (tensor-shape (!add (make-tensor `(1 1)) (make-tensor `(1))))))
  (ok (equal `(1 2) (tensor-shape (!add (make-tensor `(1 1)) (make-tensor `(2))))))
  (macrolet ((sp (answer shape1 shape2)
	       `(ok (equal ',answer (tensor-shape (!add (make-tensor ',shape1) (make-tensor ',shape2)))))))
    (sp (a b) (1) (a b))
    (sp (3 3) (3 3) (3 3))
    (sp (a b) nil (a b))))

(deftest test-composed-view-constant-folding
  (ok (equal `(2 2 2) (shape (!view (make-tensor `(3 3 3)) `(0 2) `(0 2) `(0 2)) )))
  (ok (equal `(1 1 1) (shape (!view (!view (make-tensor `(3 3 3)) `(0 2) `(0 2) `(0 2)) `(0 1) `(0 1) `(0 1)))))
  (ok (equal `(1 1 1) (shape (!view (!view (make-tensor `(4 4 4)) `(2 4) `(2 4) `(2 4)) `(1 2) `(1 2) `(1 2))))))

(deftest test-accumlation-keepdims
  (testing "Reduction w/ no-grad=t, keepdims=t"
    (macrolet ((testcase (op shape initial-element element-length evaluated-to axis)
		 `(with-no-grad
		    (let ((val1 (proceed (,op (make-tensor ',shape :initial-element ,initial-element) :axis ,axis :keepdims t))))
		      (ok (equal (shape val1) ',shape))
		      (ok (= (length (elements val1)) ,element-length))
		      (ok (every (equal-to ,evaluated-to) (elements val1)))))))
      (testcase !sum  (3 3) 1.0 1 9.0 t)
      (testcase !mean (3 3) 1.0 1 1.0 t)
      
      (testcase !sum  (3 3 3) 1.0 1 27.0 t)
      (testcase !mean (3 3 3) 1.0 1 1.0 t)

      (testcase !sum  (3 3) 1.0 3 3.0 1)
      (testcase !mean (3 3) 1.0 3 1.0 1)

      (testcase !sum (3 3) 2.0 1 18 t)
      (testcase !mean (3 3) 2.0 1 2.0 t))
    
    (macrolet ((testcase (op shape initial-element element-length evaluated-to axis params)
		 `(with-no-grad
		    (let ((val1 (pproceed ',params (,op (make-tensor ',shape :initial-element ,initial-element) :axis ,axis :keepdims t))))
		      (ok (equal (shape val1) ',shape))
		      (ok (= (length (elements val1)) ,element-length))
		      (ok (every (equal-to ,evaluated-to) (elements val1)))))))
      (testcase !sum  (a b) 1.0 1 9.0 t ((a . 3) (b . 3)))
      (testcase !mean (a b) 1.0 1 1.0 t ((a . 3) (b . 3)))
      
      (testcase !sum  (a b c) 1.0 1 27.0 t ((a . 3) (b . 3) (c . 3)))
      (testcase !mean (a b c) 1.0 1 1.0 t ((a . 3) (b . 3) (c . 3)))

      (testcase !sum  (a b) 1.0 3 3.0 1 ((a . 3) (b . 3)))
      (testcase !mean (a b) 1.0 3 1.0 1 ((a . 3) (b . 3)))

      (testcase !sum (a b) 2.0 1 18 t ((a . 3) (b . 3)))
      (testcase !mean (a b) 2.0 1 2.0 t ((a . 3) (b . 3))))))

(deftest test-accumlation-no-keepdims
  (testing "Reduction w/o keepdims=t"
    (macrolet ((testcase (op shape shape1 params initial-element element-length evaluated-to axis)
		 `(with-no-grad
		    (let ((val1 (pproceed ',params (,op (make-tensor ',shape :initial-element ,initial-element) :axis ,axis :keepdims nil))))
		      (ok (or (null ',shape1) (equal (shape val1) ',shape1)))
		      (ok (= (length (elements val1)) ,element-length))
		      (ok (every (equal-to ,evaluated-to) (elements val1)))))))
      (testcase !sum  (3 3) (3 1) nil 1.0 3 3.0 1)
      (testcase !mean (3 3) (3 1) nil 1.0 3 1.0 1)

      (testcase !sum (3 3) (3 1) nil 1.0 3 3.0 -1)
      (testcase !mean (3 3) (3 1) nil 1.0 3 1.0 -1)

      (testcase !sum (3 3) (1 1) nil 1.0 1 9.0 '(0 1))
      (testcase !mean (3 3) (1 1) nil 1.0 1 1.0 '(0 1))

      (testcase !sum (3 3) (1 1) nil 1.0 1 9.0 '(0 -1))
      (testcase !mean (3 3) (1 1) nil 1.0 1 1.0 '(0 -1))

      ;; TODO: Shapes are inferenced to (a 1)
      (testcase !sum (a b) nil ((a . 3) (b . 3)) 1.0 3 3.0 1)
      (testcase !mean (a b) nil ((a . 3) (b . 3)) 1.0 3 1.0 1)))
  (let ((*default-order* :row))
    (ok (equal `(3 1) (buffer-stride (tensor-buffer (proceed (!sum (make-tensor `(3 3) :initial-element 1) :axis 0))))))
    (ok (equal `(1 1) (buffer-stride (tensor-buffer (proceed (!sum (ax+b `(3 3) 0 1) :keepdims nil :axis 1))))))))

(deftest memory-order-test
  (testing "Column Major"
    (let ((*default-order* :column))
      (ok
       (every
	#'=
	(buffer-value (tensor-buffer (proceed (ax+b `(3 5 2) 1.0 1.0))))
	#(1.0 11.0 21.0 3.0 13.0 23.0 5.0 15.0 25.0 7.0 17.0 27.0 9.0 19.0 29.0 2.0
	  12.0 22.0 4.0 14.0 24.0 6.0 16.0 26.0 8.0 18.0 28.0 10.0 20.0 30.0)))))
  (testing "Row Major"
    (let ((*default-order* :row))
      (ok
       (every
	#'=
	(buffer-value (tensor-buffer (proceed (ax+b `(3 5 2) 1.0 1.0))))
	#(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 14.0 15.0 16.0 17.0
	  18.0 19.0 20.0 21.0 22.0 23.0 24.0 25.0 26.0 27.0 28.0 29.0 30.0))))))

(deftest simple-view-test
  (macrolet ((okwhen (form value)
	       `(if (eql *default-order* :row)
		    (ok (every #'= (buffer-value (tensor-buffer (proceed (!contiguous ,form)))) ,value))
		    (ok (= (reduce #'+ (buffer-value (tensor-buffer (proceed (!contiguous ,form))))) (reduce #'+ ,value))))))
    (dolist (*default-order* `(:row :column))
      (okwhen (!view (ax+b `(3 3) 1 0) 1 1) #(4.0))
      
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 0 0) #(0.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 0 1) #(1.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 0 2) #(2.0))
      
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 0 -1) #(2.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 0 -2) #(1.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 0 -3) #(0.0))

      ;; (/ 10 3) = 3 ... 1
      (ok (!view (ax+b `(10) 1 0) `(0 9 3)) #(0.0 3.0 6.0))

      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 t 0) #(0.0 3.0 6.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 t 1) #(1.0 4.0 7.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 t 2) #(2.0 5.0 8.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 0 t t) #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 1 t t) (map 'list #'(lambda (x) (+ x 9)) #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 2 t t) (map 'list #'(lambda (x) (+ x 18)) #(0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0)))
      (okwhen (!view (ax+b `(3 3 3) 1 0) 2 2 2) #(26.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) `(0 2) `(0 2) 2) #(2.0 5.0 11.0 14.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) `(1 3) `(1 3) 2) #(14.0 17.0 23.0 26.0))
      (okwhen (!view (ax+b `(3 3 3) 1 0) `(1 3) `(1 3) `(1 -1)) #(13.0 16.0 22.0 25.0))
      (okwhen (!view (ax+b `(10) 1 0) `(-1 1 -2)) #(9.0 7.0 5.0 3.0)))))

(deftest composed-view-test
  (macrolet ((okwhen (form value)
	       `(if (eql *default-order* :row)
		    (ok (every #'= (buffer-value (tensor-buffer (proceed (!contiguous ,form)))) ,value))
		    (ok (= (reduce #'+ (buffer-value (tensor-buffer (proceed (!contiguous ,form))))) (reduce #'+ ,value))))))
    (dolist (*default-order* `(:row :column))
      ;; Needs more case to test
      (okwhen (!view (!view (ax+b `(10) 1 0) `(0 10)) `(0 5)) #(0.0 1.0 2.0 3.0 4.0))
      (okwhen (!view (!view (ax+b `(10) 1 0) `(0 -1)) `(2 5)) #(2.0 3.0 4.0))
      (okwhen (!view (!view (ax+b `(20) 1 0) `(0 10 2)) `(2 6)) #(2.0 4.0))
      (okwhen (!view (!view (ax+b `(20) 1 0) `(18 0 -2)) `(10 2 -2)) #(8.0 10.0 12.0 14.0)))))

(deftest symbolic-view-test
  (ok (every #'= #(0.0) (buffer-value (tensor-buffer (pproceed `((a . 2) (b . 3)) (!view (ax+b `(a b) 1 0) 0 0))))))
  (ok (every #'= #(23.0) (buffer-value (tensor-buffer (pproceed `((a . 2) (b . 3)) (!contiguous (!view (ax+b `(10 10) 1 0) 'a 'b))))))))

(deftest slice-broadcast-not-coexisting
  (ok (signals (!view (ax+b `(1 3) 0 1) `(:~ 2) 1) 'caten-forward-error))
  (ok (!view (ax+b `(1 3) 0 1) `(:~ 2) t)))
;; TODO
;; - Implement Autograd
;;   - 1. View Backward
;;       - BroadcastingとSliceは同時に適用できないとする (OK)
;;       - Composed Slice Backward Test.
;;   - 2. Sum/Mean Backward
;;   - 3. Test ChainRule
;;   ax+bからのSumができない？？

;; - Implement Matmul
;;   - Float前範囲に対するULP検証は外でやる

;; - AJiTでaasmをloweringする
;;   - CLANG/METAL/CUDA JIT
;; log1p fusionとか実装する
