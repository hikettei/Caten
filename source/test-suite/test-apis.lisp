(in-package :caten/test-suite)

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
	     (flet ((r (x) (caten/runtime:buffer-value (caten/runtime:realize-graph (caten::%tensor->aasm x) :buffer-type 'caten/byoc/lisp:LispBuffer))))
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
	   (caten/runtime:realize-graph (caten::%tensor->aasm (!add (make-tensor `(3 3) :dtype dtype) (make-tensor `(3 3) :dtype dtype :initial-element il))) :buffer-type 'caten/byoc/lisp:LispBuffer)))
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

(defun runtime-check-schedule (runtime count &aux (graph (runtime-graph runtime)))
  (declare (type GraphRuntime runtime)
	   (type fixnum count))
  (let ((sched (optimize-aasm graph)))
    ;; Only checked on VM Mode
    (when (caten/codegen/backend:jit-mode-p)
      (assert
       (= (length (graph-nodes sched)) count)
       ()
       "check-schedule: should satisfy (kernel_count=~a) <= ~a.~%~a" (length (graph-nodes sched)) count sched)))
  t)

(deftest test-simplifier-no-grad
  (with-no-grad
    (ok (runtime-check-schedule (caten (!neg (!add (iconst 0) (iconst 'a)))) 3))
    (ok (runtime-check-schedule (caten (!neg (!add (iconst 'a) (iconst 0)))) 3))
    (ok (runtime-check-schedule (caten (!neg (!mul (iconst 0) (iconst 'a)))) 2))
    (ok (runtime-check-schedule (caten (!neg (!mul (iconst 'a) (iconst 0)))) 2))
    (ok (runtime-check-schedule (caten (!neg (!mul (iconst 1) (iconst 'a)))) 3))
    (ok (runtime-check-schedule (caten (!neg (!mul (iconst 'a) (iconst 1)))) 3))
    ;; the top should not folded
    (ok (runtime-check-schedule (caten (!add (iconst 0) (iconst 'a))) 2))
    (ok (runtime-check-schedule (caten (!mul (iconst 0) (iconst 'a))) 1))
    (ok (= 1 (elements (pproceed `((a . 1)) (!add (iconst 0) (iconst 'a))))))
    (ok (= -1 (elements (pproceed `((a . 1)) (!neg (!add (iconst 0) (iconst 'a)))))))
    (ok (= 0 (elements (pproceed `((a . 1)) (!mul (iconst 0) (iconst 'a))))))
    (ok (= 0 (elements (pproceed `((a . 1)) (!neg (!mul (iconst 0) (iconst 'a)))))))
    ;; still depends on 'a
    (ok (signals (proceed (!neg (!add (iconst 0) (iconst 'a)))) 'runtime-error))
    ;; a dependency is purged
    ;; failing
    (ok (= 0 (elements (proceed (!neg (!mul (iconst 0) (iconst 'a)))))))))

(deftest test-simplifier
  (ok (runtime-check-schedule (caten (!neg (!add (iconst 0) (iconst 'a)))) 4))
  (ok (runtime-check-schedule (caten (!neg (!add (iconst 'a) (iconst 0)))) 4))
  (ok (runtime-check-schedule (caten (!neg (!mul (iconst 0) (iconst 'a)))) 3))
  (ok (runtime-check-schedule (caten (!neg (!mul (iconst 'a) (iconst 0)))) 3))
  (ok (runtime-check-schedule (caten (!neg (!mul (iconst 1) (iconst 'a)))) 4))
  (ok (runtime-check-schedule (caten (!neg (!mul (iconst 'a) (iconst 1)))) 4))
  ;; the top should not folded
  (ok (runtime-check-schedule (caten (!add (iconst 0) (iconst 'a))) 3))
  (ok (runtime-check-schedule (caten (!mul (iconst 0) (iconst 'a))) 2))
  (ok (= 1 (elements (pproceed `((a . 1)) (!add (iconst 0) (iconst 'a))))))
  (ok (= -1 (elements (pproceed `((a . 1)) (!neg (!add (iconst 0) (iconst 'a)))))))
  (ok (= 0 (elements (pproceed `((a . 1)) (!mul (iconst 0) (iconst 'a))))))
  (ok (= 0 (elements (pproceed `((a . 1)) (!neg (!mul (iconst 0) (iconst 'a)))))))
  ;; still depends on 'a
  (ok (signals (proceed (!neg (!add (iconst 0) (iconst 'a)))) 'runtime-error))
  ;; a dependency is purged
  ;; failing
  (ok (= 0 (elements (proceed (!neg (!mul (iconst 0) (iconst 'a))))))))

(deftest test-symbolic-shape-inference
  (ok (equal `(A B) (tensor-shape (!add (iconst 'a) (make-tensor `(a b))))))
  (with-no-grad
    (ok (equal `(A B) (tensor-shape (!add (iconst 'a) (make-tensor `(a b))))))))

(deftest test-broadcast-shape-inference
  (ok (equal `(1) (tensor-shape (!add (make-tensor `(1)) (make-tensor `(1))))))
  (ok (equal `(1 1) (tensor-shape (!add (make-tensor `(1)) (make-tensor `(1 1))))))
  (ok (equal `(1 1) (tensor-shape (!add (make-tensor `(1 1)) (make-tensor `(1))))))
  (ok (equal `(1 2) (tensor-shape (!add (make-tensor `(1 1)) (make-tensor `(2))))))
  (macrolet ((sp (answer shape1 shape2)
	       `(ok (equal ',answer (tensor-shape (!add (make-tensor ',shape1) (make-tensor ',shape2)))))))
    (sp (a b) (1) (a b))
    (sp (3 3) (3 3) (3 3))
    (sp (a b) nil (a b))))

(deftest test-composed-view-constant-folding-shape-inference
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
		      (ok (every (equal-to ,evaluated-to) (elements val1)) (format nil "expecting ~a, getting ~a" ,evaluated-to (elements val1)))))))
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
      ;; Failing case (the input shape includes 1)
      (testcase !sum (1 1 3 3 3 3 3 3) (1 1 3 3 3 1 1 1) nil 1.0 27 27 '(-1 -2 -3))
      (testcase !sum  (3 3) (3 1) nil 1.0 3 3.0 1)
      (testcase !mean (3 3) (3 1) nil 1.0 3 1.0 1)

      (testcase !sum (3 3) (3 1) nil 1.0 3 3.0 -1)
      (testcase !mean (3 3) (3 1) nil 1.0 3 1.0 -1)

      (testcase !sum (3 3) (1 1) nil 1.0 1 9.0 '(0 1))
      (testcase !mean (3 3) (1 1) nil 1.0 1 1.0 '(0 1))

      (testcase !sum (3 3) (1 1) nil 1.0 1 9.0 '(0 -1))
      (testcase !mean (3 3) (1 1) nil 1.0 1 1.0 '(0 -1))

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
	(elements (proceed (ax+b `(3 5 2) 1.0 1.0)))
	#(1.0 11.0 21.0 3.0 13.0 23.0 5.0 15.0 25.0 7.0 17.0 27.0 9.0 19.0 29.0 2.0
	  12.0 22.0 4.0 14.0 24.0 6.0 16.0 26.0 8.0 18.0 28.0 10.0 20.0 30.0)))))
  (testing "Row Major"
    (let ((*default-order* :row))
      (ok
       (every
	#'=
	(elements (proceed (ax+b `(3 5 2) 1.0 1.0)))
	#(1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0 10.0 11.0 12.0 13.0 14.0 15.0 16.0 17.0
	  18.0 19.0 20.0 21.0 22.0 23.0 24.0 25.0 26.0 27.0 28.0 29.0 30.0))))))

(deftest test-where-regression-test
  (testing "where(scalar, tensor1, tensor2) = tensor1 or tensor2"
    (let ((out (caten
		(!where (!>= (iconst 'a) (iconst 3)) (make-tensor `(40 40) :initial-element 1.0) (make-tensor `(40 40) :initial-element 2.0)))))
      (ok (= (length (elements (forward out `(a . 4)))) (* 40 40)))
      (ok (every (equal-to 1) (elements (forward out `(a . 4)))))
      (ok (every (equal-to 2) (elements (forward out `(a . 2)))))))
  (testing "where(tensor, scal1, scal2) = scal1 or scal2"
    (let ((out (caten (!where (!>= (ax+b `(40 40) 1 -40) (fconst 0)) (fconst 2.0) (fconst 3.0)))))
      (ok (= (length (elements (forward out))) (* 40 40)))
      (let ((*default-order* :row))
	(let ((result (elements (forward out))))
	  (ok (every (equal-to 3.0) (map 'list #'(lambda (x) (aref result x)) (range 0 40))))
	  (ok (every (equal-to 2.0) (map 'list #'(lambda (x) (aref result x)) (range 40 (* 40 40))))))))))

(deftest simple-view-test
  (macrolet ((okwhen (form value)
	       `(if (eql *default-order* :row)
		    (ok (every #'= (elements (proceed (!contiguous ,form))) ,value))
		    (ok (= (reduce #'+ (elements (proceed (!contiguous ,form)))) (reduce #'+ ,value))))))
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
		    (ok (every #'= (elements (proceed (!contiguous ,form))) ,value))
		    (ok (= (reduce #'+ (elements (proceed (!contiguous ,form)))) (reduce #'+ ,value))))))
    (dolist (*default-order* `(:row :column))
      ;; Needs more case to test
      (okwhen (!view (!view (ax+b `(10) 1 0) `(0 10)) `(0 5)) #(0.0 1.0 2.0 3.0 4.0))
      (okwhen (!view (!view (ax+b `(10) 1 0) `(1 10)) `(0 4)) #(1.0 2.0 3.0 4.0))
      (okwhen (!view (!view (ax+b `(10) 1 0) `(0 -1)) `(2 5)) #(2.0 3.0 4.0))
      (okwhen (!view (!view (ax+b `(20) 1 0) `(0 10 2)) `(2 6)) #(2.0 4.0))
      (okwhen (!view (!view (ax+b `(20) 1 0) `(18 0 -2)) `(10 2 -2)) #(8.0 10.0 12.0 14.0)))))

(deftest symbolic-view-test
  (ok (every #'= #(0.0) (elements (pproceed `((a . 2) (b . 3)) (!view (ax+b `(a b) 1 0) 0 0)))))
  (ok (every #'= #(23.0) (elements (pproceed `((a . 2) (b . 3)) (!contiguous (!view (ax+b `(10 10) 1 0) 'a 'b)))))))

(deftest slice-broadcast-not-coexisting
  (ok (signals (!view (ax+b `(1 3) 0 1) `(:~ 2) 1) 'caten-forward-error))
  (ok (!view (ax+b `(1 3) 0 1) `(:~ 2) t)))

(deftest view-backward
  (macrolet ((okwhen (form tensor grad)
	       `(let ((m (caten ,form)))
		  (forward m)
		  (backward m nil)
		  (ok (every #'= (elements (grad ,tensor)) ,grad)))))
    (let ((*default-order* :row))
      (let ((tensor (make-tensor `(10) :requires-grad t)))
	(okwhen (!view tensor `(0 3)) tensor #(1 1 1 0 0 0 0 0 0 0)))
      (let ((tensor (make-tensor `(10) :requires-grad t)))
	(okwhen (!view tensor `(2 4)) tensor #(0 0 1 1 0 0 0 0 0)))
      (let ((tensor (make-tensor `(10) :requires-grad t)))
	(okwhen (!contiguous (!view tensor `(4 1 -1))) tensor #(0 0 1 1 1 0 0 0 0 0)))
      (testing "Should work w/o !contiguous in the jit"
	(let ((tensor (make-tensor `(10) :requires-grad t)))
	  (okwhen (!view tensor `(4 1 -1)) tensor #(0 0 1 1 1 0 0 0 0 0))))
      (let ((tensor (make-tensor `(7 7 7) :requires-grad t)))
	(okwhen (!view tensor `(1 4) `(2 5) `(3 5)) tensor #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0
							     0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0
							     1.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
							     0.0)))
      (let ((tensor (make-tensor `(10) :requires-grad t)))
	(okwhen (!view (!view tensor `(0 5)) 0) tensor #(1 0 0)))
      (let ((tensor (make-tensor `(10) :requires-grad t)))
	(okwhen (!view (!view tensor `(0 5)) 1) tensor #(0 1 0)))
      (let ((tensor (make-tensor `(10) :requires-grad t)))
	(okwhen (!view (!view tensor `(1 5)) 0) tensor #(0 0 1)))
      ;; [TODO] Need more cases!
      )))

(deftest reduction-backward
  (macrolet ((okwhen (form tensor grad)
	       `(let ((m (caten ,form)))
		  (forward m)
		  (backward m nil)
		  (ok (every #'= (elements (grad ,tensor)) ,grad)))))
    (let ((*default-order* :row))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!mean a) a #(0.11111111 0.11111111 0.11111111 0.11111111 0.11111111 0.11111111 0.11111111 0.11111111 0.11111111)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!sum a) a #(1 1 1 1 1 1 1 1 1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!sum a :axis 1) a #(1 1 1 1 1 1 1 1 1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!sum a :axis 0) a #(1 1 1 1 1 1 1 1 1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!sum a :axis 1 :keepdims t) a #(1 1 1 1 1 1 1 1 1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!sum a :axis 0 :keepdims t) a #(1 1 1 1 1 1 1 1 1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!sum a :keepdims t) a #(1 1 1 1 1 1 1 1 1)))

      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!neg (!sum a)) a #(-1 -1 -1 -1 -1 -1 -1 -1 -1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!neg (!sum a :axis 1)) a #(-1 -1 -1 -1 -1 -1 -1 -1 -1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!neg (!sum a :axis 0)) a #(-1 -1 -1 -1 -1 -1 -1 -1 -1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!neg (!sum a :axis 1 :keepdims t)) a #(-1 -1 -1 -1 -1 -1 -1 -1 -1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!neg (!sum a :axis 0 :keepdims t)) a #(-1 -1 -1 -1 -1 -1 -1 -1 -1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!neg (!sum a :keepdims t)) a #(-1 -1 -1 -1 -1 -1 -1 -1 -1)))
      (let ((a (make-tensor `(3 3) :requires-grad t))) (okwhen (!sum (ax+b `(3 3) 1 0 :out a)) a #(1 1 1 1 1 1 1 1 1))))))
;; Accumlating gradients multiple times
(deftest test-chain-rule
  (testing "A x B"
    (let ((a (make-tensor `(3 3) :requires-grad t :initial-element 2.0))
	  (b (make-tensor `(3 3) :requires-grad t :initial-element 3.0)))
      (let ((m (caten (!mul a b))))
	(forward m)
	(backward m nil))
      (ok (every (equal-to 3) (elements (grad a))))
      (ok (every (equal-to 2) (elements (grad b))))))
  (testing "A x B (Zero-Grad)"
    (let ((a (make-tensor `(3 3) :requires-grad t :initial-element 2.0))
	  (b (make-tensor `(3 3) :requires-grad t :initial-element 3.0)))
      (let ((m (caten (!mul a b))))
	(forward m)
	(backward m nil)
	(forward m)
	(backward m nil))
      (ok (every (equal-to 6) (elements (grad a))))
      (ok (every (equal-to 4) (elements (grad b))))))
  (testing "f(A) + f(A) + f(A)"
    (macrolet ((f (form v)
		 `(dolist (zero-grad `(nil t))
		    (let ((a (make-tensor `(3 3) :requires-grad t :initial-element 1.0)))
		      (let ((m (caten ,form)))
			(forward m)
			(backward m nil)
			(when zero-grad
			  (forward m) (backward m nil)))
		      (ok (every (equal-to ,v) (elements (grad a))) (format nil "expecting ~a, getting ~a" ,v (elements (grad a))))))))
      (f (!+ (!neg a) (!neg a) (!neg a)) -3)
      (f (!+ (!neg a) (!neg a) a) -1)
      (f (!+ a (!neg a) (!neg a)) -1)
      (f (!+ (!neg a) a (!neg a)) -1)
      (f (!+ (!neg a) a a) 1)
      (f (!+ a (!neg a) a) 1)
      (f (!+ a a (!neg a)) 1)
      (f (!neg (!+ a a a)) -3)
      (f (!neg (!+ (!neg a) a (!neg a))) 1))))

(deftest test-zero-grad-reduction
  (let ((a (make-tensor `(3 3) :requires-grad t)))
    (let ((m (caten (!sum a :axis t))))
      (forward m) (backward m nil) (forward m) (backward m nil)
      (ok (every (equal-to 2) (elements (grad a))))))
  (let ((a (make-tensor `(3 3) :requires-grad t)))
    (let ((m (caten (!sum a :axis t :keepdims t))))
      (forward m) (backward m nil) (forward m) (backward m nil)
      (ok (every (equal-to 2) (elements (grad a))))))
  (let ((a (make-tensor `(3 3) :requires-grad t)))
    (let ((m (caten (!neg (!sum a :axis t)))))
      (forward m) (backward m nil) (forward m) (backward m nil)
      (ok (every (equal-to -2) (elements (grad a)))))))

(deftest test-gemm
  (let ((a (ax+b `(3 4) 0 1))
	(b (ax+b `(4 3) 0 1)))
    (ok (equal-to 4) (elements (proceed (!matmul a b)))))
  (let ((m (proceed (!matmul (ax+b `(3 4) 1 1) (ax+b `(4 3) 1 1)))))
    (ok (every #'= (elements m) #(70 80 90 158 184 210 246 288 330)))))

(deftest broadcast-regression-test
  (ok (every #'= (elements (proceed (!mul (ax+b `(1 10) 1 0) (ax+b `(10 1) 1 0))))
	     #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 2.0 3.0 4.0 5.0 6.0
	       7.0 8.0 9.0 0.0 2.0 4.0 6.0 8.0 10.0 12.0 14.0 16.0 18.0 0.0 3.0
	       6.0 9.0 12.0 15.0 18.0 21.0 24.0 27.0 0.0 4.0 8.0 12.0 16.0 20.0
	       24.0 28.0 32.0 36.0 0.0 5.0 10.0 15.0 20.0 25.0 30.0 35.0 40.0 45.0
	       0.0 6.0 12.0 18.0 24.0 30.0 36.0 42.0 48.0 54.0 0.0 7.0 14.0 21.0
	       28.0 35.0 42.0 49.0 56.0 63.0 0.0 8.0 16.0 24.0 32.0 40.0 48.0 56.0
	       64.0 72.0 0.0 9.0 18.0 27.0 36.0 45.0 54.0 63.0 72.0 81.0))))

(deftest test-wrapped-with
  (testing "Intentionally causes the overflow and check counts are reset (requires to optimize/get work %threefy2x32)"
    (let ((caten/aasm::*wrap-around-mode* t))
      (loop for dtype in `(:uint64 :uint32 :uint16 :uint8 :int64 :int32 :int16 :int8)
	    for ans   in `(1 1 1 1 -9223372036854775807 -2147483647 -32767 -127) do
	(let* ((max (make-tensor `(3 3) :initial-element (dtype/max dtype) :dtype dtype))
	       (one (make-tensor `(3 3) :initial-element 2 :dtype dtype))
	       (val (proceed (!add max one))))
	  (if (and (caten/codegen/backend:jit-mode-p) (not (eql :native (ctx:getenv :BACKEND))))
	      (ok (every (equal-to ans) (elements val)) (format nil "[~a] got ~a, expected ~a." dtype (elements val) ans))
	      (ok (every #'(lambda (x) (not (= x 0))) (elements val)) (format nil "[~a+VM] got ~a, != 0" dtype (elements val)))))))))

(deftest reduction-side-effects
  (testing "A[RealizedBuffer] += B[Lazy or Realized] should be must have a side effect to increase *rng-counter*"
    (let ((a (proceed (make-tensor `(1) :dtype :uint32 :initial-element 1))))
      (ok (every (equal-to 1) (elements a)))
      (proceed (!add a (iconst 1) :reduce t))
      (ok (every (equal-to 2) (elements a)))
      (proceed (!add a (iconst 1) :reduce t))
      (ok (every (equal-to 3) (elements a)))
      (proceed (!add a (iconst 1) :reduce t))
      (ok (every (equal-to 4) (elements a))))))

(defclass TestIndexComponents (Func) nil)
(defmethod forward ((op TestIndexComponents) &rest inputs)
  (let ((out (st "A[~] -> A[~]" ((car inputs)))))
    (setf (tensor-dtype out) :int64)
    out))
(defmethod backward ((op TestIndexComponents) &optional dout) dout)
(defmethod lower ((op TestIndexComponents) &rest inputs)
  (with-context (_ (%index-components (car inputs) (cdr inputs)))))
(defun test-ic (tensor) (apply #'forward (make-instance 'TestIndexComponents) tensor (map 'list #'iconst (shape tensor))))

(deftest regression-test-index-component-lazy-shaped
  (let ((*default-order* :row))
    (ok (every #'= #(0 1 2 3 4 5 6 7 8) (elements (proceed (test-ic (make-tensor `(3 3))))))
	"First, confirm the function works against the normal inputs.")
    (ok (every #'=
	       (elements (pproceed `((a . 4) (b . 5)) (test-ic (make-tensor `(a b)))))
	       (elements (proceed (test-ic (make-tensor `(4 5))))))
	"Does symbolic index-component work?")
    (ok (every #'=
	       (elements (pproceed `((a . 4) (b . 5)) (!sin (!cast (test-ic (make-tensor `(a b))) :float32))))
	       (elements (proceed (!sin (!cast (test-ic (make-tensor `(4 5))) :float32)))))
	"Fused with Unary")))

(deftest threefry2x32
  (testing "Sampling from [0, 1) with setting seed=0, *rng-counter*=0"
    (with-manual-seed (0)
      (let* ((n 100)
	     (first-rand (elements (proceed (!rand `(,n ,n)))))
	     (avg1 (/ (reduce #'+ first-rand) (* n n)))
	     (scnd-rand (elements (proceed (!rand `(,n ,n)))))
	     (avg2 (/ (reduce #'+ scnd-rand) (* n n)))
	     (third-rand (elements (proceed (!rand `(,n ,n)))))
	     (avg3 (/ (reduce #'+ third-rand) (* n n))))
	(ok (< (abs (- avg1 0.5)) 0.01))
	(ok (< (abs (- avg2 0.5)) 0.01))
	(ok (< (abs (- avg3 0.5)) 0.01))
	(ng (some #'= first-rand scnd-rand third-rand))
	(testing "Multiple %threefry2x32 in a single runtime (i.e.: confirm is there really no duplicates in a single compilation.)"
	  (with-manual-seed (0)
	    (let* ((first-rand1 (elements (proceed (!rand `(,n ,n))))))
	      (testing "First, confirm that when we fix *manual-seed* and *rng-counter*, the randomness should be reproduced."
		(ok (every #'= first-rand first-rand1)))
	      (testing "Then, reproduce second/third randomness in a single call of proceed."
		(let* ((second-and-third-rand (elements (proceed (!add (!rand `(,n ,n)) (!rand `(,n ,n)))))))
		  (ok (every #'= (map 'list #'+ scnd-rand third-rand) second-and-third-rand)))))))))))

(deftest threefry2x32-static
  (testing "Sampling from [0, 1) with setting seed=0, *rng-counter*=0"
    (let ((rand (caten (!rand `(1000)))))
      (with-manual-seed (0)
	(let* ((n 1000)
	       (first-rand (elements (forward rand `(n . ,n))))
	       (avg1 (/ (reduce #'+ first-rand) (* n)))
	       (scnd-rand (elements (forward rand `(n . ,n))))
	       (avg2 (/ (reduce #'+ scnd-rand) (* n)))
	       (third-rand (elements (forward rand `(n . ,n))))
	       (avg3 (/ (reduce #'+ third-rand) n)))
	  (progn
	    (ok (< (abs (- avg1 0.5)) 0.1))
	    (ok (< (abs (- avg2 0.5)) 0.1))
	    (ok (< (abs (- avg3 0.5)) 0.1))
	    (ng (some #'= first-rand scnd-rand third-rand))))))))

(deftest threefry2x32-dynamic
  (testing "Sampling from [0, 1) with setting seed=0, *rng-counter*=0"
    (let ((rand (caten (!rand `(n)))))
      (with-manual-seed (0)
	(let* ((n 1000)
	       (first-rand (elements (forward rand `(n . ,n))))
	       (avg1 (/ (reduce #'+ first-rand) (* n)))
	       (scnd-rand (elements (forward rand `(n . ,n))))
	       (avg2 (/ (reduce #'+ scnd-rand) (* n)))
	       (third-rand (elements (forward rand `(n . ,n))))
	       (avg3 (/ (reduce #'+ third-rand) n)))
	  (ok (< (abs (- avg1 0.5)) 0.1))
	  (ok (< (abs (- avg2 0.5)) 0.1))
	  (ok (< (abs (- avg3 0.5)) 0.1))
	  (ng (some #'= first-rand scnd-rand third-rand)))))))

(caten/defun[T] (axpy "axpy" :dtypes (:float32)) (x y n froma toa fromb tob)
  (!add (!view (make-tensor `(,n) :from x) `(,froma ,toa)) (!view (make-tensor `(,n) :from y) `(,fromb ,tob))))

(deftest call-aot
  (let ((a (proceed (ax+b `(3 3) 1 1))) (b (proceed (ax+b `(3 3) 1 1))))
    (ok (every #'= #(2 4 6 8 10 12 14 16 18) (elements (axpy :float32 a b 9 0 9 0 9))))))

(deftest shape-infer-failing-case
  (let ((a (make-tensor `(5 5) :dtype :uint32)))
    (ok (every (equal-to 2) (elements (proceed (!add (!add (iconst 1) (!view a `(0 2) `(0 2))) (!add (iconst 1) (!view a `(2 4) `(2 4))))))))))
;; [TODO] Precision Test
(deftest randn-compile-test
  (ok (proceed (!randn `(10 10)))))

(deftest static-make-tensor-test
  (ok (every #'(lambda (x) (> x 0)) (elements (proceed (!add (rand `(10 10)) (rand `(10 10))))))))

(deftest index-component-regression-test
  (ok (every
       #'=
       #(5.0 7.0 9.0 11.0 13.0)
       (elements (proceed (!add (!index-components (make-tensor `(5))) (!view (!index-components (make-tensor `(10))) `(5 10)))))))
  (ok
   (every
    #'=
    #(25.0 31.0 37.0 43.0 49.0 31.0 37.0 43.0 49.0 55.0 37.0 43.0 49.0 55.0 61.0 43.0 49.0 55.0 61.0 67.0 49.0 55.0 61.0 67.0 73.0)
    (elements (proceed (!add (!index-components (make-tensor `(5 5))) (!t (!view (!index-components (make-tensor `(10 5))) `(5 10) t))))))))

(deftest test-linalg
  (let ((*default-order* :row))
    (ok (every #'= (elements (proceed (!tril (make-tensor `(10 10) :initial-element 1.0))))
               #(1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0
                 0.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0
                 0.0 0.0 1.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0
                 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0
                 1.0 1.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 1.0
                 1.0 1.0 1.0 1.0 1.0)))
    (ok (every #'= (elements (proceed (!triu (make-tensor `(10 10) :initial-element 1.0))))
               #(1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0
                 1.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0
                 1.0 1.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0
                 1.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0
                 0.0 1.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 0.0
                 0.0 0.0 0.0 0.0 1.0)))
    ;; Batched
    (ok (every #'= (elements (proceed (!triu (make-tensor `(5 5 5) :initial-element 1.0))))
               #(1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 0.0 0.0 1.0 1.0 1.0 0.0 0.0 0.0 1.0
                 1.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 0.0 0.0 1.0
                 1.0 1.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 1.0
                 1.0 1.0 1.0 0.0 0.0 1.0 1.0 1.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 1.0 1.0
                 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 0.0 0.0 1.0 1.0 1.0 0.0 0.0 0.0 1.0 1.0
                 0.0 0.0 0.0 0.0 1.0 1.0 1.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 1.0 0.0 0.0 1.0 1.0
                 1.0 0.0 0.0 0.0 1.0 1.0 0.0 0.0 0.0 0.0 1.0)))))

(deftest infinity-test
  (ok (every #'float-features:float-infinity-p (elements (proceed (!full `(3 3) (inf)))))))

(deftest negative-infinity-test
  (ok (every #'(lambda (x) (eql :-inf (float-type-of x))) (elements (proceed (!full `(3 3) (-inf)))))))

(defun isnan (x) (eql (nan) x))
(deftest nan-test
  (ok (every #'isnan (elements (proceed (!full `(3 3) (nan)))))))

(deftest test-float-type-of
  (ok (eql :inf (float-type-of (inf))))
  (ok (eql :-inf (float-type-of (-inf))))
  (ok (eql :nan (float-type-of (nan))))
  (ok (eql t (float-type-of 1.0))))

(deftest test-facet
  (let ((x (proceed (make-tensor `(3 3) :initial-element 1.0))))
    (with-facet (a (x :direction :array))
      (setf (aref a 0 0) 0.0 (aref a 1 1) 0.0 (aref a 2 2) 0.0))
    (with-facet (a (x :direction :simple-array))
      (ok (every #'= a #(0.0 1.0 1.0 1.0 0.0 1.0 1.0 1.0 0.0)))))
  (let ((x (change-facet '(1 2 3 4 5) :tensor)))
    (ok (eql (tensor-dtype x) *default-int*))
    (ok (equal (tensor-shape x) '(5)))
    (ok (every #'= (elements x) '(1 2 3 4 5)))))
