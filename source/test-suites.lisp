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
    ;; A[4:0:-1]
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

(deftest test-simplifier
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
    ;;(ok (signals (proceed (!neg (!add (iconst 0) (iconst 'a))))))
    ;; a dependency is purged
    (ok (= 0 (elements (proceed (!neg (!mul (iconst 0) (iconst 'a)))))))
    ))

;; 'A 'B Shape Test
;; TODO: Testing
;; - make-tensor, initial-element
;; - print, viewed-print, broadcasted-print
;; - simple chain rule tests
;; - broadcast testing (esp: (make-tensor `(1)) and (make-tensor `(1)))
;; - scalar and matrix ops
;; - nested module testing
;; - view tesitng
;; - composed two view testing
;; - dynamic shape testing
;; - dynamic shape viewing testing
;; - ULP絡めたテストは外でやる？
