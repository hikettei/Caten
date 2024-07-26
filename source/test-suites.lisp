(in-package :cl-user)
(defpackage :caten.test (:use :cl :rove :caten))
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
    (test :~ 0 100 1 t)))

(deftest test-auto-cast
  (flet ((test (dtype il)
	   (caten/avm:%realize (caten::%tensor->aasm (!add (make-tensor `(3 3) :dtype dtype) (make-tensor `(3 3) :dtype dtype :initial-element il))))))
    (testing "fconst(1) should be valid, iconst(1.0) should be invaild"
      ;;(test :float16 1)
      (ok (test :float32 1))
      (ok (test :float64 1))
      (dolist (dtype `(:uint64 :int64 :uint32 :int32 :uint16 :int16 :uint8 :int8))
	(signals (test dtype 1.0))))))
