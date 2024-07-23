(in-package :cl-user)

(defpackage :caten/aasm.test
  (:use :cl :rove :caten/air :caten/aasm))

(in-package :caten/aasm.test)

(defun check-schedule (graph count)
  (declare (type graph graph)
	   (type fixnum count))
  (let ((sched (fold-constant graph)))
    (assert
     (= (length (graph-nodes sched)) count)
     ()
     "check-schedule: should satisfy (kernel_count=~a) <= ~a.~%~a" (length (graph-nodes sched)) count sched))
  t)

(deftest constant-folding
  (testing "Tensor Creation (Fixed Shape/Dynamic Shape)"
    (macrolet ((check (s1 s2 count)
		 `(ok
		   (check-schedule
		    (with-context
		      (a (%make-tensor ',s1))
		      (b (%make-tensor ',s2))
		      (c (%add a b)))
		    ,count))))
      (check (3 3) (3 3) 3)
      (check (a b) (3 3) 12)
      (check (3 3) (a b) 12)
      (check (a 3) (3 3) 5)))
  (testing "Tensor Creation (w/o constant folding vs worst case)"
    (macrolet ((make (s1 s2)
		 `(with-context
		    (a (%make-tensor ',s1))
		    (b (%make-tensor ',s2))
		    (c (%add a b)))))
      ;; Needs to be updated if we fold symbols
      (ok (= (length (graph-nodes (fold-constant (make (a b) (a b)))))
	     (length (graph-nodes (make (a b) (a b))))
	     (length (graph-nodes (make (3 3) (3 3))))))))
  (testing "View Creation"
    (macrolet ((check (s1 frm to by bc count)
		 `(ok
		   (check-schedule
		    (with-context
		      (a (%make-tensor ',s1))
		      (b (%view a ',frm ',to ',by ',bc)))
		    ,count))))
      (check (5 5 5) (0 0 0) (5 5 5) (1 1 1) (nil nil nil) 2)
      (check (a b c) (d e f) (g h i) (j k l) (nil nil nil) 34)))
  (testing "Reshape Creation"
    (macrolet ((check (s1 s2 count)
		 `(ok
		   (check-schedule
		    (with-context
		      (a (%make-tensor ',s1))
		      (b (%reshape a ',s2)))
		    ,count))))
      (check (1 2 3) (6) 2)
      (check (1 2 3) (d) 4)
      (check (a b c) (d) 18))))
