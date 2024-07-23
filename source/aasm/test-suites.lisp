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
		      (b (%view a ',frm ',frm ',to ',by ',bc (%stride ',frm (list 2 1 0)))))
		    ,count))))
      (check (5 5 5) (0 0 0) (5 5 5) (1 1 1) (nil nil nil) 2)
      (check (a b c) (d e f) (g h i) (j k l) (nil nil nil) 48)))
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

(deftest infer-tensor-info
  (macrolet ((ssa-form (&rest form) `(fold-constant (with-context ,@form))))
    (ok
     (let ((g (ssa-form
	       (a (%make-tensor `(3 3)))
	       (b (%reshape a `(9) :id 'X)))))
       (multiple-value-bind (nrank shape stride dtype view)
	   (infer-tensor-info g 'X)
	 (assert (= nrank 1))
	 (assert (equal shape `(9)))
	 (assert (equal stride `(1)))
	 (assert (eql dtype :float32))
	 (assert (equal view `((0) (9) (1) (nil))))
	 t)))
    (ok
     (let ((g (ssa-form
	       (a (%make-tensor `(3 3)))
	       (b (%view a `(3 3) `(0 0) `(3 3) `(1 1) `(nil nil) `(3 1)))
	       (c (%sqrt b :id 'X)))))
       (multiple-value-bind (nrank shape stride dtype view)
	   (infer-tensor-info g 'X)
	 (assert (= nrank 2))
	 (assert (equal shape `(3 3)))
	 (assert (equal stride `(3 1)))
	 (assert (eql dtype :float32))
	 (assert (equal view `((0 0) (3 3) (1 1) (nil nil))))
	 t)))))
