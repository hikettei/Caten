(in-package :cl-user)

(defpackage :caten/air.test
  (:use :cl :rove :caten/air :trivia))

(in-package :caten/air.test)

;; ~~ helpers ~~~~~~~~~~~~~~~~~~~~~~~~~
(defun <node> (type writes reads &rest attrs) (apply #'make-node :Testing type writes reads attrs))
(defun compare (graph expected
		&key
		  (slots `(caten/air::writes caten/air::reads caten/air::type caten/air::class))
		  (shuffle-order t))
  (flet ((eq-node (n1 n2)
	   (and
	    (equal (dump-into-list (node-attr n1)) (dump-into-list (node-attr n2)))
	    (every #'(lambda (slot) (equal (slot-value n1 slot) (slot-value n2 slot))) slots))))
    (if shuffle-order
	(loop for node in (graph-nodes graph)
	      for x = (find node expected :test #'eq-node)
	      if (null x)
		do (error "The node ~a is not appeared in the expected list." node)
	      else
		do (setf expected (remove x expected :test #'eq-node)))
	(return-from compare (every #'eq-node (graph-nodes graph) expected)))
    (if (null expected)
	t
	(error "Nodes ~a is not appeared in the simplified list." expected))))
(defmacro check-simplify (simplifier-name before after &key (shuffle-order t))
  `(compare
    (,simplifier-name
     (apply #'make-graph ,before))
    ,after
    :shuffle-order ,shuffle-order))

(defnode (:Testing :Test-F) () "" :slots ((mode)))
(defnode (:Testing :Test-Mul) () "" :slots ((mode) (mutated)))
(defnode (:Testing :Test-Add) () "")
(defnode (:Testing :Test-Const) () "")
(defnode (:Testing :Test-Sub) () "")
(defnode (:Testing :Test-Sub1) () "")
(defnode (:Testing :Test-X) () "")
(defnode (:Testing :Test-L) () "")
;; ~~ tests ~~~~~~~~~~~~~~~~~~~~~~~~~~
(defpattern number (x) `(guard ,x (numberp ,x)))
(defsimplifier
    (dummy-simplifier-1 :speed 0)
    ;; Attrs
    ((:Test-F ((number x)) :mode m) -> (:Test-Mul (x) :mode m :mutated 1))
    ;; Simple rewrite
    ((:Test-Add ((number x) (number y))) -> (:Test-Const ((+ x y))))
    ;; Nested
    ((:Test-Add ((:Test-Add (x y)) z)) -> (:Test-Add (x y z)))
    ;; ((Node Graph) ...) Notation
    ((:Test-Sub ((number x) (number y))) -> ((node graph) (<node> :Test-Const (node-writes node) (list (- x y)))))
    ((:Test-Sub1 ((number x) (number y))) -> ((node graph) (list (<node> :Test-Const (node-writes node) (list (- x y)))))))

(deftest simplify-test-1
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Test-Add (list 'a 'b) (list 2 3)))
    (list
     (<node> :Test-Const (list 'a 'b) (list 5)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Test-Add (list 'a 'b) (list 2 3))
     (<node> :Test-Add (list 'c 'd) (list 2 3))
     (<node> :Test-X   (list 'k) (list 'a 'b 'c 'd)))
    (list
     (<node> :Test-Const (list 'c 'd) (list 5))
     (<node> :Test-Const (list 'a 'b) (list 5))
     (<node> :Test-X   (list 'k) (list 'a 'b 'c 'd)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Test-F (list 'k) (list 1) :mode 1)
     (<node> :Test-Add (list 'm) (list 1 1))
     (<node> :Test-Add (list 'n) (list 1 1))
     (<node> :Test-X (list 'o) (list 'k 'm 'n)))
    (list
     (<node> :Test-Const (list 'n) (list 2))
     (<node> :Test-Const (list 'm) (list 2))
     (<node> :Test-Mul (list 'k) (list 1) :mode 1 :mutated 1)
     (<node> :Test-X (list 'o) (list 'k 'm 'n)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Test-Add (list 'm) (list 1 1))
     (<node> :Test-Add (list 'n) (list 1 1))
     (<node> :Test-Add (list 'a) (list 'm 'n))
     (<node> :Test-Add (list 'c) (list 'a 'm))
     (<node> :Test-Add (list 'output) (list 'a 'c)))
    (list
     (<node> :Test-Add (list 'c) (list 'm 'n 'm))
     (<node> :Test-Add (list 'output) (list 'm 'n 'c))
     (<node> :Test-Const (list 'n) (list 2))
     (<node> :Test-Const (list 'm) (list 2)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Test-Sub (list 'x 'y) (list 3 5)))
    (list
     (<node> :Test-Const (list 'x 'y) (list -2)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Test-Sub1 (list 'x 'y) (list 3 5)))
    (list
     (<node> :Test-Const (list 'x 'y) (list -2))))))

(defsimplifier
    (match-empty-reads :speed 0)
    ((:Test-F ()) -> (:Test-L ())))

(deftest simplify-test-2
  (ok
   (check-simplify
    match-empty-reads
    (list
     (<node> :Test-F (list 'a) nil))
    (list
     (<node> :Test-L (list 'a) nil)))))
