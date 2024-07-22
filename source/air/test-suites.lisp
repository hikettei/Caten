(in-package :cl-user)

(defpackage :caten/air.test
  (:use :cl :rove :caten/air :trivia))

(in-package :caten/air.test)

;; ~~ helpers ~~~~~~~~~~~~~~~~~~~~~~~~~
(defun <node> (type writes reads &rest attrs) (apply #'make-node :node type writes reads attrs))
(defun compare (graph expected &key (slots `(caten/air::writes caten/air::reads caten/air::attrs caten/air::type caten/air::class)))
  (flet ((eq-node (n1 n2)
	   (every #'(lambda (slot) (equal (slot-value n1 slot) (slot-value n2 slot))) slots)))
    (every #'eq-node (graph-nodes graph) expected)))
(defmacro check-simplify (simplifier-name before after)
  `(compare
    (,simplifier-name
     (apply #'make-graph ,before))
    ,after))
;; ~~ tests ~~~~~~~~~~~~~~~~~~~~~~~~~~
(defpattern number (x) `(guard ,x (numberp ,x)))
(defsimplifier
    (dummy-simplifier-1 :speed 0)
    ;; Attrs
    ((:F ((number x)) :mode m) -> (:Mul (x) :mode m :mutated 1))
    ;; Simple rewrite
    ((:Add ((number x) (number y))) -> (:Const ((+ x y))))
    ;; Nested
    ((:Add ((:Add (x y)) z)) -> (:Add (x y z)))
    ;; ((Node Graph) ...) Notation
    ((:Sub ((number x) (number y))) -> ((node graph) (<node> :Const (node-writes node) (list (- x y)))))
    ((:Sub1 ((number x) (number y))) -> ((node graph) (list (<node> :Const (node-writes node) (list (- x y)))))))

(deftest simplify-test-1
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Add (list 'a 'b) (list 2 3)))
    (list
     (<node> :Const (list 'a 'b) (list 5)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Add (list 'a 'b) (list 2 3))
     (<node> :Add (list 'c 'd) (list 2 3)))
    (list
     (<node> :Const (list 'c 'd) (list 5))
     (<node> :Const (list 'a 'b) (list 5)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :F (list 'k) (list 1) :mode 1)
     (<node> :Add (list 'm) (list 1 1))
     (<node> :Add (list 'n) (list 1 1)))
    (list
     (<node> :const (list 'n) (list 2))
     (<node> :const (list 'm) (list 2))
     (<node> :mul (list 'k) (list 1) :mode 1 :mutated 1))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Add (list 'm) (list 1 1))
     (<node> :Add (list 'n) (list 1 1))
     (<node> :Add (list 'a) (list 'm 'n))
     (<node> :Add (list 'b) (list 'a 'b))
     (<node> :Add (list 'output) (list 'a 'b)))
    (list
     (<node> :add (list 'b) (list 'm 'n 'b))
     (<node> :const (list 'n) (list 2))
     (<node> :const (list 'm) (list 2))
     (<node> :add (list 'output) (list 'a 'b)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Sub (list 'x 'y) (list 3 5)))
    (list
     (<node> :const (list 'x 'y) (list -2)))))
  (ok
   (check-simplify
    dummy-simplifier-1
    (list
     (<node> :Sub1 (list 'x 'y) (list 3 5)))
    (list
     (<node> :const (list 'x 'y) (list -2))))))
