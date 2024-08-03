(in-package :cl-user)

(defpackage :caten/ajit.test
  (:use :cl :rove :caten/air :trivia :caten/ajit))

(in-package :caten/ajit.test)

(deftest iconstraint-test
  (testing "Initializing Constraint"
    (ok (string= "m <= a < n" (form (make-iconstraint 'a 'm 'n)))))
 ; (testing "Initializing IUnion"
  ;  (ok (string=
    )

