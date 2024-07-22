(in-package :cl-user)

(defpackage :caten/air.test
  (:use :cl :rove :caten/air))

(in-package :caten/air.test)

(deftest o
  (ok (= 1 1)))

