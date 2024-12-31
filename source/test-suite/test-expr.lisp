(defpackage :caten/test-suite/expr
  (:documentation "A test suite for caten/codegen/expr and gflops computation.")
  (:use :cl :rove :caten/codegen/expr :caten/runtime))
(in-package :caten/test-suite/expr)
;; 1. expr-realize
;; 2. gflops test from the matmul schedule
(deftest test-expr-simplification
  ;; [TODO] More Simplification case if exists
  (ok (= 1 (buffer-value (expr-realize (expr-add (expr-const 1 :int64) (expr-const 0 :int64)))))))
