(defpackage :caten/test-suite/api/test-shape
  (:use :cl :caten/graph :trivia :rove))
(in-package :caten/test-suite/api/test-shape)

;; UnaryOps
;; (!reshape x `(20 20))
;; (!sin x) ---> ここにBinaryが挿入されない限り，最初のReshapeはExtra
;; (!reshape x `(20 20))

;; Testing View Simplification Case
(deftest simplify-unary-view-chain
  
  )
;; Testing Polyhedral Shape Tracker Fusion In Advance

;; Testing Codegen Fusion
