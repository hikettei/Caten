(defpackage :caten/test-suite/api/test-shape
  (:use :cl :caten/api :caten/ir :caten/graph :trivia :rove))
(in-package :caten/test-suite/api/test-shape)

;; UnaryOps
;; (!reshape x `(20 20))
;; (!sin x) ---> ここにBinaryが挿入されない限り，最初のReshapeはExtra
;; (!reshape x `(20 20))

;; Testing View Simplification Case
(deftest simplify-unary-view-chain
  (let ((xa (make-tensor-relay `(10 10) `(10 1) :float32 `((0 1) (0 1))))
        (ya (make-tensor-relay `(10 1 10) `(10 0 1) :float32 `((0 1) (0 1) (0 1))))
        (g (tensor-graph (make-tensor `(3 3)))))
    (caten/api::search-merged-view g xa ya))
  (let ((xa (make-tensor-relay `(27) `(1) :float32 `((0 1))))
        (ya (make-tensor-relay `(3 3 3) `(9 3 1) :float32 `((0 1) (0 1) (0 1))))
        (g (tensor-graph (make-tensor `(3 3)))))
    (caten/api::search-merged-view g xa ya))
  (let ((xa (make-tensor-relay `(3 3 3) `(9 3 1) :float32 `((0 1) (0 1) (0 1))))
        (ya (make-tensor-relay `(27) `(1) :float32 `((0 1))))
        (g (tensor-graph (make-tensor `(3 3)))))
    (caten/api::search-merged-view g xa ya)))
;; Testing Polyhedral Shape Tracker Fusion In Advance

;; Testing Codegen Fusion
