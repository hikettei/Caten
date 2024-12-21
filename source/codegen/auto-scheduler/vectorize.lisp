(defpackage :caten/codegen/vectorize
  (:use :cl))

(in-package :caten/codegen/vectorize)

(defgeneric %mutate-vectorize (op))

;; Algorithm
;; Create a map of expected vectorize, if blueprint has a sequence of unrolled :EXPR
;; (_gid0 . 0) (_gid1 . 1) (_gid2 . 2) (_gid3 . 3) (_gid4 . 4) ...
;;                               ...
;; Priority:
;; 2D Vectorize
;; 1D Vectorize
;; - [ ] EXPR has a unroll history
