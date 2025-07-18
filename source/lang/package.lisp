(defpackage :caten/lang
  (:use :cl))
(in-package :caten/lang)
;; lang.lisp: APIs for constructing blueprint

;; Workload
;; - 1. Implement caten/lang
;; - 2. Implement decent tests (for beam search!) using caten/lang and scheduler
;; flash-attention.lisp的な感じで，Header作ってもらってもいい。caten-toplevelになる
(defmacro <function> (name (&rest args) &body body)
  nil
  )

;; concepts

(<function> (X<float>[A B] Y<float>[A B])
    (For i = (Range 0 A 2)
     (For j = (Range 0 B 2)
          (State x[i j] = sin(y[i j])))))

(<function> (X<float>[A B])
  (For x = (Range 0 A)
       (State acc += X[A])))
;; TODO:
;; https://github.com/ruricolist/infix-math
;; TileLangLike
;; FlashAttention Benchmark
;; Reduction Resolver
;; Create caten/lang
  ;; Not in References
  ;;
