(in-package :caten/api)
;; lang.lisp: APIs for constructing blueprint


;; flash-attention.lisp的な感じで，Header作ってもらってもいい。caten-toplevelになる

(defmacro <function> (name (&rest args) &body body)
  ""
  nil
  )

;; concepts
#|
(<function> flash-attention (X<float>[A B] Y<float>[A B])
  (for i = (range 0 10 2) do
       (S: x[i] += x[i])
       (S: )
       ))
#|
;; TODO:
;; https://github.com/ruricolist/infix-math
;; TileLangLike
;; FlashAttention Benchmark
