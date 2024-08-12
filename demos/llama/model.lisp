(defpackage :caten/demos.llama.model
  (:use :cl :caten)
  (:import-from :caten/nn
   :Linear
   :!ReLU)
  (:export

   ))
(in-package :caten/demos.llama.model)

;;(defmodel (Attention ))

(defmodel (FeedForward (dim hidden-dim))
    ((w1 (Linear dim hidden-dim))
     (w2 (Linear hidden-dim dim))
     (w3 (Linear dim hidden-dim))))

(defmethod call ((model FeedForward) &rest inputs)
  (st "X[~] -> X[~]" (inputs))
  (with-slots ((w1 w1) (w2 w2) (w3 w3)) model
    (multiple-value-bind (x) (apply #'values inputs)
      ;; ReLU -> SiLU
      (call w2 (!mul (!relu (call w1 x)) (call w3 x))))))
