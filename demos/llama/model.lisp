(defpackage :caten/demos.llama.model
  (:use :cl :caten)
  (:import-from :caten/nn
   :Linear
   :!ReLU)
  (:export

   ))
(in-package :caten/demos.llama.model)

(defmodel (Attention (dim n-heads n-kv-heads head-dim rope-traditional rope-theta))
    ((repeats (floor (/ n-heads n-kv-heads)))
     (scales (/ (sqrt head-dim)))
     (wq (Linear dim (* n-heads head-dim) :bias nil))
     (wk (Linear dim (* n-kv-heads head-dim) :bias nil))
     (wv (Linear dim (* n-kv-heads head-dim) :bias nil))
     (wo (Linear (* n-kv-heads head-dim) dim :bias nil))
     (rope (RoPE head-dim :traditional rope-traditional :theta rope-theta))))

(defmodel (FeedForward (dim hidden-dim))
    ((w1 (Linear dim hidden-dim))
     (w2 (Linear hidden-dim dim))
     (w3 (Linear dim hidden-dim))))

(defmethod call ((model FeedForward) &rest inputs)
  (st "X[~] -> X[~]" (inputs))
  (with-slots ((w1 w1) (w2 w2) (w3 w3)) model
    (multiple-value-bind (x) (apply #'values inputs)
      ;; ReLU -> SiLU
      (call w2 (!mul (!softmax (call w1 x)) (call w3 x))))))