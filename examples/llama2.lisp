

(ql:quickload '(:caten :caten/apps))
(defpackage :llama2-inference
  (:use :cl :caten/apps.llama2))
(in-package :llama2-inference)


(in-package :caten/test-suite) ;; placeholder for now

(print (proceed (forward (FeedForwardLLAMA 10 1) (make-tensor `(1 10)))))



