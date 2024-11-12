(in-package :caten/nn)

; from https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/positional_encoding.py
;; RoPE
;; PositionalEncoding

(defmodel (RoPE (dims &key (traditional NIL) (base 10000) (scale 1.0)))
  ((dims dims)
   (traditional traditional)
   (base base)
   (scale scale)))

(defmethod call ((op RoPE) &rest inputs)
  (let* ((x (car inputs))
         (shape (shape x))
         (last-two (last shape 2))
         (n (first last-two))
         (d (second last-two)))
    (dotimes (i 10)
      (format t "i = ~a~%" i))))

(defparameter *tensor1* (make-tensor `(3 4 3) :initial-element 1.0))

(let ((instance (make-instance 'RoPE)))
  (call instance *tensor1*))

(in-package :caten/nn.test)


(defun test-rope-tensor ()
  (with-no-grad
    (let ((input-tensor *tensor1*))
      (print input-tensor))))

