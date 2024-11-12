(in-package :caten/nn)

; from https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/positional_encoding.py
;; RoPE
;; PositionalEncoding

(defmodel (RoPE (dims &key (traditional NIL) (base 10000) (scale 1.0)))
  ((dims dims)
   (traditional traditional)
   (base base)
   (scale scale)))

;RoPE needs an extra arg in the call function, the generic doesn't consider it
;offset: int = 0
(defmethod call ((op RoPE) &rest inputs)
  (let* ((x (car inputs))
         (shape (shape x))
         (last-two (last shape 2))
         (n (first last-two))
         (d (second last-two)))
    (dotimes (i 10)
      (format t "i = ~a~%" i)
      (defparameter shape (shape x))
      (defparameter x (!reshape x (list n d)))
      (defparameter positions (range 0 n )) ;not sure if this is what mlx doing (mx.arange(n))
      (print positions)
      )))

(let ((instance (make-instance 'RoPE)))
  (call instance *tensor1*))

(in-package :caten/nn.test)


(defun test-rope-tensor ()
  (with-no-grad
    (let ((input-tensor *tensor1*))
      (print input-tensor))))

