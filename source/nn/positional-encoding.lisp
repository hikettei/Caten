(in-package :caten/nn)

; from https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/positional_encoding.py
;; RoPE
;; PositionalEncoding

(defmodel (RoPE (dims &key (traditional NIL) (base 10000) (scale 1.0) (offset 1.0)))
    ((dims dims)
     (traditional traditional)
     (base base)
     (scale scale)
     (offset offset)))



(defmethod call ((op RoPE) &rest inputs)
  (let* ((x (car inputs))
         (shape (shape x))
         (last-two (last shape 2))
         (n (first last-two))
         (d (second last-two))
         (d-minus-1 (1- d)))
    (dotimes (i 1)
      (let* ((shape (shape x))
             (b (reduce #'* (butlast shape 2)))
             (x (!reshape x (list b n d)))
             (positions (!index-components (list n)))
             ;TODO: handle potential divisions by 0
             (freqs (!exp (!div (!index-components (list (floor d 2))) 
                                (!const x (log (- (floor 10000 d) 1))))))
             (theta (!reshape positions (list 1 n)))
             (costheta (!cos theta))
             (sintheta (!sin theta))
             (x1 (!view x t t '(0 d 2)))
             (x2 (!view x t t '(1 d-minus-1 2)))
             )
        (format t "~%Original x shape: ~A" (shape x))
        (format t "~%X1 (even) shape: ~A" (shape x1))
        (format t "~%X2 (odd) shape: ~A" (shape x2))
        (format t "~%Freqs: ~A" (proceed freqs))
        (values x1 x2 costheta sintheta)))))



(defparameter *tensor1* (make-tensor '(14 10 20) :initial-element 1.0))
(let ((instance (make-instance 'RoPE)))
  (call instance *tensor1*))

(in-package :caten/nn.test)



(defun test-rope-tensor ()
  (with-no-grad
    (let ((input-tensor *tensor1*))
      (print input-tensor))))

