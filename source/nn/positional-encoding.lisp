(in-package :caten/nn)

; from https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/positional_encoding.py
;; RoPE
;; PositionalEncoding

(defmethod call ((op RoPE) &rest inputs)
  (let* ((x (car inputs))
         (shape (shape x))
         (last-two (last shape 2))
         (n (first last-two))
         (d (second last-two)))
    (dotimes (i 1)
      (format t "i = ~a~%" i)
      (let* ((shape (shape x))
             (b (reduce #'* ( butlast shape 2)))
             (x (!reshape x (list b n d)))
             (positions (!index-components (list n)))
              ;TODO: handle potential divisions by 0
             (freqs (!exp (!div (!index-components (list (floor d 2))) (!const x (log (- (floor 10000 d) 1))))))
             (theta (!reshape positions (list 1 n)))
             (costheta (!cos theta))
             (sintheta (!sin theta))
             )
        (print (proceed freqs))
        ))))



(defparameter *tensor1* (make-tensor `(4 6 8) :initial-element 1.0))

(let ((instance (make-instance 'RoPE)))
  (call instance *tensor1*))



(defparameter *tensor* (make-tensor '(2 3 4) :initial-element 1.0))

(defparameter *positions* (!index-components *tensor*))


(print (reduce #'* ( butlast '(1 2 3 4 5) 2)))  ; Returns 120
(print (proceed *positions*))


(defparameter *reshaped-positions* (!reshape *positions* '(6 4)))


(print (proceed *reshaped-positions*))
(defparameter *freqs* (make-tensor '(1 2) :initial-element 2.0))

(defparameter *theta* (!mul *reshaped-positions* *freqs*))


(print (proceed *theta*))


(setq shape '(5))

;; Call !index-components on the shape
(print (proceed (!index-components shape)))

(print (proceed (!const *tensor1* 2)))
(!index-components (list (shape *tensor1*)))

(in-package :caten/nn.test)



(defun test-rope-tensor ()
  (with-no-grad
    (let ((input-tensor *tensor1*))
      (print input-tensor))))

