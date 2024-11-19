

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
         (n (first last-two))     ; Sequence length
         (d (second last-two))    ; Embedding size
         (b (if (> (length shape) 2)
                (reduce #'* (butlast shape 2))
                1))               ; Batch size
         (x (!reshape x (list b n d)))
         (positions (!index-components (list n)))
         (freqs (!exp (!div (!mul (!index-components (list (floor d 2)))
                                  (!const x (- (log 10000))))
                            (!const x d))))
         (positions-reshaped (!reshape positions (list n 1)))    ; Shape: (n,1)
         (freqs-reshaped (!reshape freqs (list 1 (floor d 2))))  ; Shape: (1, D/2)
         (theta (!mul positions-reshaped freqs-reshaped))        ; Shape: (n, D/2)
         (costheta (!cos theta))
         (sintheta (!sin theta))
         (x1 (!view x 't 't `(0 ,d 2)))  ; x[..., 0:d:2]
         (x2 (if (evenp d)
                 (!view x 't 't `(1 ,(1+ d) 2))  ; x[..., 1:(d+1):2]
                 (!view x 't 't `(1 ,d 2))))     ; x[..., 1:d:2]
         ;; Compute rx1 and rx2
         (rx1 (!sub (!mul x1 costheta) (!mul x2 sintheta)))
         (rx2 (!add (!mul x1 sintheta) (!mul x2 costheta)))
         (rx1-expanded (!reshape rx1 (append (shape rx1) (list 1))))
         (rx2-expanded (!reshape rx2 (append (shape rx2) (list 1))))
         (result (!concatenate -1 rx1-expanded rx2-expanded))    ; Shape: (b, n, half-dim, 2)
         (rotated (!reshape result (list b n (* 2 (floor d 2)))))
         (final-result (if (evenp d)
                           rotated
                           (let ((last-elem (!view x 't 't `(,(- d 1) ,d))))  ; Shape: (b, n, 1)
                             (!concatenate -1 rotated last-elem)))))
    (proceed (!reshape final-result shape))))