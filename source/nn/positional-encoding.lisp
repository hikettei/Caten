(in-package :caten/nn)

;; RoPE
;; PositionalEncoding

(defmodel (RoPE (dim &key (base 10000)))
    ((dim dim)
     (base base)))

(defmethod call ((op RoPE) &rest inputs)
  (with-slots (dim base) op
    (let* ((x (first inputs))
         (x-shape (shape x)) ; x-shape: (batch-size, seq-len, num-heads, head-dim)
         (batch-size (nth 0 x-shape))
         (seq-len (nth 1 x-shape))
         (num-heads (nth 2 x-shape))
         (head-dim (nth 3 x-shape))
         (head-dim-half (floor (/ head-dim 2)))
         (indices (!mul (!const x 2) (!index-components (list head-dim-half)))) ; Shape: (head-dim-half)
         (exponents (!div indices (!const x head-dim))) ; Shape: (head-dim-half)
         (base (!const x base))
         (log-base (!log base)) ; Natural logarithm
         (theta (!exp (!neg (!mul exponents log-base)))) ; Shape: (head-dim-half)
         (theta-reshaped (!reshape theta (list 1 head-dim-half))) ; Shape: (1, head-dim-half)
         (seq-idx (!index-components (list seq-len))) ; Shape: (seq-len)
         (seq-idx-reshaped (!reshape seq-idx (list seq-len 1))) ; Shape: (seq-len, 1)
         (idx-theta (!mul seq-idx-reshaped theta-reshaped)) ; Shape: (seq-len, head-dim-half)
         (cosine (!cos idx-theta)) ; Shape: (seq-len, head-dim-half)
         (sine (!sin idx-theta))   ; Shape: (seq-len, head-dim-half)
         (xshaped (!reshape x (list batch-size seq-len num-heads head-dim-half 2)))
         (cosine-reshaped (!reshape cosine (list 1 seq-len 1 head-dim-half 1)))
         (sine-reshaped (!reshape sine (list 1 seq-len 1 head-dim-half 1)))
         (num-dimensions 5)
         (x0-subscripts (make-list num-dimensions :initial-element 'T))
         (x1-subscripts (make-list num-dimensions :initial-element 'T)))
    (setf (nth (- num-dimensions 1) x0-subscripts) '(0 1)) ; Select index 0
    (setf (nth (- num-dimensions 1) x1-subscripts) '(1 2)) ; Select index 1
    (let* ((x0 (apply #'!view xshaped x0-subscripts))
           (x1 (apply #'!view xshaped x1-subscripts))
           (rotated0 (!sub (!mul x0 cosine-reshaped) (!mul x1 sine-reshaped))) ; Shape: same as x0
           (rotated1 (!add (!mul x0 sine-reshaped) (!mul x1 cosine-reshaped))) ; Shape: same as x0
           (x-out (!concatenate -1 rotated0 rotated1)) 
           (x-out-final (!reshape x-out (list batch-size seq-len num-heads (* 2 head-dim-half)))))
      (let ((final-result (if (= (* 2 head-dim-half) head-dim)
                              x-out-final
                              (let* ((x-dims (shape x))
                                     (subs (make-list (length x-dims) :initial-element 'T)))
                                (setf (nth (- (length x-dims) 1) subs) (list (- head-dim 1) head-dim))
                                (let ((last-elem (apply #'!view x subs))) ; Shape: (batch-size, seq-len, num-heads, 1)
                                  (!concatenate -1 x-out-final last-elem))))))
        (!reshape final-result x-shape))))))



(defun !rope (x dim)
  (declare (type tensor x))
  (forward (rope dim) x))
