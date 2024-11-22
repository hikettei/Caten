(in-package :caten/nn)

;; RoPE
;; PositionalEncoding

(defmodel (RoPE (dim &key (base 10000)))
    ((dim dim)
     (base base)))

(defmethod call ((op RoPE) &rest inputs)
  (with-slots (dim base) op
    (let* ((x (first inputs))
           (x-shape (shape x))  ; x-shape: (batch-size, seq-len, num-heads, head-dim)
           (batch-size (nth 0 x-shape))
           (seq-len (nth 1 x-shape))
           (num-heads (nth 2 x-shape))
           (head-dim (nth 3 x-shape)))
      ;; Validate that dim matches head_dim / 2
      (when (/= dim (/ head-dim 2))
        (error "Mismatch: Provided dim (~A) does not match head-dim / 2 (~A)" dim (/ head-dim 2)))
      (let* ((indices (!mul (!const x 2) (!index-components (list dim))))  ; Shape: (dim)
             (exponents (!div indices (!const x head-dim)))                ; Shape: (dim)
             (base (!const x base))
             (log-base (!log base))                                       ; Natural logarithm
             (theta (!exp (!neg (!mul exponents log-base))))              ; Shape: (dim)
             (theta-reshaped (!reshape theta (list 1 dim)))               ; Shape: (1, dim)
             (seq-idx (!index-components (list seq-len)))                 ; Shape: (seq-len)
             (seq-idx-reshaped (!reshape seq-idx (list seq-len 1)))       ; Shape: (seq-len, 1)
             (idx-theta (!mul seq-idx-reshaped theta-reshaped))           ; Shape: (seq-len, dim)
             (cosine (!cos idx-theta))                                    ; Shape: (seq-len, dim)
             (sine (!sin idx-theta))                                      ; Shape: (seq-len, dim)
             (xshaped (!reshape x (list batch-size seq-len num-heads dim 2)))
             (cosine-reshaped (!reshape cosine (list 1 seq-len 1 dim 1)))
             (sine-reshaped (!reshape sine (list 1 seq-len 1 dim 1)))
             (num-dimensions 5)
             (x0-subscripts (make-list num-dimensions :initial-element 'T))
             (x1-subscripts (make-list num-dimensions :initial-element 'T)))
        (setf (nth (- num-dimensions 1) x0-subscripts) '(0 1)) ; Select index 0
        (setf (nth (- num-dimensions 1) x1-subscripts) '(1 2)) ; Select index 1
        (let* ((x0 (apply #'!view xshaped x0-subscripts))
               (x1 (apply #'!view xshaped x1-subscripts))
               (rotated0 (!sub (!mul x0 cosine-reshaped) (!mul x1 sine-reshaped)))
               (rotated1 (!add (!mul x0 sine-reshaped) (!mul x1 cosine-reshaped)))
               (x-out (!concatenate -1 rotated0 rotated1))
               (x-out-final (!reshape x-out (list batch-size seq-len num-heads (* 2 dim)))))
          (let ((final-result
                  (if (= (* 2 dim) head-dim)
                      x-out-final
                      (let* ((x-dims (shape x))
                             (subs (make-list (length x-dims) :initial-element 'T)))
                        (setf (nth (- (length x-dims) 1) subs) (list (- head-dim 1) head-dim))
                        (let ((last-elem (apply #'!view x subs))) ; Shape: (batch-size, seq-len, num-heads, 1)
                          (!concatenate -1 x-out-final last-elem))))))
            (!reshape final-result x-shape)))))))

(defun !rope (x dim)
  (declare (type tensor x))
  (forward (rope dim) x))