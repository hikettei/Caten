(in-package :caten/nn)

;; Ref: https://github.com/tinygrad/tinygrad/blob/master/tinygrad/tensor.py#L1672
;; TODO: Support symbolic by implementing
;; !if !some
;; TODO: test !repeat
(defun _pool (x k_ stride dilation)
  (declare (type Tensor x))
  (assert (>= (ndim x) (length k_)))
  ;; s_, d_ = make_pair(stride, len(k_)), make_pair(dilation, len(k_))
  (multiple-value-bind (s_ d_)
      (values (maybe-list stride k_) (maybe-list dilation k_))
    (assert (= (length s_) (length d_) (length k_)))
    ;;noop_, i_ = [None] * len(self.shape[:-len(k_)]), self.shape[-len(k_):]
    (multiple-value-bind (noop_ noop1_ i_)
	(values
	 (loop repeat (length (butlast (shape x) (length k_))) collect t)
	 (loop for i upfrom 0 below (length (butlast (shape x) (length k_))) collect (nth i (shape x)))
	 (last (shape x) (length k_)))
      ;;o_ = [math.ceil((i - d * (k-1))/s) for i,d,k,s in zip(i_, d_, k_, s_)]
      (let ((o_ (loop for i in i_ for d in d_ for k in k_ for s in s_
		      collect (ceiling (/ (- i (* d (- k 1))) s)))));; TODO: Support symbolic (need !ceiling)
	;; TODO: IfNode and support symbolic.
	;; any(k > s for k,s in zip(k_, s_)) or any(d != 1 for d in d_):
	;; (!if (!or (!some lambda list ...)))
	(if (or (some #'(lambda (x) (not (= x 1))) d_) (some #'> k_ s_))
	    ;; xup = self.repeat([1]*len(noop_) + [math.ceil(k*(i+d) / i) for k,i,d in zip(k_, i_, d_)])
	    (let* ((xup (apply #'!repeat x (append (loop repeat (length noop_) collect 1)
						   (loop for k in k_ for i in i_ for d in d_
							 collect (ceiling (/ (* k (+ i d)) i))))))
		   ;; xup = xup.shrink(tuple(noop_ + [(0,k*(i+d)) for k,i,d in zip(k_, i_, d_)]))
		   (xup (apply #'!view xup (append noop_ (loop for k in k_ for i in i_ for d in d_ collect `(0 ,(* k (+ i d)))))))
		   ;; xup = xup.reshape(noop_ + flatten((k,i+d) for k,i,d in zip(k_, i_, d_)))
		   (xup (!reshape xup (append noop1_ (loop for k in k_ for i in i_ for d in d_ append `(,k ,(+ i d))))))
		   ;; xup = xup.shrink(noop_ + flatten(((0,k), (0,o*s)) for k,o,s in zip(k_, o_, s_)))
		   (xup (apply #'!view xup (append noop_ (loop for k in k_ for o in o_ for s in s_ append (list (list 0 k) (list 0 (* o s)))))))
		   ;; xup = xup.reshape(noop_ + flatten((k,o,s) for k,o,s in zip(k_, o_, s_)))
		   (xup (!reshape xup (append noop1_ (loop for k in k_ for o in o_ for s in s_ append (list k o s)))))
		   ;; xup = xup.shrink(noop_ + flatten(((0,k), (0,o), (0,1)) for k,o in zip(k_, o_)))
		   (xup (apply #'!view xup (append noop_ (loop for k in k_ for o in o_ append (list (list 0 k) (list 0 o) (list 0 1))))))
		   ;; xup = xup.reshape(noop_ + flatten((k,o) for k,o in zip(k_, o_)))
		   (xup (!reshape xup (append noop1_ (loop for k in k_ for o in o_ append (list k o))))))
	      ;; xup.permute(*range(len(noop_)), *[len(noop_)+i*2+1 for i in range(len(i_))], *[len(noop_)+i*2 for i in range(len(i_))])
	      ;; Return: [N in_channels, o_, kernel_size]
	      (!permute xup (append (range 0 (length noop_)) (loop for _ in i_ for i upfrom 0 collect (+ 1 (length noop_) (* i 2))) (loop for _ in i_ for i upfrom 0 collect (+ (length noop_) (* i 2))))))
	    (progn
	      ;; xup = self.pad(tuple(noop_ + [(0, max(0,o*s-i)) for i,o,s in zip(i_, o_, s_)]))
	      (let* ((xup (!padding x (append noop_ (loop for i in i_ for o in o_ for s in s_ collect (list 0 (* o (- s i)))))))
		     ;; xup = xup.shrink(tuple(noop_ + [(0,o*s) for o,s in zip(o_, s_)]))
		     (xup (apply #'!view xup (append (loop for o in o_ for s in s_ collect (list 0 (* s o))))))
		     ;; xup = xup.reshape(noop_ + flatten(((o,s) for o,s in zip(o_, s_))))
		     (xup (!reshape xup (append noop1_ (loop for o in o_ for s in s_ collect (list o s)))))
		     ;; xup = xup.shrink(noop_ + flatten(((0,o), (0,k)) for o,k in zip(o_, k_)))
		     (xup (apply #'!view xup (append noop_ (loop for o in o_ for k in k_ append (list (list 0 o) (list 0 k)))))))
		(!permute xup (append (range 0 (length noop_)) (loop for _ in i_ for i upfrom 0 collect (+ (length noop_) (* i 2))) (loop for _ in i_ for i upfrom 0 collect (+ 1 (length noop_) (* i 2))))))))))))

(in-package :caten/nn.test)

(deftest pooling-test
  (let ((caten/aasm:*default-order* :row))
    ;; TODO: Counting the number of kernels
    (ok (every #'= (elements (proceed (!sin (caten/nn:!padding (make-tensor `(10 10) :initial-element 2.0) `((2 2) (2 2)) :value 1.0)))))
	#(0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096
	  0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096
	  0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096
	  0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.9092974 0.9092974 0.9092974 0.9092974 0.9092974
	  0.9092974 0.9092974 0.9092974 0.9092974 0.9092974 0.84147096 0.84147096
	  0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096
	  0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096
	  0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096
	  0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096 0.84147096))))

