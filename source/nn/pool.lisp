(in-package :caten/nn)

;; Ref: https://github.com/tinygrad/tinygrad/blob/master/tinygrad/tensor.py#L1672
;; TODO: Support symbolic by implementing
;; !if !some
;; TODO: test !repeat
(defun pool (x k_ stride dilation)
  (declare (type Tensor x))
  (assert (>= (ndim x) (length k_)))
  ;; s_, d_ = make_pair(stride, len(k_)), make_pair(dilation, len(k_))
  (multiple-value-bind (s_ d_)
      (values (cons stride (length k_)) (cons dilation (length k_)))
    (assert (= (length s_) (length d_) (length k_)))
    ;;noop_, i_ = [None] * len(self.shape[:-len(k_)]), self.shape[-len(k_):]
    (multiple-value-bind (noop_ i_)
	(values
	 (loop repeat (length (slice (shape x) 0 (- (length k_)))) collect nil)
	 (slice (shape x) (- (length k_))))
      ;;o_ = [math.ceil((i - d * (k-1))/s) for i,d,k,s in zip(i_, d_, k_, s_)]
      (let ((o_ (loop for i in i_ for d in d_ for k in k_ for s in s_
		      collect (ceiling (/ (- i (* d (- k 1)) s))))));; TODO: Support symbolic (need !ceiling)

	;; TODO: IfNode and support symbolic.
	;; any(k > s for k,s in zip(k_, s_)) or any(d != 1 for d in d_):
	;; (!if (!or (!some lambda list ...)))
	(if (or (some #'(lambda (x) (not (= x 1))) d_) (some #'> k_ s_))
	    ;; xup = self.repeat([1]*len(noop_) + [math.ceil(k*(i+d) / i) for k,i,d in zip(k_, i_, d_)])
	    (let* ((xup (apply #'!repeat (loop repeat (length noop_) collect 1)
			       (loop for k in k_ for i in i_ for d in d_
				     collect (ceiling (/ (* k (+ i d)) i)))))
		   ;; xup = xup.shrink(tuple(noop_ + [(0,k*(i+d)) for k,i,d in zip(k_, i_, d_)])).reshape(noop_ + flatten((k,i+d) for k,i,d in zip(k_, i_, d_)))
		   (xup))

	      )
	    (progn
	      
	      ))))))
