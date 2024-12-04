(in-package :caten/nn)

;; Ref: https://github.com/tinygrad/tinygrad/blob/master/tinygrad/tensor.py#L1672
(defun _pool (x k_ stride dilation &key (ceiling #'ceiling))
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
		      collect (funcall ceiling (/ (- i (* d (- k 1))) s))))) ;; TODO: Support symbolic (need !ceiling)
        (let* ((xup (apply #'!repeat x (append (loop repeat (length noop_) collect 1)
					       (loop for k in k_ for i in i_ for d in d_
						     collect (funcall ceiling (/ (* k (+ i d)) i))))))
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
	  (!permute xup (append (range 0 (length noop_)) (loop for _ in i_ for i upfrom 0 collect (+ 1 (length noop_) (* i 2))) (loop for _ in i_ for i upfrom 0 collect (+ (length noop_) (* i 2))))))))))

(defun make-pooling (x f kernel-size &key (stride nil) (dilation 1) (padding 0) (pad-value 0.0) (ceiling #'ceiling))
  (declare (type Tensor x) (type function f))
  (let* ((k_ (if (integerp kernel-size) (list kernel-size kernel-size) kernel-size))
         (x (!padding2d x (padding2d-shape padding (length k_)) :value pad-value))
         (x (_pool x k_ (or stride k_) dilation :ceiling ceiling))
         (axis (map 'list #'- (range 1 (1+ (length k_)))))
         (reduced-shape (butlast (shape x) (length k_))))
    (!reshape (funcall f x :axis axis) reduced-shape)))
;; ~~ apis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodel (AvgPool (kernel-size &key (stride nil) (dilation 1) (padding 0) (ceiling #'ceiling)))
    ((kernel-size kernel-size)
     (stride stride)
     (dilation dilation)
     (padding padding)
     (ceiling ceiling)))

(defmethod call ((pool AvgPool) &rest inputs)
  (with-attrs ((kernel-size :kernel-size) (stride :stride) (dilation :dilation) (padding :padding) (ceiling :ceiling)) pool
    (make-pooling (car inputs) #'!mean kernel-size :stride stride :dilation dilation :padding padding :ceiling ceiling)))

(defun !avgpool (x &key (kernel-size `(2 2)) (stride nil) (dilation 1) (padding 0) (ceiling #'ceiling))
  "Applies average pooling over the tensor."
  (declare (type list kernel-size))
  (forward (AvgPool kernel-size :stride stride :dilation dilation :padding padding :ceiling ceiling) x))

(defmodel (MaxPool (kernel-size &key (stride nil) (dilation 1) (padding 0) (ceiling #'ceiling)))
    ((kernel-size kernel-size)
     (stride stride)
     (dilation dilation)
     (padding padding)
     (ceiling ceiling)))

(defmethod call ((pool MaxPool) &rest inputs)
  (with-attrs ((kernel-size :kernel-size) (stride :stride) (dilation :dilation) (padding :padding) (ceiling :ceiling)) pool
    (make-pooling (car inputs) #'!max kernel-size :stride stride :dilation dilation :padding padding :ceiling ceiling :pad-value (-inf))))

(defun !maxpool (x &key (kernel-size `(2 2)) (stride nil) (dilation 1) (padding 0) (ceiling #'ceiling))
  "Applies max pooling over the tensor."
  (declare (type list kernel-size))
  (forward (MaxPool kernel-size :stride stride :dilation dilation :padding padding :ceiling ceiling) x))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(in-package :caten/nn.test)
