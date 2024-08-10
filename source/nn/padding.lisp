(in-package :caten/nn)

(defun padding2d-shape (padding dims)
  (declare (type (or integer list) padding)
	   (type fixnum dims))
  (if (integerp padding)
      (loop repeat (* 2 dims) collect padding)
      (if (= (length padding) (* 2 dims))
	  padding
	  (reverse
	   (loop for p in padding append (loop repeat 2 collect p))))))

(defun !padding (x padding &key (value 0.0))
  (declare (type list padding)
	   (type Tensor x))
  (assert (= (ndim x) (length padding)))
  (let* ((slc
	   (loop for pads in padding
		 for s in (shape x)
		 if (listp pads)
		   collect (list (car pads) (!+ (iconst s) (iconst (car pads))))
		 else
		   collect t))
	 (padded-tensor
	   (make-tensor
	    (loop for s in (shape x)
		  for pads in padding
		  if (listp pads)
		    collect (!+ (iconst s) (iconst (car pads)) (iconst (second pads)))
		  else
		    collect s)
	    :initial-element value :dtype (dtype-of x)))
	 (out (!move (apply #'!view padded-tensor slc) x)))
    (apply #'!view-from-base out (loop for s in (shape padded-tensor) collect `(0 ,s)))))

(defun !padding2d (x padding &key (value 0.0))
  "padding = (padding_left, padding_right, padding_top, padding_bottom)
(~ top/bottom left/right)"
  (declare (type Tensor x) (type list padding) (type number value))
  (assert (= 4 (length padding)))
  (when (every #'(lambda (x) (and (numberp x) (= 0 x))) padding)
    (return-from !padding2d x))
  (let* ((s2 (last (shape x) 2))
	 (slc
	   (list
	    (list (nth 0 padding) (!+ (iconst (first s2)) (iconst (nth 0 padding))))
	    (list (nth 2 padding) (!+ (iconst (second s2)) (iconst (nth 2 padding))))))
	 (base
	   (make-tensor
	    (append
	     (butlast (shape x) 2)	     
	     (list (!+ (iconst (first  s2)) (iconst (nth 0 padding)) (iconst (nth 1 padding))))
	     (list (!+ (iconst (second s2)) (iconst (nth 2 padding)) (iconst (nth 3 padding)))))
	    :initial-element value :dtype (dtype-of x)))
	 (out
	   (apply
	    #'!view
	    base
	    (append
	     (loop for i in (butlast (shape x) 2) collect t)
	     slc)))
	 (out (!move out x)))
    (apply #'!view-from-base out (loop for s in (shape base) collect `(0 ,s)))))

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
