(in-package :caten/onnx)

(macrolet ((def-elwise (name version op)
	     `(defop (,name ,version)
		  ((gph inputs attrs)
		    (declare (ignore attrs))
		    (assert (= 2 (length inputs)) () "Assertion Failed: ~a expects binary ops but got ~a" ,name inputs)
                    (reduce #',op inputs)))))
  (def-elwise "Add" 1 caten:!add)
  (def-elwise "Sub" 1 caten:!sub)
  (def-elwise "Mul" 1 caten:!mul)
  (def-elwise "Div" 1 caten:!div))

(defop ("Conv" 1)
    ((cls inputs attrs)
      (let* ((data   (nth 0 inputs))
	     (kernel (nth 1 inputs))
	     (ndim   (caten:ndim data)))
        
	(when (null (gethash "kernel_shapes" attrs))
	  (setf (gethash "kernel_shapes" attrs) (cddr (caten:shape kernel))))

	(when (gethash "auto_pad" attrs)
	  (cond
	    ((or (string= "SAME_UPPER" (gethash "auto_pad" attrs))
		 (string= "SAME_LOWER" (gethash "auto_pad" attrs)))
	     (error "Conv: auto_pad SAME_UPPER and SAME_LOWER are not supported yet."))
	    ((string= "VALID" (gethash "auto_pad" attrs))
	     (setf (gethash "pads" attrs) (range 0 (- ndim 2))))
	    ((string= "NOTSET" (gethash "auto_pad" attrs)))
	    (T
	     (error "Value ~a in attribute \"auto_pad\" of operator Conv is invaild." (gethash "auto_pad" attrs)))))
        
	(setf (gethash "channels" attrs) (car (gethash "kernel_shapes" attrs)))
	(when (listp (gethash "pads" attrs))
          (assert (= (mod (length (gethash "pads" attrs)) 2) 0))
	  (let* ((pads (loop with size = (length (gethash "pads" attrs))
			     with mid = (/ size 2)
			     for i upfrom 0 below size by 2
			     for j = (+ mid 1)
			     do (assert (= (nth i (gethash "pads" attrs)) (nth j (gethash "pads" attrs))) () "Conv: Pads must be symmetric (TODO: Support this).")
			     collect (list (nth i (gethash "pads" attrs)) (nth j (gethash "pads" attrs))))))
            (caten/nn:!convnd data kernel :bias (third inputs) :stride (gethash "strides" attrs 1) :padding (map 'list #'car pads) :dilation (gethash "dilations" attrs 1) :groups (gethash "group" attrs 1)))))))
;; https://github.com/onnx/onnx/blob/main/docs/Operators.md#MaxPool
(defop ("MaxPool" 1)
    ((cls inputs attrs)
      (let ((pads
	      (when (listp (gethash "pads" attrs))
		(assert (= (mod (length (gethash "pads" attrs)) 2) 0))
		(loop with size = (length (gethash "pads" attrs))
		      with mid = (/ size 2)
		      for i upfrom 0 below size by 2
		      for j = (+ mid 1)
		      do (assert (= (nth i (gethash "pads" attrs)) (nth j (gethash "pads" attrs))) () "Conv: Pads must be symmetric (TODO: Support this).")
		      collect (list (nth i (gethash "pads" attrs)) (nth j (gethash "pads" attrs)))))))
        (caten:call (caten/nn:MaxPool (gethash "kernel_shape" attrs) :padding (map 'list #'car pads) :stride (gethash "strides" attrs)) (first inputs)))))

(macrolet ((def-unary (name version op)
             `(defop (,name ,version)
                  ((gph inputs attrs)
                    (declare (ignore attrs))
		    (,op (car inputs))))))
  (def-unary "Sin" 7 caten:!sin)
  (def-unary "Cos" 7 caten:!cos)
  (def-unary "Tan" 7 caten:!tan)
  (def-unary "Exp" 1 caten:!exp)
  (def-unary "Sqrt" 1 caten:!sqrt)
  (def-unary "Relu" 1 caten/nn:!relu)
  (def-unary "Sigmoid" 1 caten/nn:!sigmoid))

(defop ("Softmax" 1)
    ((cls inputs attrs)
      (caten/nn:!softmax (car inputs) :axis (gethash "axis" attrs 1))))

(defop ("LeakyRelu" 6)
    ((cls inputs attrs)
      (let ((alpha (gethash "alpha" attrs)))
	(caten/nn:!leaky-relu (car inputs) :neg-slope alpha))))

(defop ("Erf" 13)
    ((cls inputs attrs)
      (declare (ignore attrs))
      ;; Approximation of error function.
      ;; x.sign() * (1 - ((((1.061405429 * t + -1.453152027) * t + 1.421413741) * t + -0.284496736) * t + 0.254829592) * t * (-(x.square())).exp())
      (let ((t1 (caten:!recip (caten:!+ (caten:fconst 1) (caten:!* (caten:fconst 0.3275911) (caten:!abs (car inputs)))))))
	(caten:!*
         (caten:!signum (car inputs))
	 (caten:!-
	  (caten:fconst 1.0)
	  (caten:!*
	   (caten:!+
	    (caten:!+
	     (caten:!*
	      t1
	      (caten:!+
	       (caten:!* (caten:fconst 1.061405429) t1)
	       (caten:fconst -1.453152027)))
	     (caten:fconst 1.421413741))
	    t1
	    (caten:fconst -0.284496736))
	   t1
	   (caten:!exp (caten:!neg (caten:!square (car inputs))))))))))

(defop ("Gemm" 1)
    ((cls inputs attrs)
      (assert (or (= (length inputs) 2) (= (length inputs) 3))
	      ()
	      "Assertion failed. Gemm should take two or three inputs but got: ~a" inputs)
      (let ((alpha (gethash "alpha" attrs))
	    (beta  (gethash "beta" attrs))
	    (transA (gethash "transA" attrs 0))
	    (transB (gethash "transB" attrs 0)))
	(let* ((a (if alpha (caten:!mul (car inputs) (caten:!const (car inputs) alpha)) (car inputs)))
	       (b (second inputs))
	       (a (if (= 1 transA) (caten:!t a) a))
	       (b (if (= 1 transB) (caten:!t b) b))
	       (out (caten:!matmul a b))
	       (out (if (= (length inputs) 3)
			(caten:!add out (caten:!mul (caten:!const b beta) (third inputs)))
			out)))
	  out))))

(defop ("MatMul" 1)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (caten:!matmul (car inputs) (second inputs))))

(defop ("Shape" 1)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (loop for s in (caten:shape (car inputs))
	    collect (caten::->iconst s))))

(defop ("Shape" 15)
    ((cls inputs attrs)
      (multiple-value-bind (start end) (values (gethash "start" attrs) (gethash "end" attrs))
	(map 'list #'caten::->iconst (subseq (caten:shape (car inputs)) start end)))))

(defop ("Unsqueeze" 1)
    ((cls inputs attrs)
      (let* ((x (if (= (caten:ndim (car inputs)) 0)
                    (caten:!reshape (car inputs) 1)
                    (car inputs)))
             (new-shape (copy-list (caten:shape x)))
             (normalized-axes
               (loop with ndim = (caten:ndim x)
                     for axis in (gethash "axes" attrs)
                     for maybe-fixnum = (caten::canonicalize-int axis) ;; If axis is tensor and created from graph, fold them.
                     if (and (numberp maybe-fixnum) (< maybe-fixnum 0))
                       collect (+ ndim maybe-fixnum)
                     else
                       collect axis)))
	(dolist (axis normalized-axes)
          (setf new-shape (append (subseq new-shape 0 axis) `(1) (subseq new-shape axis))))
        (caten:!reshape x new-shape))))

(defop ("Concat" 1)
    ((cls inputs attrs)
      (setf inputs (alexandria:flatten inputs))
      (apply #'caten:!concatenate (gethash "axis" attrs) (alexandria:flatten inputs))))

;; [TODO] Gather (needs a update of caten compiler?)
(defop ("Cast" 1)
    ((cls inputs attrs)
      (warn "Cast is not tested!")
      ;; [TODO] Rename "to" into dtype
      (caten:!cast (car inputs) (gethash "to" attrs))))

(defop ("Clip" 13)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (assert (= 3 (length inputs)) () "Clip expects 3 inputs(input, min, max) but got ~a" inputs)
      (apply #'caten:!clip inputs)))

(defop ("Range" 11)
    ((cls inputs attrs)
      (declare (ignore inputs))
      (let* ((start (caten::->iconst (gethash "start" attrs)))
             (limit (caten::->iconst (gethash "limit" attrs)))
             (delta (caten::->iconst (gethash "delta" attrs)))
             ;; Ref: https://github.com/onnx/onnx/blob/main/docs/Changelog.md#range-11
             (number-of-elements
               (caten:!maximum (caten:!ceiling (caten:!idiv (caten:!- start limit) delta)) (caten:!const start 0)))
             (out (caten:make-tensor `(,number-of-elements) :dtype :int64)))
        (caten:!add start(caten:!mul delta (caten:!index-components out))))))    

(defop ("Reduce" 1)
    ((gph inputs attrs &key (reduce #'caten:!sum))
      (let ((axis (gethash "axes" attrs 0)))
        ;; [TODO] Update the shape inference of keepdims
        (funcall reduce (car inputs) :axis axis :keepdims (= (gethash "keepdims" attrs 1) 1)))))

;; [TODO] TopK
(macrolet ((defreduce (name op)
	     `(defop (,name 0)
		  ((gph inputs attrs)
		    (let ((r (get-converter "Reduce" (gp-opset-version gph))))
		      (funcall r gph inputs attrs :reduce ,op))))))
  (defreduce "ReduceMax"  #'caten:!max)
  (defreduce "ReduceMin"  #'caten:!min)
  (defreduce "ReduceSum"  #'caten:!sum)
  (defreduce "ReduceMean" #'caten:!mean)
  ;; [TODO] ReduceProd
  ;; [TODO] ReduceLogSumExp
  )

(defop ("Less" 9)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (caten:!< (car inputs) (second inputs))))

(defop ("Equal" 9)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (caten:!eq (car inputs) (second inputs))))

(defop ("Where" 9)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (multiple-value-bind (condition x y) (apply #'values inputs)
        (caten:!where condition x y))))

(defop ("Pow" 13)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (caten:!expt (car inputs) (second inputs))))

(defop ("Constant" 9)
    ((cls inputs attrs)
      (declare (ignore inputs))
      (let ((val (gethash "value" attrs)))
	(when val
	  (if (numberp val)
	      (caten:iconst val)
              (if (stringp val)
                  (caten:iconst (intern val))
	          (if (arrayp val)
		      (caten:change-facet val :tensor)
		      (error "Constant-9: Cannot translate ~a into Caten?" val))))))))

(defop ("ConstantOfShape" 9)
    ((cls inputs attrs)
      (let* ((value (or (and (gethash "value" attrs) (aref (gethash "value" attrs) 0)) 0.0))
	     (shape (coerce (caten:change-facet (car inputs) :simple-array) 'list)))
        (caten:make-tensor shape :initial-element value))))
;; [TODO] Slice-10
(defop ("Transpose" 1)
    ((cls inputs attrs)
      (let ((perm (gethash "perm" attrs)))
	(assert (listp perm) () "[WIP] Transpose: perm must be static and given as a list. but got ~a" perm)
	(caten:!permute (car inputs) perm))))

(defop ("Expand" 8)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (let ((new-shape (second inputs)))
        (caten:!expand (car inputs) (coerce (caten:change-facet new-shape :simple-array) 'list)))))
;; [TODO] Tile
(defop ("Reshape" 1)
    ((cls inputs attrs)
      (caten:!reshape (car inputs) (gethash "shape" attrs))))

(defop ("Flatten" 1)
    ((cls inputs attrs)
      (let ((axis (or (gethash "axis" attrs) 1)))
        (caten:!flatten (car inputs) :axis axis))))
;; Ref: https://github.com/onnx/onnx/blob/main/docs/Changelog.md#globalaveragepool-1
(defop ("GlobalAveragePool" 1)
    ((cls inputs attrs)
      (declare (ignore attrs))
      (caten/nn:!avgpool (car inputs) :kernel-size (last (caten:shape (car inputs)) 2))))
