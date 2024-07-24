(in-package :caten/avm)

(defun dtype->lisp (dtype)
  (case dtype
    (:float64 'double-float)
    (:float32 'single-float)
    (:uint32  '(unsigned-byte 32))
    (:int32   '(signed-byte 32))
    (otherwise (error "dtype->lisp: ~a is not supported" dtype))))

(defmethod %vm/allocate-buffer ((device-id (eql :lisp)) buffer)
  (if (= (buffer-nrank buffer) 0)
      (setf (buffer-value buffer) (coerce 0 (dtype->lisp (buffer-dtype buffer))))
      (setf (buffer-value buffer)
	    (make-array (apply #'* (buffer-shape buffer))
			:element-type (dtype->lisp (buffer-dtype buffer))
			:initial-element (coerce 0 (dtype->lisp (buffer-dtype buffer))))))      
  buffer)

(defun index-components ())
(defun map-into/buffer (result op &rest buffers)
  (declare (type buffer result)
	   (type function op)
	   (type list buffers))
  (let* ((nrank (buffer-nrank (car buffers)))
	 (index-components-p (eql op #'index-components))
	 (index-components (coerce 0 (dtype->lisp (buffer-dtype (car buffers)))))
	 (offsets (make-list (1+ (length buffers)) :initial-element 0)))
    (labels ((bref (buffer idx)
	       (if (= (buffer-nrank buffer) 0)
		   (buffer-value buffer)
		   (aref (buffer-value buffer) idx)))
	     (explore (dim offsets)
	       (let ((size (if (some (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)
			       (let ((view (nth dim (buffer-views (find-if (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)))))
				 (abs (- (car view) (second view))))
			       (nth dim (buffer-shape result)))))
		 ;; initial offset
		 (loop for n upfrom 0
		       for buff in `(,result ,@buffers)
		       for view = (nth dim (buffer-views buff))
		       if (and view (car view))
			 do (setf (nth n offsets) (* (nth n (buffer-stride buff)) (car view))))
		 (loop for n upfrom 0 below size
		       do (if (= dim 0)
			      (if index-components-p
				  (progn
				    (setf (aref (buffer-value result) (car offsets)) index-components)
				    (incf index-components))
				  (if (= (buffer-nrank result) 0)
				      (setf (buffer-value result) (apply op (map 'list #'bref buffers offsets)))
				      (setf (aref (buffer-value result) (car offsets))
					    (apply op (map 'list #'bref buffers offsets)))))
			      (explore (1- dim) (copy-list offsets)))
			  (loop for n upfrom 0
				for buff in `(,result ,@buffers)
				for view = (nth dim (buffer-views buff))
				for stride = (nth dim (buffer-stride buff))
				if (and stride view)
				  do (if (fourth view)
					 nil ;; broadcast
					 (incf (nth n offsets) (* (third view) stride)))
				else if stride
				       do (incf (nth n offsets) stride))))))
      (explore (1- nrank) offsets))))

(defun map-view (op &rest buffers)
  "Note: In a special case where op is #'index-components, map-view writes (car buffer) <- index-component."
  (let ((out (copy-buffer (car buffers))))
    (if (= 0 (buffer-nrank (car buffers)))
	(setf (buffer-value out) (apply op (map 'list #'buffer-value buffers)))
	(progn
	  (setf (buffer-value out) (copy-seq (buffer-value out)))
	  (apply #'map-into/buffer out op buffers)))
    out))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride)
      (parse-allocate-node node args)
    (realize-buffer graph (node->id node) :shape1 shape :stride1 stride)))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Load)) graph node args)
  (let* ((tgt (car args))
	 (val (getattr node :value))
	 (out (copy-buffer tgt)))
    (setf (buffer-value out) val)
    out))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Index-Components)) graph node args)
  (map-view #'index-components (car args)))

(macrolet ((impl (kw op)
	     `(defmethod %impl ((device-id (eql :lisp)) (op (eql ,kw)) graph node args) (apply #'map-view ,op args))))
  (impl :add #'+)
  (impl :mul #'*)
  (impl :sqrt  #'sqrt)
  (impl :neg   #'-)
  (impl :recip #'/))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :view)) graph node args)
  (multiple-value-bind (shape v1 v2 v3 stride bc)
      (parse-view-node node args)
    (let ((buffer (copy-buffer (car args))))
      (setf (buffer-shape buffer) shape
	    (buffer-stride buffer) stride
	    (buffer-views buffer)
	    (loop for i upfrom 0 below (length v1)
		  collect (list (nth i v1) (nth i v2) (nth i v3) (nth i bc))))
      buffer)))
