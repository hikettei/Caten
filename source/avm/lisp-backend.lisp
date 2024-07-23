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

(defun map-into/buffer (result op &rest buffers)
  (declare (type buffer result)
	   (type function op)
	   (type list buffers))
  (let* ((nrank (buffer-nrank (car buffers)))
	 (offsets (make-list (1+ (length buffers)) :initial-element 0)))
    (labels ((explore (dim offsets)
	       (let ((size (if (some (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)
			       (let ((view (find-if (compose #'identity #'(lambda (x) (nth dim x)) #'buffer-views) buffers)))
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
			      (setf (aref (buffer-value result) (car offsets))
				    (apply op (map 'list #'aref (map 'list #'buffer-value buffers) offsets)))
			      (explore (1- dim) (copy-list offsets)))
			  (loop for n upfrom 0
				for buff in `(,result ,@buffers)
				for view = (nth dim (buffer-views buff))
				for stride = (nth dim (buffer-stride buff))
				if view
				  do (if (fourth view)
					 nil ;; broadcast
					 (incf (nth n offsets) (* (third view) stride)))
				else
				  do (incf (nth n offsets) stride))))))
      (explore (1- nrank) offsets))))

(defun map-view (op &rest buffers)
  (assert (every #'(lambda (x) (= (buffer-nrank (car buffers)) (buffer-nrank x))) buffers)
	  ()
	  "map-view: inconsistent use of nrank: ~a" buffers)
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

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Sqrt)) graph node args) (apply #'map-view #'sqrt args))
