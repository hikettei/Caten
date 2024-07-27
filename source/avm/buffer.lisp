(in-package :caten/avm)

(defparameter *device* :lisp "a keyword to indicate the device (being dispatched)")
(defparameter *max-display-len* 4)
(defmacro with-device (device &body body)
  "declares the device to use"
  `(let ((*device* ,device)) ,@body))

(defstruct (Buffer
	    (:constructor make-buffer (nrank shape stride dtype views)))
  (value nil)
  (nrank nrank :type fixnum)
  (shape shape :type list)
  (stride stride :type list)
  (dtype dtype :type dtype-t)
  (views views :type list))

;; methods start with % = users need to override it to implement new backends.
(defgeneric %vm/allocate-buffer (device-id buffer)
  (:documentation "This method allocates new array/scalar based on buffer, modifying buffer-value, returning buffer."))

(defun realize-buffer (graph id &key shape1 stride1 views1)
  "allocates the id"
  (declare (type graph graph)
	   (type symbol id))
  (multiple-value-bind (nrank shape stride dtype views)
      (infer-tensor-info graph id)
    (when shape1  (setf shape shape1))
    (when stride1 (setf stride stride1))
    (when views1  (setf views views1))
    (%vm/allocate-buffer
     *device*
     (make-buffer nrank shape stride dtype views))))

(defgeneric %vm/read-index (device-id buffer nth)
  (:documentation "Reads `nth` element of the buffer. Only be used to render the tensor, so you do not have to consider about the performance."))

(defun buffer-ref (buffer &rest subscripts)
  "Very slow, should be used to only render the tensors."
  (declare (type buffer buffer) (type list subscripts))
  (assert (= (length subscripts) (buffer-nrank buffer)) ())
  (flet ((->idx (rank)
	   (let ((view (nth rank (buffer-views buffer)))
		 (idx  (nth rank subscripts))
		 (stride (nth rank (buffer-stride buffer))))
	     (if view ;; (from to by broadcast)
		 (if (fourth view)
		     0
		     (* (+ (first view) idx) (third view) stride))
		 (* stride idx)))))
    (%vm/read-index *device* buffer (apply #'+ (map 'list #'->idx (range 0 (buffer-nrank buffer)))))))

(defun indent (n) (with-output-to-string (o) (dotimes (i n) (princ " " o))))
(defun pprint-buffer (buffer &key (indent 0) (max *max-display-len*))
  (handler-bind ((error
		   #'(lambda (c)
		       (warn "pprint-buffer: Failed to render the content due to~%~a" c)
		       (format nil "~a<<Error During Rendering>>" (indent indent)))))
    (%pprint-buffer buffer :indent indent :max max)))

(defun %pprint-buffer (buffer &key (indent 0) (max *max-display-len*))
  (declare (type buffer buffer) (type fixnum indent max))
  ;; Scalars
  (when (= (buffer-nrank buffer) 0)
    (return-from %pprint-buffer (format nil "~a~a" (indent indent) (buffer-value buffer))))
  buffer
  )

