(in-package :caten/avm)

(defparameter *device* :lisp "a keyword to indicate the device (being dispatched)")
(defparameter *max-display-len* 10)
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
  (views views :type list)
  ;; Metadata for JIT
  (inferred-permute nil :type list)
  (depend-idx-list nil :type list)
  (orig-buffer-shape nil :type list)
  (shape-base nil :type list)
  (stride-base nil :type list)
  (views-base nil :type list))

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
(defun pprint-buffer (buffer &key (indent 0) (max *max-display-len*) (comma " ") (bracket-start "(") (bracket-end ")") (omit1 "~") (omit2 "..."))
  (handler-bind ((error
		   #'(lambda (c)
		       (warn "Failed to render the buffer due to~%~a" c)
		       (let* ((condition (format nil "~a" c))
			      (trim (subseq condition 0 (min (length condition) 70))))
			 (return-from pprint-buffer (format nil "~a<<Error during rendering: ~a...>>" (indent indent) trim))))))
    (%pprint-buffer buffer :indent-with indent :max max
			   :bracket-start bracket-start :bracket-end bracket-end
			   :comma comma :omit1 omit1 :omit2 omit2)))

(defun %pprint-buffer (buffer &key (indent-with 0) (max *max-display-len*) (comma " ") (bracket-start "(") (bracket-end ")") (omit1 "~") (omit2 "..."))
  (declare (type buffer buffer) (type fixnum indent-with max))
  ;; Scalars
  (when (= (buffer-nrank buffer) 0)
    (return-from %pprint-buffer (format nil "~a~a" (indent indent-with) (buffer-value buffer))))

  (let ((sample-size
	  (loop for i upfrom 0
		  below
		  (min
		   1000 ;; if elements are sparse ...
		   (apply #'* (map 'list #'(lambda (x v) (if (fourth v) 1 x)) (buffer-shape buffer) (buffer-views buffer))))
		maximize (length (format nil "~a" (%vm/read-index *device* buffer i))))))
    (with-output-to-string (stream)
      (format stream " ~a" (indent indent-with))
      (labels ((pprint-helper (dim subscripts lastp indent)
		 (let ((size (nth dim (buffer-shape buffer))))
		   (if (null size)
		       (let* ((content (format nil "~a" (apply #'buffer-ref buffer subscripts)))
			      (diff    (max 0 (- sample-size (length content))))
			      (offset  (with-output-to-string (out) (dotimes (i diff) (princ " " out)))))
			 (format stream "~a~a" content (if lastp "" offset)))
		       (if (<= size max)
			   (progn
			     (format stream bracket-start)
			     (dotimes (i size)
			       (setf (nth dim subscripts) i)
			       (pprint-helper (1+ dim) subscripts (= i (1- size)) (1+ indent))
			       (unless (= i (1- size)) (format stream comma)))
			     (format stream bracket-end)
			     (unless lastp (format stream "~%~a" (indent indent))))
			   (let ((mid (round (/ max 2))))
			     (format stream bracket-start)
			     (dotimes (i mid)
			       (setf (nth dim subscripts) i)
			       (pprint-helper (1+ dim) subscripts (= i (1- size)) (1+ indent))
			       (format stream comma))
			     (if (= dim (1- (buffer-nrank buffer)))
				 (format stream "~a " omit1)
				 (format stream "~a~a~%~a"
					 (indent (+ indent-with (* mid sample-size)))
					 omit2
					 (indent (+ 2 indent))))
			     (dotimes (i mid)
			       (let ((idx (+ (- size mid) i)))
				 (setf (nth dim subscripts) idx)
				 (pprint-helper (1+ dim) subscripts (= i (1- mid)) (1+ indent))
				 (unless (= i (1- mid)) (format stream comma))))
			     (format stream bracket-end)
			     (unless lastp (format stream "~%~a" (indent indent)))))))))
	(pprint-helper 0 (make-list (buffer-nrank buffer) :initial-element nil) t indent-with)))))

