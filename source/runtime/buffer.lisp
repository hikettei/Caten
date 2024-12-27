(defpackage :caten/runtime/buffer
  (:documentation "
Buffer: Abstraction for the multiple facet of storage object.
```
         (tensor-buffer tensor)
          /       |         \
 ClangBuffer  MetalBuffer CUDABuffer etc...
```

Buffer expects following the methods to be implemented:

- open-buffer
- close-buffer
- transfer-from-array
- transfer-into-array
- bref
")
  (:use :cl :alexandria)
  (:export
   #:AbstractBuffer
   #:buffer-shape
   #:buffer-stride
   #:buffer-dtype
   #:buffer-views
   #:buffer-nrank
   #:buffer-value
   #:make-buffer
   
   #:open-buffer
   #:close-buffer
   #:transfer-from-array
   #:transfer-into-array
   #:bref
   #:buffer-ref
   #:pprint-buffer))

(in-package :caten/runtime/buffer)

(defparameter *max-display-matrix* 2)
(defparameter *max-display-len* 10)

(defclass AbstractBuffer ()
  ((shape :accessor buffer-shape :initarg :shape :initform nil :type list)
   (stride :accessor buffer-stride :initarg :stride :initform nil :type list)
   (dtype :accessor buffer-dtype :initarg :dtype :type keyword)
   (views :accessor buffer-views :initarg :views :initform nil :type list)
   (nrank :accessor buffer-nrank :initarg :nrank :initform 0 :type fixnum)
   (value :accessor buffer-value :initarg :value :initform nil))
  (:documentation ""))

(defgeneric open-buffer (buffer)
  (:documentation "Fills the (buffer-value buffer) with zero the given shape and dtype."))

(defgeneric close-buffer (buffer) (:documentation "Frees the buffer."))

(defgeneric transfer-from-array (buffer array) (:documentation ""))

(defgeneric transfer-into-array (buffer array) (:documentation ""))

(defgeneric bref (buffer index) (:documentation ""))

(defun buffer-ref (buffer &rest subscripts)
  (declare (type AbstractBuffer buffer) (type list subscripts))
  (assert (= (length subscripts) (buffer-nrank buffer)) ())
  (flet ((->idx (rank)
	   (let ((view   (nth rank (buffer-views buffer)))
		 (idx    (nth rank subscripts))
		 (stride (nth rank (buffer-stride buffer))))
	     (if view
		 (if (fourth view)
		     0
		     (* (+ (first view) idx) (third view) stride))
                 (* stride idx)))))
    (bref buffer (apply #'+ (map 'list #'->idx (iota (buffer-nrank buffer)))))))

(defun make-buffer (shape stride dtype views &key (value nil) (device 'AbstractBuffer))
  (declare (type list shape stride views))
  (assert (= (length views) (length shape) (length stride)))
  (make-instance device :nrank (length shape) :shape shape :stride stride :dtype dtype :views views :value value))
;; ~~~~~ pprint-buffer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun indent (n) (with-output-to-string (o) (dotimes (i n) (princ " " o))))

(defun pprint-buffer (buffer &key (indent 0) (max *max-display-len*) (comma " ") (bracket-start "(") (bracket-end ")") (omit1 "~") (omit2 "..."))
  (handler-bind ((error
		   #'(lambda (c)
		       (warn "Failed to render the buffer due to~%~a~%Invaild strides?" c)
		       (let* ((condition (format nil "~a" c))
			      (trim (subseq condition 0 (min (length condition) 70))))
			 (return-from pprint-buffer (format nil "~a<<Error during rendering: ~a...>>" (indent indent) trim))))))
    (%pprint-buffer buffer :indent-with indent :max max
			   :bracket-start bracket-start :bracket-end bracket-end
			   :comma comma :omit1 omit1 :omit2 omit2)))

(defun %pprint-buffer (buffer &key (indent-with 0) (max *max-display-len*) (comma " ") (bracket-start "(") (bracket-end ")") (omit1 "~") (omit2 "..."))
  (declare (type AbstractBuffer buffer) (type fixnum indent-with max))
  (when (= (buffer-nrank buffer) 0) (return-from %pprint-buffer (format nil "~a~a" (indent indent-with) (buffer-value buffer))))
  (let ((sample-size
	  (loop for i upfrom 0 below (min 1000 (apply #'* (map 'list #'(lambda (x v) (if (fourth v) 1 x)) (buffer-shape buffer) (or (buffer-views buffer) (loop for s in (buffer-shape buffer) collect nil)))))
                maximize (length (format nil "~a" (buffer-ref buffer i))))))
    (with-output-to-string (stream)
      (format stream " ~a" (indent indent-with))
      (labels ((pprint-helper (dim subscripts lastp indent &aux (max (if (and (>= (buffer-nrank buffer) 3) (<= dim (- (buffer-nrank buffer) 3))) *max-display-matrix* max)))
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
