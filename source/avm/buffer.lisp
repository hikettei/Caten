(in-package :caten/avm)

(defparameter *device* :lisp "a keyword to indicate the device (being dispatched)")
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
