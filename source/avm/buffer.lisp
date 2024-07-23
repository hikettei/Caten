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

(defun realize-buffer (graph id)
  "allocates the id"
  (declare (type graph graph)
	   (type symbol id))
  (%vm/allocate-buffer
   *device*
   (apply #'make-buffer (multiple-value-list (infer-tensor-info graph id)))))
