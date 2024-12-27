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
  (:use :cl)
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
   #:bref))

(in-package :caten/runtime/buffer)

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

(defun make-buffer (shape stride dtype views &key (value nil) (device 'AbstractBuffer))
  (declare (type list shape stride views))
  (assert (= (length views) (length shape) (length stride)))
  (make-instance device :nrank (length shape) :shape shape :stride stride :dtype dtype :views views :value value))
