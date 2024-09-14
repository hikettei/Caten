(in-package :caten/gguf)

(defparameter *print-object-omit-threshold* 200)
(defun gguf-string (buffer)
  (declare (type input-buffer buffer) (optimize (speed 3)))
  (let* ((len (readu64-le buffer))
	 (str (make-array len :element-type '(unsigned-byte 8)
			      :initial-contents
			      (loop repeat len collect (fast-read-byte buffer)))))
    (values (the string (babel:octets-to-string str :encoding :utf-8)) len)))

(declaim (inline readf16-le readf32-le readf64-le caten/common.dtype:decode-float32 caten/common.dtype:decode-float64))
(defun readf16-le (buffer) (caten/common.dtype:decode-float16 (readu16-le buffer)))
(defun readf32-le (buffer) (caten/common.dtype:decode-float32 (readu32-le buffer)))
(defun readf64-le (buffer) (caten/common.dtype:decode-float64 (readu64-le buffer)))
