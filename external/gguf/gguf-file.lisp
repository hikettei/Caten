(in-package :caten/gguf)
;; [TODO] The fastest gguf parser!

;; Corresponds to https://github.com/ggerganov/ggml/blob/master/docs/gguf.md#file-structure
(defclass GGUF ()
  ((version :type fixnum :initarg :version :accessor gguf-version)
   (tensor-count :type (unsigned-byte 64) :initarg :tensor-count :accessor gguf-tensor-count)
   (metadata-kv-count :type (unsigned-byte 64) :initarg :metadata-kv-count :accessor gguf-metadata-kv-count)
   (metadata :initarg :metadata :accessor gguf-metadata)
   (tensor-info :initarg :tensor-info :accessor gguf-tensor-info)))

(defun parse-header (buffer)
  (declare (type input-buffer buffer))
  (multiple-value-bind (g1 g2 u f)
      (values
       (fast-read-byte buffer)
       (fast-read-byte buffer)
       (fast-read-byte buffer)
       (fast-read-byte buffer))
    (let ((header (make-array 4 :element-type '(unsigned-byte 8) :initial-contents (list g1 g2 u f))))
      (babel:octets-to-string header :encoding :utf-8))))

(defun make-gguf (stream)
  "GGUF File Structure:
https://github.com/ggerganov/ggml/blob/master/docs/gguf.md#file-structure
[Magic Number (4 Byte)] | [GGUF Version (4 Byte)] | [Tensor_Count (8 Byte)] | [Metadata_KV_Count (8 Byte)] | [Rest_data]"
  (with-fast-input (buffer nil stream)
    (multiple-value-bind (header version tensor-count metadata-kv-count)
	(values
	 (parse-header buffer)
	 (readu32-le buffer)
	 (readu64-le buffer)
	 (readu64-le buffer))
      (declare (type string header) (type (unsigned-byte 64) version tensor-count))
      (assert (string= header "GGUF") () "Expecting the header to be GGUF, but got ~a.~%The given stream is not a gguf format.~%~a" header stream)
      (let* ((metadata (parse-metadata-kv buffer metadata-kv-count))
	     (gguf (make-instance 'gguf :version version :tensor-count tensor-count :metadata-kv-count metadata-kv-count
					:metadata metadata :tensor-info nil)))
	;; Processing [Rest_Data]
	;; Rest_Data is consisted of two parts:
	;; [tensor_info] | [rest_of_the_file]
	gguf))))

(defun make-gguf-from-pathname (pathname)
  "Creates GGUF from pahtname"
  (with-open-file (stream pathname :element-type '(unsigned-byte 8))
    (make-gguf stream)))
