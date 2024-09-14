(in-package :caten/gguf)

(defun gguf-metadata-value-type (indicator)
  (ecase indicator
    (0 :uint8)
    (1 :int8)
    (2 :uint16)
    (3 :int16)
    (4 :uint32)
    (5 :int32)
    (6 :float32)
    (7 :bool)
    (8 :string)
    (9 :array)
    (10 :uint64)
    (11 :int16)
    (12 :float64)))

(Defun value-type->lisp-type (dtype)
  (case dtype
    (:array 'simple-array)
    (:string 'string)
    (otherwise (caten/common.dtype:dtype->lisp dtype))))

(defun gguf-metadata-value (buffer dtype)
  (declare (type keyword dtype) (type input-buffer buffer))
  (ecase dtype
    (:uint8 (readu8-le buffer))
    (:int8 (read8-le buffer))
    (:uint16 (readu16-le buffer))
    (:int16 (read16-le buffer))
    (:uint32 (readu32-le buffer))
    (:int32 (read32-le buffer))
    (:uint64 (readu64-le buffer))
    (:int64 (read64-le buffer))
    (:float32 (readf32-le buffer))
    (:float64 (readf64-le buffer))
    (:bool
     (let ((val (readu8-le buffer)))
       (case val
	 (0 nil)
	 (1 t)
	 (otherwise (error "gguf-metadata-value: Invaild bool value ~a" val)))))
    (:string (gguf-string buffer))
    (:array
     (let* ((value-type (gguf-metadata-value-type (readu32-le buffer)))
	    (len (readu64-le buffer))
	    (array (loop repeat len collect len collect (gguf-metadata-value buffer value-type))))
       (make-array (length array) :initial-contents array)))))

(defstruct (Metadata
	    (:constructor make-metadata (key value-type value)))
  (key key :type string)
  (value-type value-type :type keyword)
  (value value :type (or number boolean simple-array)))

(defmethod print-object ((metadata Metadata) stream)
  (let* ((obj (format nil "~a" (metadata-value metadata)))
	 (obj (if (>= (length obj) *print-object-omit-threshold*)
		  (format nil "~a..." (subseq obj 0 *print-object-omit-threshold*))
		  obj))
	 (type (metadata-value-type metadata)))
    (format stream "<Metadata[~a] ~a -> ~a>"
	    (case type
	      (:array
	       (assert (arrayp (metadata-value metadata)))
	       (format nil "Array{~a x ~a}" (array-element-type (metadata-value metadata)) (array-total-size (metadata-value metadata))))
	      (otherwise type))
	    (metadata-key metadata) obj)))

(defun make-gguf-metadata (buffer)
  (declare (type input-buffer buffer))
  (multiple-value-bind (key value-type)
      (values
       (gguf-string buffer)
       (gguf-metadata-value-type (readu32-le buffer)))
    (make-metadata key value-type (gguf-metadata-value buffer value-type))))

(defun parse-metadata-kv (buffer metadata-kv-count)
  (declare (type input-buffer buffer) (type fixnum metadata-kv-count))
  (loop repeat metadata-kv-count
	collect (make-gguf-metadata buffer)))
