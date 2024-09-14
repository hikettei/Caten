(in-package :caten/gguf)

(defun ggml-type (indicator)
  (ecase indicator
    (0 :F32)
    (1 :F16)
    (2 :Q4_0)
    (3 :Q4_1)
    ;; 4 support has been removed
    ;; 5 support has been removed
    (6 :Q5_0)
    (7 :Q5_1)
    (8 :Q8_0)
    (9 :Q8_1)
    (10 :Q2_K)
    (11 :Q2_K)
    (12 :Q4_K)
    (13 :Q5_K)
    (14 :Q6_K)
    (15 :Q8_K)
    (16 :IQ2_XXS)
    (17 :IQ2_XS)
    (18 :IQ3_XXS)
    (19 :IQ1_S)
    (20 :IQ4_NL)
    (21 :IQ3_S)
    (22 :IQ2_S)
    (23 :IQ4_XS)
    (24 :I8)
    (25 :I16)
    (26 :I32)
    (27 :I64)
    (28 :F64)
    (29 :IQ1_M)))

(defun ggml-type->lisp-type (ggml-type)
  (ecase ggml-type
    (:F32 'single-float)
    (:F16 'single-float)
    (:Q4_0 '())
    (:Q4_1)
    (:Q5_0)
    (:Q5_1)
    (:Q8_0)
    (:Q8_1)
    (:Q2_K)
    (:Q2_K)
    (:Q4_K)
    (13 :Q5_K)
    (14 :Q6_K)
    (15 :Q8_K)
    (16 :IQ2_XXS)
    (17 :IQ2_XS)
    (18 :IQ3_XXS)
    (19 :IQ1_S)
    (20 :IQ4_NL)
    (21 :IQ3_S)
    (22 :IQ2_S)
    (23 :IQ4_XS)
    (24 :I8)
    (25 :I16)
    (26 :I32)
    (27 :I64)
    (28 :F64)
    (29 :IQ1_M)))

(defstruct (Tensor-Info
	    (:constructor make-tensor-info (name n-dimension dimensions tensor-type offset)))
  (name name :type string)
  (n-dimension n-dimension :type fixnum)
  (dimensions dimensions :type list)
  (ggml-type tensor-type :type keyword)
  (relative-offset offset :type fixnum)
  (absolute-offset 0 :type fixnum)
  (buffer))

(defmethod print-object ((tensor Tensor-info) stream)
  (let* ((obj (format nil "~a" (tensor-info-buffer tensor)))
	 (obj (if (>= (length obj) *print-object-omit-threshold*)
		  (subseq obj 0 *print-object-omit-threshold*)
		  obj)))
    (format stream "<Tensor-Info{name=~a, ggml-type=:~a, dimensions=~a}~%  ~a~%>"
	    (tensor-info-name tensor)
	    (tensor-info-ggml-type tensor)
	    (tensor-info-dimensions tensor)
	    (if (null (tensor-info-buffer tensor))
		(format nil "[Not realized, relative_offset=~a, absolute_offset=~a]"
			(tensor-info-relative-offset tensor) (tensor-info-absolute-offset tensor))
		(format nil "~a" obj)))))

(defmethod tensor-info-realize ((tensor Tensor-info) buffer)
  (declare (type input-buffer buffer) (optimize (speed 3)))
  (assert (null (tensor-info-buffer tensor)) () "The given tensor-info is already realized. ~a" tensor)
  (with-fast-input (rest-of-the-file nil buffer (tensor-info-offset tensor))
    (let ((output (make-array (apply #'* (tensor-info-dimensions tensor))
			      :element-type (ggml-type->lisp-type (tensor-info-ggml-type tensor)))))
      output)))

(defun gguf-tensor-info (buffer)
  (declare (type input-buffer buffer))
  (let* ((name (gguf-string buffer))
	 (n-dimension (readu32-le buffer))
	 (dimensions (loop repeat n-dimension collect (readu64-le buffer)))
	 (tensor-dtype (ggml-type (readu32-le buffer)))
	 (offset (readu64-le buffer)))
    (make-tensor-info name n-dimension dimensions tensor-dtype offset)))

(defun parse-tensor-info (buffer tensor-count alignment)
  (declare (type input-buffer buffer) (type fixnum tensor-count))
  (let ((tensors (loop repeat tensor-count collect (gguf-tensor-info buffer)))
	(start (buffer-position buffer)))
    ;; Inconveniently, the offset defined in gguf is relative to the end of header and is unaligned.
    ;; we need to compute the absolute file offset ourselves instead.
    (loop for tensor in tensors
	  for rel = (tensor-info-relative-offset tensor)
	  for offset = (+ start rel) do
	    (incf offset (mod (- alignment (mod offset alignment)) alignment))
	    (setf (tensor-info-absolute-offset tensor) offset))
    tensors))
