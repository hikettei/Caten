(in-package :caten/gguf)

;; [TODO] Implement QOPs in external/qnn
;; [TODO] Optimize array manipulation by implementing Lisp JIT
;; Currently, caten/gguf dequantizes the weight and computations are going under float.
;; https://github.com/ggerganov/ggml/blob/fca1caafea7de9fbd7efc733b9818f9cf2da3050/src/ggml-quants.c
;; https://github.com/99991/pygguf/blob/main/gguf.py
(defgeneric dequantize (type-id aligned-buffer tensor-info) (:documentation "Return: Dequantized Buffer"))

;;(defun buffer-sizeof (tensor-info block-size elements-per-block)
;;  (declare (optimize (speed 3)) (type fixnum block-size elements-per-block))
;;  (floor (the fixnum (/ (the fixnum (* block-size (the fixnum (apply #'* (tensor-info-dimensions tensor-info))))) elements-per-block))))

(defmethod dequantize ((type-id (eql :f32)) aligned-buffer tensor-info)
  (declare (optimize (speed 3)))
  (let* ((size (apply #'* (tensor-info-dimensions tensor-info)))
	 (out (make-array size :element-type 'single-float)))
    ;; [TODO] Optimize this by rendering C kernel
    (dotimes (i size)
      (setf (aref out i)
	    (let ((val (readf32-le aligned-buffer)))
	      (if (eql val :not-a-number)
		  (caten/apis:nan :dtype :float32)
		  val))))
    out))

(defmethod dequantize ((type-id (eql :f16)) aligned-buffer tensor-info)
  (declare (optimize (speed 3)))
  (let* ((size (apply #'* (tensor-info-dimensions tensor-info)))
	 (out (make-array size :element-type 'single-float)))
    ;; [TODO] Optimize this by rendering C kernel
    (dotimes (i size)
      (setf (aref out i)
	    (let ((val (readf16-le aligned-buffer)))
	      (if (eql val :not-a-number)
		  (caten/apis:nan :dtype :float16)
		  val))))
    out))
