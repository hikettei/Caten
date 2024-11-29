(in-package :caten/test-suite)

(python-exec
 "
def test_dequantization(values, scale):
  pass

"
 )

(python-exec
"
def generate_gguf():
  pass
")

(in-package :caten/gguf)


(defparameter *scale-factor-bytes*
  (make-array 2
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x00 #x30)))


(defparameter *quantized-values*
  (make-array 32
              :element-type '(unsigned-byte 8)
              :initial-contents
              (let ((values '(1 -1 2 -2 3 -3 4 -4 5 -5 6 -6 7 -7 8 -8
                              9 -9 10 -10 11 -11 12 -12 13 -13 14 -14 15 -15 16 -16)))
                    (mapcar (lambda (v)
                              (if (>= v 0)
                                  v
                                  (+ 256 v)))
                            values))))


(defparameter *data*
  (let ((total-length (+ (length *scale-factor-bytes*) (length *quantized-values*))))
    (make-array total-length
                :element-type '(unsigned-byte 8)
                :initial-contents
                (concatenate 'list *scale-factor-bytes* *quantized-values*))))

(defparameter *input-buffer* (fast-io:make-input-buffer :vector *data*))

(defparameter *test-tensor-info*
  (make-tensor-info "test-tensor"
                    1           ;; Number of dimensions
                    '(32)       ;; Dimensions
                    :Q8_0       ;; Tensor type
                    0))         ;; Offset

(print (dequantize :Q8_0 *input-buffer* *test-tensor-info*))