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

(defun create-test-buffer ()
  (make-array '(5) :element-type '(signed-byte 8) :initial-contents '(127 -128 0 64 -64)))

(defun create-test-tensor-info ()
  (let ((tensor (make-tensor-info
                 "test-tensor"
                 1
                 '(5)
                 :Q8_0
                 0)))
    (setf (tensor-info-buffer tensor) (create-test-buffer)) ;; Set binary buffer
    tensor))

(print (type-of (create-test-buffer)))

(defun test-dequantize-q8-0 ()
  (let* ((tensor-info (create-test-tensor-info))               ;; Create test tensor-info
         (aligned-buffer (tensor-info-buffer tensor-info))     ;; Get the buffer
         (expected '(12.7 -12.8 0.0 6.4 -6.4))                ;; Expected output
         (result (dequantize :Q8_0 aligned-buffer tensor-info))) ;; Call dequantize
    (if (equal result expected)
        (format t "Test passed: ~a~%" result)
        (format t "Test failed: ~a (expected ~a)~%" result expected))))

(print (create-test-buffer))

(test-dequantize-q8-0)

