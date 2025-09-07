(defpackage :caten/test-suite/runtime/test-runtime-common
  (:use :rove :cl :caten/runtime/buffer :caten/runtime/runtime :caten/runtime/bring-your-own-backend)
  (:export #:runtime-test))
(in-package :caten/test-suite/runtime/test-runtime-common)

(defun runtime/buffer/test (&key
                              (backend "CLANG")
                              (dtypes '(:float64 :float32 :int64 :int32 :int16 :int8 :uint64 :uint32 :uint16 :uint8)))
  (ctx:with-contextvar (:BACKEND backend)
    ;; all dtypes, matrix/scalar
    (let ((runtime (make-runtime)))
      (dolist (dtype dtypes)
        (testing (format nil "Testing ~a matrix allocation, transferring, direct memory access, and deallocation." dtype)
          (let ((buf (make-buffer '(3 3) '(3 1) dtype nil)))
            (open-buffer runtime buf)
            (ok (every #'(lambda (x) (= x 0)) (transfer-into-array buf)))
            (ok (= 0 (bref buf 0)))
            (close-buffer runtime buf))))
      (dolist (dtype dtypes)
        (testing (format nil "Testing ~a scalar allocation, transferring, direct memory access, and deallocation." dtype)
          (let ((buf (make-buffer nil nil dtype nil)))
            (open-buffer runtime buf)
            (ok (= 0 (transfer-into-array buf)))
            (close-buffer runtime buf)))))))
        
