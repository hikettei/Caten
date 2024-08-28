(defpackage :caten/external.backends.metal
  (:use :cl :caten/ajit :caten/air :caten/avm :cffi :cl-metal)
  (:import-from
   :caten/common.dtype
   #:dtype/cast))
(in-package :caten/external.backends.metal)

(defparameter *access* nil)
(defparameter *args* nil)
(defun args-p (id) (if (stringp id) (find (intern id) *args*) (find id *args*)))

;; tensor_cores = [TensorCore(dims=(8,8,8), threads=[(0,2),(1,4),(0,2),(1,2)], dtype_in=di, dtype_out=do) for (di, do) in [(dtypes.float, dtypes.float), (dtypes.half, dtypes.float), (dtypes.half, dtypes.half)]] # noqa: E501

;;
(defun ->cdtype (dtype)
  (ecase dtype
    (:bool "boolean")
    (:float64 "double")
    (:float32 "float")
    (:uint64 "uint64_t")
    (:int64 "int64_t")
    (:int32 "int32_t")
    (:uint32 "uint32_t")
    (:int16 "int16_t")
    (:uint16 "uint16_t")
    (:uint8 "uint8_t")
    (:int8 "int8_t")))

#|
float2 __WMMA_8_8_8_float_float(float2 m, float2 n, float2 o) {
  simdgroup_float8x8 a,b,c; a.thread_elements()[0] = m.x; a.thread_elements()[1] = m.y; b.thread_elements()[0] = n.x;
  b.thread_elements()[1] = n.y; c.thread_elements()[0] = o.x; c.thread_elements()[1] = o.y; simdgroup_multiply_accumulate(c, a, b, c);
  return float2(c.thread_elements()[0], c.thread_elements()[1]);
}
|#
(defmethod %render-program-toplevel ((lang (eql :metal)) body) (format nil "~%#include <metal_stdlib>~%using namespace metal;~%~a" body))
(defmethod %render-function ((lang (eql :metal)) avm args body)
  (let ((extra-args `("uint3 gid [[threadgroup_position_in_grid]]" "uint3 lid [[thread_position_in_threadgroup]]")))
    (format nil "kernel void ~(~a~)(~(~a~), ~a, ~a) {~%~a~%}"
	    (avm-name avm)
	    (apply
	     #'concatenate
	     'string
	     (butlast
	      (loop for arg in args
		    append
		    (list
		     (format nil "~a~a ~(~a~)"
			     (->cdtype (argument-dtype arg))
			     (if (= (buffer-nrank (argument-metadata arg)) 0)
				 (if (argument-pointer-p arg) "*" "")
				 "*")
			     (caten/ajit:argument-name arg))
		     ", "))))
	    (car extra-args)
	    (second extra-args)
	    body)))
