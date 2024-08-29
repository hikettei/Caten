(defpackage :caten/external.backends.metal
  (:use :cl :caten/ajit :caten/air :caten/avm :cffi :cl-metal)
  (:import-from
   :caten/common.dtype
   #:dtype/cast))
(in-package :caten/external.backends.metal)
;; Reading: https://dl.acm.org/doi/pdf/10.1145/2400682.2400713
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
    (:float16 "bfloat")
    (:uint64 "uint64_t")
    (:int64 "int64_t")
    (:int32 "int32_t")
    (:uint32 "uint32_t")
    (:int16 "int16_t")
    (:uint16 "uint16_t")
    (:uint8 "uint8_t")
    (:int8 "int8_t")))

;; TODO: Global/Local Loop Scheduling
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

(defmethod %render-body ((lang (eql :metal)) kernel-lang jit-graph polyhedral indent args)
  (declare (type graph jit-graph) (type polyhedral polyhedral) (type fixnum indent))
  (let ((*args* (loop for arg in args if (argument-pointer-p arg) collect (caten/ajit:argument-name arg))))
    (with-output-to-string (out)
      (macrolet ((line (designator &rest args)
		   `(progn
		      (dotimes (i (* 2 indent)) (princ " " out))
		      (format out ,designator ,@args)
		      (format out "~%")))
		 (r (obj) `(render-expr lang ,obj)))
	(loop for node in (graph-nodes jit-graph)
	      for type = (node-type node) do
		(assert (eql :Render (node-class node)))
		(ecase type
		  (:FOR
		   (multiple-value-bind (idx upfrom below by)
		       (values (getattr node :idx) (getattr node :upfrom) (getattr node :below) (getattr node :by))
		     (assert (and idx upfrom below by) () "Missing ~a" (list idx upfrom below by))
		     (line "for(int ~(~a~)=~a;~a;~a+=~a) {" (r idx) (r upfrom) (r below) (r idx) (r by))
		     (incf indent)))
		  (:ENDFOR
		   (decf indent)
		   (line "}"))
		  (:IF
		   (let ((c (getattr node :condition)))
		     (assert c () "Missing condition")
		     (line "if ~a {" (r c))
		     (incf indent)))
		  (:ELSE
		   (decf indent)
		   (line "} else {")
		   (incf indent))
		  (:ENDIF
		   (decf indent)
		   (line "}"))
		  (:FUNCALL
		   (let ((idx (getattr node :idx))
			 (args (map 'list #'(lambda (x) (r x)) (getattr node :args))))
		     (princ (%render-nodes kernel-lang (gethash idx (poly-pipeline polyhedral)) args indent) out)))))))))

