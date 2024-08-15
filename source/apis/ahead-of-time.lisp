(in-package :caten/apis)

;; Compiles the ahead-of-time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defgeneric call-aot (device-id default-dtype default-order op &rest args))
  (defun make-avm-aot (name dtype order lambda-list body &aux (*default-order* order))
    (let* ((*default-float* (if (caten/common.dtype:dtype/floatp dtype)    dtype *default-float*))
	   (*default-uint*  (if (caten/common.dtype:dtype/uintegerp dtype) dtype *default-uint*))
	   (*default-int*   (if (caten/common.dtype:dtype/integerp dtype)  dtype *default-int*))	   
	   (graph-f (compile nil `(lambda (,@(collect-initargs-names lambda-list)) ,@body)))
	   (outputs (multiple-value-list (apply graph-f (collect-initargs-names lambda-list))))
	   (name (intern (format nil "~(~a~)" name) "KEYWORD"))
	   (blueprint (let ((*device* :lisp)) (caten outputs :jit nil :name name))))
      blueprint))
  (defmacro caten/defun[T] ((name &key (dtypes) (orders `(:row :column))) lambda-list &body body)
    `(progn
       ,@(loop
	   for order in orders
	   collect
	   (loop
	     for dtype in dtypes
	     for aot-avm = (make-avm-aot name dtype order lambda-list body)
	     do (print aot-avm)))))
	     
  (defmacro caten/defun[all] (name lambda-list &body body)
    `(caten/defun[T] (,name :dtypes (:float64 :float32 :float16 :uint64 :int64 :uint32 :int32 :uint16 :int16 :uint8 :int8)) (,@lambda-list) ,@body))
  (defmacro caten/defun[float] (name lambda-list &body body)
    `(caten/defun[T] (,name :dtypes (:float64 :float32 :float16)) (,@lambda-list) ,@body))
  (defmacro caten/defun[int] (name lambda-list &body body)
    `(caten/defun[T] (,name :dtypes (:int64 :uint32 :int32 :uint16 :int16 :uint8 :int8)) (,@lambda-list) ,@body))
  (defmacro caten/defun[uint] (name lambda-list &body body)
    `(caten/defun[T] (,name :dtypes (:uint64 :uint32 :uint16 :uint8)) (,@lambda-list) ,@body)))

;;(caten/defun[int] randn (size) (!randn `(,size)))

;; [TODO] Implement BLAS
(caten/defun[all] axpy! (n froma toa bya fromb tob byb)
  (!add (!view (make-tensor `(mn)) `(,froma ,toa ,bya)) (!view (make-tensor `(,n)) `(,fromb ,tob ,byb)))) 
