(in-package :caten/apis)
;; TODO: Documentation
;; TODO: Involve them unittests
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *aot-jit* (ctx:getenv :AOT))
  (defparameter *aot-vm* (ctx:getenv :AOT_VM))
  (defun cache-dir (function-name dtype order device)
    (merge-pathnames (pathname (format nil "./.caten_aot/~(~a~)_~(~a~)_~(~a~)_~(~a~)/" function-name dtype order device))))
  (defgeneric invoke-aot-function (device-id default-dtype default-order op &rest args))
  (defun create-blueprint-from-body (name dtype order lambda-list body &key (aot-mode t) &aux (*default-order* order))
    (let* ((*default-float* (if (caten/common.dtype:dtype/floatp dtype)    dtype *default-float*))
	   (*default-uint*  (if (caten/common.dtype:dtype/uintegerp dtype) dtype *default-uint*))
	   (*default-int*   (if (caten/common.dtype:dtype/integerp dtype)  dtype *default-int*))	   
	   (graph-f (compile nil `(lambda (,@(collect-initargs-names lambda-list)) ,@body)))
	   (outputs (multiple-value-list (apply graph-f (collect-initargs-names lambda-list))))
	   (name (intern (format nil "~(~a~)_~(~a~)_~(~a~)" name order dtype) "KEYWORD"))
	   (blueprint (if aot-mode (let ((*device* :lisp)) (caten outputs :jit nil :name name)) (caten outputs :name name))))
      (when aot-mode (when (= 1 (ctx:getenv :STATIC_GENSYM)) (caten/codegen/rewriting-rules:apply-static-gensym blueprint)))	
      blueprint))
  (defmacro caten/defun[T] ((name cffi-prefix &key (dtypes) (orders `(:row :column))) lambda-list &body body)
    (declare (type string cffi-prefix))
    (let ((op-dispatcher (intern (format nil "~a/~a" name cffi-prefix) "KEYWORD")))
      `(progn
	 ,@(loop
	     for order in orders
	     append
	     (loop
	       for dtype in dtypes
	       append
	       (loop
		 for *device* in `(,@*aot-vm* ,@*aot-jit*)
		 for jit-p = (find *device* *aot-jit*)
		 for blueprint = (create-blueprint-from-body cffi-prefix dtype order lambda-list body)
		 for avm = (if jit-p (caten/codegen:jit blueprint :renderer *device* :dir (cache-dir name dtype order *device*)) blueprint)
		 append
		 `((defmethod invoke-aot-function ((device-id (eql ,*device*)) (default-dtype (eql ,dtype))
						   (order (eql ,order)) (op (eql ,op-dispatcher)) &rest args)
		     (flet ((-> (x) (if (tensor-p x) (tensor-buffer x) x)))
		       (multiple-value-bind (,@(collect-initargs-names lambda-list)) (apply #'values (map 'list #'-> args))
			 (apply #'forward ,avm (list ,@(loop for name in (collect-initargs-names lambda-list) collect `(cons ',name ,name)))))))))))
	 ;; If not implemented -> jit+cache
	 (defmethod invoke-aot-function (device-id default-dtype order (op (eql ,op-dispatcher)) &rest args)
	   (let* ((avm (create-blueprint-from-body ,cffi-prefix default-dtype order ',lambda-list ',body :aot-mode nil)))
	     (when avm
	       (eval
		`(defmethod invoke-aot-function ((device-id (eql ,device-id)) (default-dtype (eql ,default-dtype)) (order (eql ,order)) (op (eql ,,op-dispatcher)) &rest args)
		   (flet ((-> (x) (if (tensor-p x) (tensor-buffer x) x)))
		     (multiple-value-bind (,@(collect-initargs-names ',lambda-list)) (apply #'values (map 'list #'-> args))
		       (apply #'forward ,avm (list ,@(loop for name in (collect-initargs-names ',lambda-list) collect `(cons ',name ,name))))))))
	       (apply #'invoke-aot-function device-id default-dtype order op args))))
	 (defun ,name (dtype ,@lambda-list)
	   (invoke-aot-function *device* dtype *default-order* ,op-dispatcher ,@(collect-initargs-names lambda-list))))))
  (defmacro caten/defun[all] ((name cffi-prefix) lambda-list &body body)
    `(caten/defun[T] (,name ,cffi-prefix :dtypes (:float64 :float32 :float16 :uint64 :int64 :uint32 :int32 :uint16 :int16 :uint8 :int8)) (,@lambda-list) ,@body))
  (defmacro caten/defun[float] ((name cffi-prefix) lambda-list &body body)
    `(caten/defun[T] (,name ,cffi-prefix :dtypes (:float64 :float32 :float16)) (,@lambda-list) ,@body))
  (defmacro caten/defun[int] ((name cffi-prefix) lambda-list &body body)
    `(caten/defun[T] (,name ,cffi-prefix :dtypes (:int64 :uint32 :int32 :uint16 :int16 :uint8 :int8)) (,@lambda-list) ,@body))
  (defmacro caten/defun[uint] ((name cffi-prefix) lambda-list &body body)
    `(caten/defun[T] (,name ,cffi-prefix :dtypes (:uint64 :uint32 :uint16 :uint8)) (,@lambda-list) ,@body)))
