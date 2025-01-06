(in-package :caten/apis)

(defgeneric change-facet (obj direction)
  (:documentation "
```
(change-facet obj direction)
```

`change-facet` converts `obj` to the data type specified by direction.
During the conversion process, it attempts to synchronize the Buffer (i.e., no copying is performed).

By default, `:direction` could be one of :tensor, :simple-array, :array.

Users can extend this method if needed.
"))

(defun obj-dtype-of (obj)
  (if (numberp obj)
      (if (floatp obj) *default-float* *default-int*)
      (if (typep obj 'boolean) :bool (error "Cannot determine the type of ~a" obj))))

(defun simple-array->array (array dimensions dtype)
  (declare (type simple-array array))
  (make-array dimensions :element-type (dtype->lisp dtype) :displaced-to array :displaced-index-offset 0))

(defmethod change-facet ((obj number) (direction (eql :tensor)))
  (ctx:with-contextvar (:BACKEND "LISP")
    (proceed (make-scalar obj :dtype (obj-dtype-of obj)))))

(defmethod change-facet ((obj list) direction)
  (labels ((list-dimensions (list depth)
	     (loop repeat depth
		   collect (length list)
		   do (setf list (car list))))
	   (list-to-array (list depth)
	     (make-array (list-dimensions list depth)
			 :initial-contents list
			 :element-type (dtype->lisp (obj-dtype-of (car (flatten list))))))
	   (get-dimensions (x &optional (n 1))
	     (if (some #'listp x)
		 (get-dimensions (car x) (1+ n))
		 n)))
    (change-facet (list-to-array obj (get-dimensions obj)) direction)))

(defmethod change-facet ((obj array) (direction (eql :tensor)))
  (let* ((storage
           (or
            (array-displacement obj)
            (progn
	      #+sbcl(sb-ext:array-storage-vector obj)
	      #-sbcl(make-array (apply #'* (array-dimensions obj))
                                :element-type (array-element-type obj)
			        :displaced-to obj))))
         (dtype (if (eql t (array-element-type obj))
                    (obj-dtype-of (aref storage 0))
                    (caten/common.dtype:lisp->dtype (array-element-type obj))))
         (buffer (make-buffer (array-dimensions obj) (static-compute-strides *default-order* (array-dimensions obj)) dtype nil :device (caten/codegen/backend:get-buffer-type)))
         (_ (setf (buffer-value buffer) storage))
         (place (make-tensor (array-dimensions obj) :dtype dtype :from buffer)))
    (declare (ignore _))
    (setf (tensor-buffer place) buffer)
    place))

(defmethod change-facet ((obj tensor) (direction (eql :array)))
  (assert (tensor-buffer obj) () "The tensor ~a is not realized." obj)
  (let ((storage (buffer-value (tensor-buffer obj))))
    (simple-array->array storage (buffer-shape (tensor-buffer obj)) (tensor-dtype obj))))

(defmethod change-facet ((obj tensor) (direction (eql :simple-array)))
  (assert (tensor-buffer obj) () "The tensor ~a is not realized." obj)
  (buffer-value (tensor-buffer obj)))

(defmacro with-facet ((bind (object &key (direction :array))) &body body)
  "
```
(with-facet (bind (object &key (direction :array))) &body body)
```

Binds the result of `(change-facet object direction)` to the `bind`.
"
  `(let ((,bind (change-facet ,object ,direction)))
     ,@body))

(defmacro with-facets ((&rest input-forms) &body body)
  "
```
(with-facets ((&rest input-forms) &body body))
```

Expands to a series of `with-facet` forms.
"
  (labels ((expand-forms (rest-forms)
	     (if rest-forms
		 `(with-facet ,(car rest-forms)
		    ,(expand-forms (cdr rest-forms)))
		 `(locally ,@body))))
    (expand-forms input-forms)))
