(in-package :caten/avm)

(defun dtype->lisp (dtype)
  (case dtype
    (:float64 'double-float)
    (:float32 'single-float)
    (:uint32  '(unsigned-byte 32))
    (:int32   '(signed-byte 32))
    (otherwise (error "dtype->lisp: ~a is not supported" dtype))))

(defmethod %vm/allocate-buffer ((device-id (eql :lisp)) buffer)
  (if (= (buffer-nrank buffer) 0)
      (setf (buffer-value buffer) (coerce 0 (dtype->lisp (buffer-dtype buffer))))
      (setf (buffer-value buffer)
	    (make-array (apply #'* (buffer-shape buffer))
			:element-type (dtype->lisp (buffer-dtype buffer))
			:initial-element (coerce 0 (dtype->lisp (buffer-dtype buffer))))))      
  buffer)

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride)
      (parse-allocate-node node args)
    (realize-buffer graph (node->id node) :shape1 shape :stride1 stride)))
