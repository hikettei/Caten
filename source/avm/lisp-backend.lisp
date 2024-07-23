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

(defun map-view (op &rest buffers)
  (assert (every #'(lambda (x) (= (buffer-nrank (car buffers)) (buffer-nrank x))) buffers)
	  ()
	  "map-view: inconsistent use of nrank: ~a" buffers)
  (let ((out (copy-buffer (car buffers))))
    ;; TODO: view
    (if (= 0 (buffer-nrank (car buffers)))
	(setf (buffer-value out) (apply op (map 'list #'buffer-value buffers)))
	(progn
	  (setf (buffer-value out) (copy-seq (buffer-value out)))
	  (apply #'map-into (buffer-value out) op (map 'list #'buffer-value buffers))))
    (print out)
    out))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride)
      (parse-allocate-node node args)
    (realize-buffer graph (node->id node) :shape1 shape :stride1 stride)))

(defmethod %impl ((device-id (eql :lisp)) (op (eql :Sqrt)) graph node args) (apply #'map-view #'sqrt args))
