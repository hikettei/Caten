(in-package :caten/avm)

(defgeneric %impl (device op graph node args) (:documentation "Peforms the corresponding nodes"))

(defmethod %impl :around (device-id (op (eql :Allocate)) graph node args)
  (let ((from (getattr node :from)))
    ;; See ajit/type-relay.lisp: we need to feed a fake-array to relay-checker instead of the realized buffer.
    (if (or (null from) (eql device-id :relay-checker))
	(call-next-method)
	(progn
	  (assert (or (symbolp from) (buffer-p from)) () ":from attribute for ~a must be a realized buffer! (check the graph construction process.)" node)
	  (if (buffer-p from)
	      (progn (report-allocation t (buffer-dtype from) (buffer-shape from)) from)
	      (let ((val (vm/readvar *vm* from)))
		(if (buffer-p val)
		    (progn (report-allocation t (buffer-dtype val) (buffer-shape val)) val)
		    (error "When Processing ~a, the variable ~a should be declared in the VM, and it should be a variable, getting ~a." node from val))))))))

(defmethod %impl (device-id (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride)
      (parse-allocate-node node args)
    (flet ((->number (x) (if (buffer-p x) (buffer-value x) x)))
      (let ((memory-pool (getattr node :pool))) ;; the second run of :Allocation?
        (when (and (buffer-p memory-pool) shape)
          (when (equal (map 'list #'->number shape) (buffer-shape memory-pool)) ;; dynamic shape can changed the demanded size.
             (report-allocation t (buffer-dtype memory-pool) (buffer-shape memory-pool))
             (return-from %impl memory-pool))
          ;; TODO(hikettei) free the old memory allocation
          ))
      (setf (getattr node :pool)
            (realize-buffer graph (node->id node)
	                    :shape1 (map 'list #'->number shape)
		            :stride1 (map 'list #'->number stride)))
      (report-allocation nil (buffer-dtype (getattr node :pool)) (buffer-shape (getattr node :pool)))
      (getattr node :pool))))
