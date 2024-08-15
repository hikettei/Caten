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
	      from
	      (let ((val (vm/readvar *vm* from)))
		(if (buffer-p val)
		    val
		    (error "When Processing ~a, the variable ~a should be declared in the VM, and it should be a variable, getting ~a." node from val))))))))
