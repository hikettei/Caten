(in-package :caten/avm)

(defgeneric %impl (device op graph node args) (:documentation "Peforms the corresponding nodes"))

(defmethod %impl :around (device-id (op (eql :Allocate)) graph node args)
  (let ((from (getattr node :from)))
    ;; See ajit/type-relay.lisp: we need to feed a fake-array to relay-checker instead of the realized buffer.
    (if (or from (not (eql device-id :relay-checker)))
	(progn
	  (assert (buffer-p from) () ":from attribute for ~a must be a realized buffer! (check the graph construction process.)" node)
	  from)
	(call-next-method))))
