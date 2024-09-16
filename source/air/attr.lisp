(in-package :caten/air)

;; TODO: Pattern Matcherも通るようにしたい
;; TODO: "BinaryOps"でAttr Compare

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *allow-undefined-attribute* t)  
  (defgeneric attribute->instance (module attr))
  (defmethod attribute->instance :around (module attr)
    (if (next-method-p)
	(call-next-method)
	(if *allow-undefined-attribute*
	    :default
	    (error "Undefined Attribute: ~a/~a" module attr))))
  (defclass Attribute () nil)
  (defmacro defattr ((module type) (&rest direct-superclasses) description &key (verify #'identity) (slots))
    "Defines a new attribute."
    (declare (type keyword module type)
	     (type string description))
    (let* ((class-name (intern (format nil "~a-attr" (symbol-name type)))))
      `(progn
	 (defmethod attribute->instance ((module (eql ,module)) (id (eql ,type))) ',class-name)
	 (defclass ,class-name (Attribute ,@direct-superclasses)
	   ,@slots
	   (:documentation ,description))
	 (defmethod get-verifier ((attr ,class-name)) ,verify)
	 (defun make-attr (module type &rest args)
	   (let ((instance-key (attribute->instance module type)))
	     (if (eql instance-key :default)
		 t ;; [TODO]
		 (apply #'make-instance instance-key args))))))))
