(in-package :caten/air)
;; [TODO]
;; out-toの統一
(defgeneric attribute->instance (attr))

(defmethod attribute->instance :around (attr)
  (if (next-method-p)
      (call-next-method)
      (error "Undefined Attribute: ~a Defined attributes are ..." attr)))

(defclass Attribute () nil)
;; (defmethod make-load-form ((attr Attribute)))

(defun rewrite-slot (slot)
  (assert (null (find :initarg slot)) () "defattr: do not specify :initarg")
  (assert (listp slot) () "defattr: ~a is not a list." slot)
  (let ((name (intern (symbol-name (car slot)) "KEYWORD")))
    (append slot `(:initarg ,name))))

(defgeneric %getattr (attr id))
(defgeneric %setattr (attr id value))
(defgeneric get-output-to (attr &rest reads))

(defun build-documentation (name document nth &rest direct-superclasses)
  (with-output-to-string (out)
    (format out "~a" document)
    (format out "~%When optimizing ~(~a~) in-place, the ~ath read is consumed.~%" name nth)
    (when direct-superclasses
      (dolist (superclass direct-superclasses)
	(format out "### [Attribute] ~(~a~)~%" superclass)
	(format out "~a~%~%" (documentation (find-class superclass) t))))))

(defmacro defnode ((module type) (&rest direct-superclasses) description &key (write-to 0) (verify 'identity) (slots))
  "Defines a new attribute."
  (declare (type keyword module type)
	   (type string description))
  (let* ((class-name (intern (format nil "~a-ATTR" (symbol-name type)))))
    `(progn
       (defmethod attribute->instance ((id (eql ,type))) ',class-name)
       (defclass ,class-name (Attribute ,@direct-superclasses)
	 ,(loop for slot in slots
		for slot-new = (rewrite-slot slot)
		collect slot-new)
	 (:documentation ,(apply #'build-documentation type description write-to direct-superclasses)))
       (defmethod get-verifier ((attr ,class-name)) #',verify)
       ,@(loop for slot in slots
	       for slot-name = (car slot)
	       for slot-key = (intern (symbol-name slot-name) "KEYWORD")
	       collect
	       `(defmethod %getattr ((attr ,class-name) (id (eql ,slot-key)))
		  (slot-value attr ',slot-name))
	       collect
	       `(defmethod %setattr ((attr ,class-name) (id (eql ,slot-key)) value)
		  (declare (optimize (safety 3)))
		  (setf (slot-value attr ',slot-name) value)))
       (defmethod get-output-to ((attr ,class-name) &rest reads) (nth ,write-to reads)))))

(defun make-attr (type &rest args)
  (let ((instance-key (attribute->instance type)))
    (apply #'make-instance instance-key args)))
