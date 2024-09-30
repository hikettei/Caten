(in-package :caten/air)

(eval-when (:compile-toplevel :load-toplevel :execute)

(defparameter *attribute->instance* (make-hash-table))
(defgeneric attribute->instance (attr))
(defmethod attribute->instance ((attr symbol))
  (if (next-method-p)
      (call-next-method)
      nil))
(defmethod attribute->instance :around ((attr symbol))
  (if (next-method-p)
      (call-next-method)
      nil))

(defgeneric dump-into-list (attr &key (allow-unbound)))

(defclass Attribute ()
  ((attr-module-key :initarg :attr-module-key :type keyword :reader attr-module-key)
   (attr-type-key :initarg :attr-type-key :type keyword :reader attr-type-key)))

(defmethod make-load-form ((attr Attribute) &optional (env))
  (declare (ignore env))
  `(make-attr ,(attr-type-key attr) ,@(dump-into-list attr)))

(defun rewrite-slot (slot)
  (assert (null (find :initarg slot)) () "defattr: do not specify :initarg")
  (assert (listp slot) () "defattr: ~a is not a list." slot)
  (let ((name (intern (symbol-name (car slot)) "KEYWORD")))
    (append slot `(:initarg ,name))))

(defgeneric %getattr (attr id))
(defgeneric %setattr (attr id value))
(defgeneric %boundp (attr id))
(defgeneric %attr-initargs (attr-id))
(defgeneric %get-output-to (attr &rest reads))
(defgeneric verify-args (attr writes reads))

(defun find-attr (attr-key)
  (declare (type keyword attr-key))
  (multiple-value-bind (module instance-key) (attribute->instance attr-key)
    (declare (ignore module))
    (assert instance-key () "The node :~a is not defined by `defnode`." attr-key)
    instance-key))

(defun build-documentation (name document nth &rest direct-superclasses)
  (with-output-to-string (out)
    (format out "~a" document)
    (format out "~%When optimizing ~(~a~) in-place, the ~ath read is consumed.~%" name nth)
    (when direct-superclasses
      (format out "~%superclasses = ")
      (dolist (superclass direct-superclasses)
	(format out "`~a`, " superclass)))))

(defmacro defnode ((class type) (&rest direct-superclasses) description &key (placeholder 0) (verify 'identity) (slots))
  "
Defines a new node.

```lisp
(defnode (class type) (&rest direct-superclasses)
  description
  &key (placeholder 0) (verify 'identity) (slots))
```

- class[keyword] an identifier of the node class.
- type[keyword] an identifier of the node type.
- direct-superclasses[list of keyword] a list of superclasses.
- description[string] a description of the node.
- placeholder[(unsigned-byte 32) or -1] when mutating the node in-place, the compiler consumes the placeholder-th read buffer.
- verify[function] a function to verify the arguments.
- slots[list of (symbol &rest slot-options)] a list of slot definitions.
"
  (declare (type keyword class type)
	   (type string description))
  (let* ((class-name (intern (format nil "~a-ATTR" (symbol-name type)))))
    (dolist (superclass direct-superclasses)
      (let ((superclasses (c2mop:class-direct-superclasses (class-of superclass))))
	(assert (= (length superclasses) 1) () "defnode[~a]: Multiple inheritance of superclass[~a] is not allowed because it is not dumped."
		type superclass)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (gethash ,type *attribute->instance*) (cons ,class ',class-name))
       (defmethod attribute->instance ((id (eql ,type))) (values ,class ',class-name))
       (defclass ,class-name (Attribute ,@direct-superclasses)
	 ,(loop for slot in slots
		for slot-new = (rewrite-slot slot)
		collect slot-new)
	 (:documentation ,(apply #'build-documentation type description placeholder direct-superclasses)))
       (defmethod %attr-initargs ((attr-id (eql ,type)))
	 "Returns a list of possible :attrs argument keywords."
	 (list
	  ,@(loop for slot in slots
                  for slot-name = (car slot)
                  for slot-key = (intern (symbol-name slot-name) "KEYWORD")
                  collect slot-key)
	  ,@(loop for class in `(,@direct-superclasses)
		  for class-of = (find-class class)
		  append
		  (loop
		    for slot in (c2mop:class-direct-slots class-of)
		    for slot-initargs = (c2mop:slot-definition-initargs slot)
		    for initarg = (when (and (= 1 (length slot-initargs)) (keywordp (car slot-initargs))) (car slot-initargs))
		    if initarg
		      collect initarg))))
       (defmethod verify-args ((attr ,class-name) writes reads) (funcall #',verify (list attr writes reads)))
       ,@(loop for slot in slots
	       for slot-name = (car slot)
	       for slot-key = (intern (symbol-name slot-name) "KEYWORD")
	       collect
	       `(defmethod %getattr ((attr ,class-name) (id (eql ,slot-key)))
		  (and (slot-boundp attr ',slot-name) (slot-value attr ',slot-name)))
	       collect
	       `(defmethod %boundp ((attr ,class-name) (id (eql ,slot-key))) (slot-boundp attr ',slot-name))
	       collect
	       `(defmethod %setattr ((attr ,class-name) (id (eql ,slot-key)) value)
		  (declare (optimize (safety 3)))
		  (setf (slot-value attr ',slot-name) value)))
       ,@(loop for class in direct-superclasses
	       for class-of = (find-class class)
	       append
	       (loop
		    for slot in (c2mop:class-direct-slots class-of)
		    for slot-initargs = (c2mop:slot-definition-initargs slot)
		    for initarg = (when (and (= 1 (length slot-initargs)) (keywordp (car slot-initargs))) (car slot-initargs))
		    for slotname = (c2mop:slot-definition-name slot)
		    if initarg
		      collect
		    `(defmethod %boundp ((attr ,class-name) (id (eql ,initarg))) (slot-boundp attr ',slotname))
		    if initarg
		      collect
		    `(defmethod %getattr ((attr ,class-name) (id (eql ,initarg))) (slot-value attr ',slotname))
		    if initarg
		      collect
		    `(defmethod %setattr ((attr ,class-name) (id (eql ,initarg)) value)
		       (declare (optimize (safety 3)))
		       (setf (slot-value attr ',slotname) value))))
       (defmethod dump-into-list ((attr ,class-name) &key (allow-unbound t))
	 (declare (ignore allow-unbound))
	 (list
	  ,@(loop for slot in slots
                  for slot-name = (car slot)
                  for slot-key = (intern (symbol-name slot-name) "KEYWORD")
                  append (list slot-key `(and (slot-boundp attr ',slot-name) (slot-value attr ',slot-name))))
	  ,@(loop for class in `(,@direct-superclasses)
		  for class-of = (find-class class)
		  append
		  (loop
		    for slot in (c2mop:class-direct-slots class-of)
		    for slot-initargs = (c2mop:slot-definition-initargs slot)
		    for initarg = (when (and (= 1 (length slot-initargs)) (keywordp (car slot-initargs))) (car slot-initargs))
		    for slotname = (c2mop:slot-definition-name slot)
		    if initarg
		      append (list initarg `(and (slot-boundp attr ',slotname) (slot-value attr ',slotname)))))))
       (defmethod %get-output-to ((attr ,class-name) &rest reads) (if (= ,placeholder -1) nil (nth ,placeholder reads))))))

(defun get-output-to (node)
  (declare (type node node))
  (apply #'%get-output-to (node-attr node) (node-reads node)))

(defmethod dump-into-list :around ((attr Attribute) &key (allow-unbound t))
  (let ((attrs (call-next-method)))
    (if allow-unbound
	attrs
	(loop for i upfrom 0 to (1+ (/ (length attrs) 2)) by 2
	      if (and (keywordp (nth i attrs)) (%boundp attr (nth i attrs)))
		collect (nth i attrs)))))

(defun make-attr (type &rest args)
  (multiple-value-bind (module instance-key) (attribute->instance type)
    (assert (and module instance-key) () "make-attr: The node :~a is not defined by `defnode`." type)
    (apply #'make-instance instance-key :attr-module-key module :attr-type-key type args)))

(defun debug/attrs-by-module ()
  (let ((module->val (make-hash-table)))
    (maphash
     #'(lambda (key val)
	 (if (null (gethash (car val) module->val))
	     (setf (gethash (car val) module->val) (list (cons key (cdr val))))
	     (push (cons key (cdr val)) (gethash (car val) module->val))))
     *attribute->instance*)
    module->val))

(defun debug/render-defined-nodes (&key (ignore `(:Testing :TMP)))
  (with-output-to-string (out)
    (let ((module->val (debug/attrs-by-module)))
      (maphash
       #'(lambda (module nodes)
	   (when (null (find module ignore))
	     (format out "  class[:~a]:~%" module)
	     (dolist (node nodes)
	       (format out "    - :~a~%" (car node)))))
       module->val)
      (dolist (ign ignore)
	(format out "  class[:~a]:~%    - <Ignored>~%" ign)))))

(defun node-build-documentation-by-class (title class-id)
  (declare (type string title) (type keyword class-id))
  (with-output-to-string (out)
    (format out "## ~a~%~%" title)
    (let ((module->val (debug/attrs-by-module)))
      (maphash
       #'(lambda (module vals)
	   (when (eql module class-id)
	     (dolist (val vals)
	       (multiple-value-bind (id class) (values (car val) (cdr val))
		 (format out "~%### :~a~%~%" id)
		 (format out (documentation (find-class class) t))))))
       module->val))))

) ;; eval-when

