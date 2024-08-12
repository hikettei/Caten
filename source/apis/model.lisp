(in-package :caten/apis)

(defclass Model () nil)
(defgeneric call (model &rest inputs))
(defmethod st/impl ((module Model) &rest inputs)
  (flet ((detach (x) (st "A[~] -> A[~]" (x))))
    (apply #'values (map 'list #'detach (multiple-value-list (apply #'call module inputs))))))
(defmacro defmodel ((name (&rest initargs) &key (where nil) (documentation "")) (&rest slots) &body body)
  "Define a model. (A simplified version of defmodule)"
  (let* ((initarg-names (collect-initargs-names initargs))
	 (initarg-kws   (map 'list #'(lambda (x) (intern (string-upcase (format nil "~a" x)) "KEYWORD")) initarg-names))
	 (attr-form (loop for kw in initarg-kws
			  for nth upfrom 0
			  for nm = (nth nth initarg-names)
			  append (list kw nm)))
	 (slot-names (map 'list #'car slots))
	 (slot-forms (map 'list (compose #'car #'cdr) slots))
	 (defclass-slots (map 'list #'(lambda (x) `(,(car x) ,@(cddr x))) slots)))
    (assert (every #'(lambda (x) (null (find :initform x))) defclass-slots)
	    ()
	    "defmodel: :initform would be overwritten")
    (with-gensyms (_initargs)
      `(progn
	 (defmodule (,name ((,@initargs) ,@attr-form) :where ,where :direct-superclasses (Model))
	     (,@defclass-slots)
	     :documentation ,documentation
	     ,@(when (null where) `(:forward st/impl))
	     :impl call)
	 (defmethod initialize-instance :after ((,name ,name) &rest ,_initargs &key &allow-other-keys)
	   (declare (ignore ,_initargs))
	   (with-attrs (,@(map 'list #'(lambda (x) `(,x ,(intern (symbol-name x) "KEYWORD"))) initarg-names)) ,name
	     (let* (,@(loop for name in slot-names
			    for form in slot-forms
			    collect `(,name ,form)))
	       ,@(loop for slot-name in slot-names
		       collect `(setf (slot-value ,name ',slot-name) ,slot-name))
	       ,@body)))))))
