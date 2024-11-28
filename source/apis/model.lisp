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

(defmacro defcall ((model-bind model) (&rest inputs) &body body)
  "
```
(defcall (model-bind model) (&rest inputs) body0
```
"
  (let* ((where (princ-to-string inputs))
         (where (when inputs (subseq where 1 (1- (length where)))))
         (wt (when where (%parse-st (format nil "~a -> ~a" where where))))
         (wt (if where wt (make-st "" nil nil)))
         (keys (when wt (remove-duplicates (flatten (map 'list #'at-shape (st-bf wt)))))))
    (with-gensyms (inputs solved)
      `(defmethod call ((,model-bind ,model) &rest ,inputs)
         (assert (= ,(length (st-bf wt)) (length ,inputs))
                 ()
                 "(call ~a &rest inputs): The number of inputs does not match defined inputs ~a~%call is defined as: ~a" ',model ,inputs ,where)
         ,(when where `(st ,(format nil "~a -> ~a" where where) (,inputs)))
         (multiple-value-bind (,@(map 'list (compose #'intern #'princ-to-string #'at-name) (st-bf wt))) (apply #'values ,inputs)
           (let ((,solved ,(when where `(%solve-st (%parse-st ,(format nil "~a -> ~a" where where)) nil nil :tensors ,inputs :return-solved t))))
             (declare (ignorable ,solved))
             (let (,@(loop for key in keys collect `(,(intern (princ-to-string key)) (gethash ,key ,solved))))
               (declare (ignorable ,@(map 'list (compose #'intern #'princ-to-string) keys)))
               ,@body)))))))
