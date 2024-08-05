(cl:in-package :cl-user)
(defpackage :caten/common.contextvar
  (:documentation "Helpers for context var.
Usage:
(ctx:getenv :SERIALIZE) -> 1
(setf (ctx:getenv :SERIALIZE) 1)
(help) -> full documentation")
  (:nicknames :ctx)
  (:use :cl)
  (:export
   #:*ctx*
   #:help
   #:getenv
   #:with-contextvar
   ))
(in-package :caten/common.contextvar)

(macrolet ((defcontext (&rest slots)
	     (assert (every
		      #'(lambda (x)
			  (and
			   (= (length x) 5)
			   (keywordp (car x))
			   (find (third x) `(:int :string))
			   (ecase (third x)
			     (:int (integerp (second x)))
			     (:string (stringp (second x))))
			   (stringp (fifth x))))
		      slots)			  
		     ()
		     "Slots = (ENV_NAME DEFAULT_VALUE DTYPE(:=string or int) ASSERTION DESCRIPTION)")
	     `(progn
		(defstruct ContextVar
		  ,@(loop for slot in slots
			  for slot-name = (intern (symbol-name (car slot)))
			  for default   = (second slot)
			  for dtype = (ecase (third slot) (:int 'fixnum) (:string 'string))
			  collect `(,slot-name ,default :type ,dtype)))
		;; (setf getenv)
		,@(loop for slot in slots
			collect
			`(defmethod getenv ((id (eql ,(car slot))))
			   (assert (contextvar-p *ctx*) () "Caten/common.contextvar: *ctx* is not initialized, getting ~a." *ctx*)
			   (let ((val (uiop:getenv ,(symbol-name (car slot)))))
			     (funcall
			      #',(fourth slot)
			      (if val
				  ,(ecase (third slot)
				     (:int
				      `(let ((val (read-from-string val)))
					 (if (integerp val)
					     val
					     (progn
					       (warn "Caten/common.contextvar: ~a should be an integer but got ~a, setting the default value." ',(car slot) val)
					       nil))))
				     (:string `(progn val)))
				  (slot-value *ctx* ',(intern (symbol-name (car slot)))))))))
		,@(loop for slot in slots
			collect
			`(defmethod (setf getenv) ((id (eql ,(car slot))) value)
			   (setf (uiop:getenv ,(symbol-name (car slot))) (format nil "~a" value))))
		(defun help (&optional (stream t))
		  (format stream "ContextVar:~%~a"
			  (with-output-to-string (out)
			    ,@(loop for slot in slots
				    collect `(format out "~a[~a] (default: ~a), ~a~%" ',(car slot) ,(third slot) ,(second slot) ,(fifth slot)))))))))
  (defcontext
      ;; (ENV_NAME DEFAULT_VALUE DTYPE DESCRIPTION)
      (:SERIALIZE
       1 :int
       (lambda (x)
	 (when (not (or (= x 0) (= x 1))) (warn "SERIALIZE is specified as 0 or 1, setting 0. got ~a" x) (setf x 0))
	 x)
       "See: caten/ajit:auto-schedule. Value is specified as 0(true) or 1(false).")
      (:JIT_DEBUG
       0 :int
       (lambda (x)
	 (when (not (typep x '(integer 0 3))) (warn "JIT_DEBUG should be an integer from 0 to 3, got ~a, setting 0" x) (setf x 0))
	 x)
       "DEBUG-Level during jit-compilation")))

(defparameter *ctx* (make-contextvar))
(defmacro with-contextvar ((&rest configurations) &body body)
  `(let ((*ctx* (make-contextvar ,@configurations))
	 (*use-default* t))
     ,@body))
