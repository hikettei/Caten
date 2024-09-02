(cl:in-package :cl-user)
(defpackage :caten/common.contextvar
  (:documentation "Helpers for context var.
Usage:
(ctx:getenv :SERIALIZE) -> 1
(setf (ctx:getenv :SERIALIZE) 1)
(help) -> full documentation")
  (:nicknames :ctx)
  (:use :cl :cl-ppcre)
  (:export
   #:*ctx*
   #:help
   #:getenv
   #:with-contextvar
   ))
(in-package :caten/common.contextvar)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun oneof (name default &rest options)
    `(lambda (x)
       (when (null (find x ',@options))
	 (warn "ContextVar: ~a expects one of ~a butgot ~a, setting ~a" ',name ',options x ',default)
	 (setf x ',default))
       x))
  (defun oneof-kw (name default &rest options)
    `(lambda (x &aux (x (intern x "KEYWORD")))
       (when (null (find x ',@options))
	 (warn "ContextVar: ~a expects one of ~a butgot ~a, setting ~a" ',name ',options x ',default)
	 (setf x ',default))
       x))
  (defun parse-list->kw (string)
    (declare (type string string))
    (map 'list #'(lambda (x) (intern (regex-replace-all " " x "") "KEYWORD")) (split "," string))))

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
			   (assert (contextvar-p *ctx*) () "Caten/common.contextvar: *ctx* is not initialized, or recompiled after changing the slots, getting ~a." *ctx*)
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
			`(defmethod (setf getenv) (value (id (eql ,(car slot))))
			   (setf (uiop:getenv ,(symbol-name (car slot))) (format nil "~a" value))))
		(defun help (&optional (stream t))
		  (format stream "ContextVar:~%~a"
			  (with-output-to-string (out)
			    ,@(loop for slot in slots
				    collect `(format out "~a[~a] (default: ~a), ~a~%" ',(car slot) ,(third slot) ,(second slot) ,(fifth slot)))))))))
  (defcontext
      ;; (ENV_NAME DEFAULT_VALUE DTYPE DESCRIPTION)
      (:SERIALIZE
       0 :int
       (lambda (x)
	 (when (not (or (= x 0) (= x 1))) (warn "SERIALIZE is specified as 0 or 1, setting 0. got ~a" x) (setf x 0))
	 x)
       "See: caten/ajit:auto-schedule. Value is specified as 0(true) or 1(false).")
      (:JIT_DEBUG
       0 :int
       (lambda (x)
	 (when (not (typep x '(integer 0 4))) (warn "JIT_DEBUG should be an integer from 0 to 3, got ~a, setting 0" x) (setf x 0))
	 x)
       "DEBUG-Level during jit-compilation")
    (:JIT
     0 :int identity
     "Set 1 to use JIT_BACKEND, 0 to use VM_BACKEND")
    (:JIT_BACKEND
     "CLANG" :string
     (lambda (x) (intern x "KEYWORD"))
     "Default backend (JIT)")
    (:AVM
     "LISP" :string
     (lambda (x) (intern x "KEYWORD"))
     "Default backend (VM)")
    (:STATIC_GENSYM
     1 :int identity
     "If this option is set to 1, the temporary variables generated by (gensym) are uniquely determined (e.g.: val_n). If set to 0, uses gensym. (e.g.: tidXXXX)")
    (:DEFAULT_FLOAT
     "FLOAT32" :string
     #.(oneof-kw "DEFAULT_FLOAT" :float32 `(:float64 :float32 :float16 :bfloat16))
     "Default float to use (one of :FLOAT64, :FLOAT32, :FLOAT16, :BFLOAT16)")
    (:DEFAULT_INT
     "INT32" :string
     #.(oneof-kw "DEFAULT_INT" :int32 `(:int64 :int32 :int16 :int8))
     "A default dtype to use as a (iconst x)")
    (:DEFAULT_UINT
     "UINT32" :string
     #.(oneof-kw "DEFAULT_UINT" :uint32 `(:uint64 :uint32 :uint16 :uint8))
     "A default dtype to use as a (uconst x)")
    (:DEFAULT_ORDER
     "ROW" :string
     #.(oneof-kw "DEFAULT_ORDER" :row `(:row :column))
     "A default memory-layout used to compute the strides.")     
    (:ANIMATE
     1 :int
     #.(oneof "ANIMATE" 1 `(0 1))
     "Set 0 to disable the animated progress bar of common/tqdm.lisp")
    (:CC
     "gcc" :string identity
     "Default C Compiler")
    (:OMP
     0 :int #.(oneof "OMP" 0 `(0 1))
     "Set 1 to use omp.h for Clang")
    (:AOT_VERBOSE
     0 :int #.(oneof "AOT_VERBOSE" 0 `(0 1))
     "When this option is set to 1, it prints all the information that AIR deletes during AOT compilation.")
    (:AOT
     "" :string parse-list->kw
     "For AOT, please set the list of devices that will execute AOT compilation. The devices specified here must have all Renderers implemented.
(e.g.: AOT=CLANG,METAL)")
    (:AOT_VM
     "" :string parse-list->kw
     "For AOT_VM, please set the list of devices that will execute AOT compilation. The devices configured here are implemented through a VM, not a Renderer (i.e., devices without JIT implementation).")
    (:CALL_ZENITY
     0 :int #.(oneof "CALL_ZENITY" 0 `(0 1))
     "(For JIT) %render-compile always produce an simple-error.")))

(defparameter *ctx* (make-contextvar))
(defmacro with-contextvar ((&rest configurations) &body body)
  `(let ((*ctx* (make-contextvar ,@configurations)))
     ,@body))
