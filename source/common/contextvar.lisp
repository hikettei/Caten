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
   #:with-contextvar))
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
			  collect `(,slot-name (if (uiop:getenv ,(symbol-name (car slot)))
						   (ecase ,(third slot)
						     (:int
						      (let ((val (read-from-string (uiop:getenv ,(symbol-name (car slot))))))
							(if (integerp val)
							    val
							    (progn
							      (warn "Caten/common.contextvar: ~a should be an integer but got ~a, setting the default value." ',(car slot) val)
							      ,default))))
						     (:string (uiop:getenv ,(symbol-name (car slot)))))
						   ,default)
					       :type ,dtype)))
		,@(loop for slot in slots
			collect
			`(defmethod getenv ((id (eql ,(car slot))))
			   (assert (contextvar-p *ctx*) () "Caten/common.contextvar: *ctx* is not initialized, or recompiled after changing the slots, getting ~a." *ctx*)
			   (let ((val (slot-value *ctx* ',(intern (symbol-name (car slot))))))
			     (funcall #',(fourth slot) val))))
		,@(loop for slot in slots
			collect
			`(defmethod (setf getenv) (value (id (eql ,(car slot))))
			   (setf (slot-value *ctx* ',(intern (symbol-name (car slot))))
				 (ecase ,(third slot)
				   (:int
				    (let ((val (read-from-string (format nil "~a" value))))
				      (if (integerp val)
					  val
					  (error "Caten/common.contextvar: ~a should be an integer but got ~a" ',(car slot) value))))
				   (:string
				    (assert (stringp value) () "Caten/common.contextvar: ~a should be a string but got ~a" ',(car slot) value)
				    value)))))
		(defun help (&optional
			       (stream t)
			     &aux (max
				   ,(apply
				     #'max
				     (map
				      'list
				      #'(lambda (x)
					  (length (format nil "  ~a[~a] (default: ~a):" (car x) (second x) (third x))))
				      slots))))
		  (format stream "~%CONTEXTVAR:~%~a"
			  (with-output-to-string (out)
			    ,@(loop for slot in slots
				    for size = (length (format nil "  ~a[~a] (default: ~a):" (car slot) (second slot) (third slot)))
				    collect `(format out "  ~a[~(~a~)] (default: ~a)~a ~a~%"
						     (log:maybe-ansi log::cyan (format nil "~a" ',(car slot)))
						     (log:maybe-ansi log::gray (format nil "~a" ,(third slot)))
						     ,(second slot)
						     (with-output-to-string (out) (dotimes (i (+ 2 (- max ,size))) (princ " " out)))
						     (log:maybe-ansi log::white (format nil "~a" ,(fifth slot))))))))
		(defmacro with-contextvar ((&key
					      ,@(loop for slot in slots
						      for accessor = (intern (format nil (string-upcase "contextvar-~a") (car slot)))
						      collect
						      `(,(intern (symbol-name (car slot))) (,accessor *ctx*))))
					   &body body)
		  `(let ((*ctx* (make-contextvar
				 ,,@(loop for slot in slots for name = (intern (symbol-name (car slot)))
					  append
					  (list (car slot) `(if (keywordp ,name)
								(symbol-name ,name)
								,name))))))
		     ,@body)))))
  (defcontext
      ;; (ENV_NAME DEFAULT_VALUE DTYPE DESCRIPTION)
      (:DEBUG
       0 :int #.(oneof "DEBUG" 0 `(-1 0))
       "Set -1 to disable logger")	 
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
    (:DOT
     0 :int #.(oneof "DOT" 0 `(0 1))
     "Set 1 to generate a lowered dot graph and opening in the default browser. (requirement: graphviz")
    (:CI
     0 :int identity
     "Set 1 if the test is running under Github Actions")
    (:AUTO_SCHEDULER
     1 :int #.(oneof "AUTO_SCHEDULER" 1 `(0 1))
     "Set 1 to enable auto-scheduler for JIT")
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
     "For AOT, please set the list of devices that will execute AOT compilation. The devices specified here must have all Renderers implemented. (e.g.: AOT=CLANG,METAL)")
    (:AOT_VM
     "" :string parse-list->kw
     "For AOT_VM, please set the list of devices that will execute AOT compilation. The devices configured here are implemented through a VM, not a Renderer (i.e., devices without JIT implementation).")
    (:PACKED
     1 :int #.(oneof "PACKED" 1 `(0 1))
     "Set 1 to allow jit to generate packed-funcall")
    (:CALL_ZENITY
     0 :int #.(oneof "CALL_ZENITY" 0 `(0 1))
     "(For JIT) %render-compile always produce an simple-error.")
    (:COLOR
     1 :int #.(oneof "COLOR" 1 `(0 1))
     "Use cl-ansi-color if set to 1")
    (:SAFETY
     0 :int #.(oneof "SAFETY" 0 `(0 1))
     "When this parameter is set to 1, caten/air checks that all dependencies are satisfied during each process of Graph Rewriting. (i.e.: FastGraph becomes Graph) This directly impacts the compilation speed.")))

(defparameter *ctx* (make-contextvar))
