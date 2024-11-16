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
     "Select either 0 or -1. Set -1 to supress the caten/common.logger")
    (:JIT_DEBUG
     0 :int
     (lambda (x)
       (when (not (typep x '(integer 0 5))) (warn "JIT_DEBUG should be an integer from 0 to 5, got ~a, setting 0" x) (setf x 0))
       x)
     "Choose a value from 0 to 5. Gradually specifies the level of debugging when executing with JIT=1 (setting JIT_DEBUG >= 2 is recommended).")
    (:NOOPT
     0 :int #.(oneof "NOOPT" 0 `(0 1))
     "Select either 0 or 1. Disables Loop Collapse during JIT execution.")
    (:DOT
     0 :int #.(oneof "DOT" 0 `(0 1 2))
     "Choose from 0, 1, or 2. Setting it to 1 opens the computation graph in a default browser when lowering the AST; setting it to 2 does so when running the scheduler. (Requirement: graphviz)")
    (:CI
     0 :int identity
     "Set to 1 to indicate that it runs on GitHub Actions.")
    (:AUTO_SCHEDULER
     0 :int #.(oneof "AUTO_SCHEDULER" 1 `(0 1))
     "Set to 1 to optimize using caten/codegen/polyhedral during JIT execution.")
    (:JIT
     0 :int identity
     "Set to 1 to run the graph with the JIT codegen.")
    (:JIT_BACKEND
     "CLANG" :string
     (lambda (x) (intern x "KEYWORD"))
     "The backend used during JIT execution.")
    (:AVM
     "LISP" :string
     (lambda (x) (intern x "KEYWORD"))
     "The backend used during AVM execution.")
    (:DEFAULT_FLOAT
     "FLOAT32" :string
     #.(oneof-kw "DEFAULT_FLOAT" :float32 `(:float64 :float32 :float16 :bfloat16))
     "Declares the default FLOAT type. Selected from :FLOAT64, :FLOAT32, :FLOAT16, and :BFLOAT16")
    (:DEFAULT_INT
     "INT64" :string
     #.(oneof-kw "DEFAULT_INT" :int64 `(:int64 :int32 :int16 :int8))
     "Declares the default INT type. Selected from :int64, :INT32, :INT16, and :INT8.")
    (:DEFAULT_UINT
     "UINT64" :string
     #.(oneof-kw "DEFAULT_UINT" :uint64 `(:uint64 :uint32 :uint16 :uint8))
     "Declares the default UINT type. Selected from :UINT64, :UINT32, :UINT16, and :UINT8.")
    (:DEFAULT_ORDER
     "ROW" :string
     #.(oneof-kw "DEFAULT_ORDER" :row `(:row :column))
     "Declare the default memory layouts. Selected from ROW or COLUMN")
    (:ANIMATE
     1 :int
     #.(oneof "ANIMATE" 1 `(0 1))
     "Select either 0 or 1. Setting it to 0 suppresses animations in caten/common/tqdm.lisp.")
    (:CC
     "gcc" :string identity
     "The default GCC compiler used by the CLANG backend.")
    (:OMP
     0 :int #.(oneof "OMP" 0 `(0 1))
     "Set 1 to use OpenMP by the CLANG backend.")
    (:AOT_VERBOSE
     0 :int #.(oneof "AOT_VERBOSE" 0 `(0 1))
     "Set to 1 to debug the AIR dump during AOT compilation.")
    (:AOT
     "" :string parse-list->kw
     "Declare all JIT device names capable of performing AOT compilation. (e.g.: AOT=CLANG,METAL)")
    (:AOT_VM
     "" :string parse-list->kw
     "Declare all VM device names capable of performing AOT compilation. (e.g.: AOT_VM=LISP)")
    (:COLOR
     0 :int #.(oneof "COLOR" 0 `(0 1))
     "Set 1 to use cl-ansi-color")
    ;; [TODO]remove
    (:SAFETY
     0 :int #.(oneof "SAFETY" 0 `(0 1))
     "Set 1 to always use Graph instead of FastGraph.")
    (:PROFILE_SIMPLIFIER
     0 :int #.(oneof "PROFILE_SIMPLIFIER" 0 `(0 1))
     "Set 1 to profile the simplifier in %make-graph-from-iseq")))

(defparameter *ctx* (make-contextvar))
