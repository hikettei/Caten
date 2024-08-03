(in-package :caten/ajit)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;  A Set of CFFI Bindings for ISL and utils for it.
;; (Which complements the lack of functions in the cl-isl binding)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; ~ CFFI Pointer Wrappers  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct isl-obj ptr)
(defun wrap-with-pointer (ptr) (make-isl-obj :ptr ptr))
(defcfun ("isl_ctx_alloc" %isl-ctx-alloc) :pointer)
(defcfun ("isl_ctx_free" %isl-ctx-free) :void (ctx :pointer))

(defstruct isl-ctx ptr objects)
(defun register-alloc-obj (base-obj object)
  (when *isl-context*
    (push (cons base-obj object) (isl-ctx-objects *isl-context*))))

(declaim (ftype (function () isl-ctx) isl-ctx-alloc))
(defun isl-ctx-alloc () (make-isl-ctx :ptr (%isl-ctx-alloc)))
(defun isl-ctx-free (ctx)
  (loop for (specializer . ptr) in (isl-ctx-objects ctx)
	do (free specializer (isl-obj-ptr ptr)))
  (%isl-ctx-free (isl-ctx-ptr ctx)))

(defparameter *isl-context* nil "A place to bind isl-ctx")
(defmacro with-isl-ctx (&body body)
  "Binds context"
  `(let ((*isl-context* (isl-ctx-alloc)))
     (unwind-protect
	  (progn ,@body)
       (isl-ctx-free *isl-context*))))
;; ~~ Defcfun helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmacro define-isl-function (name-cffi return &rest args)
  (let* ((name (intern (replace-string (string-upcase name-cffi) #\_ #\-)))
	 (cffi (symb '% name)))
    `(progn
       (defcfun (,name-cffi ,cffi) ,return
	 ,@(loop for arg in args
		 for bind = (first arg)
		 for type = (second arg)
		 if (eql type :context)
		   collect `(,bind :pointer)
		 else
		   collect arg))
       (defun ,name (,@(map 'list #'car args))
	 (,(if (eql return :pointer)
	       'wrap-with-pointer
	       'progn)	 
	  (,cffi
	   ,@(loop for arg in args
		   for bind = (first arg)
		   for type = (second arg)
		   if (eql type :pointer)
		     collect `(isl-obj-ptr ,bind)
		   else if (eql type :context)
		      collect `(isl-ctx-ptr ,bind)
		   else
		     collect bind)))))))
;; ~ CFFI Functions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-isl-function "isl_set_read_from_str" :pointer
  (ctx :context)
  (str :string))

(define-isl-function "isl_union_set_read_from_str" :pointer
  (ctx :context)
  (str :string))

(define-isl-function "isl_union_map_dump" :void
  (union-map :pointer))

(define-isl-function "isl_union_set_copy" :pointer
  (set :pointer))

(define-isl-function "isl_union_map_union" :pointer
  (map1 :pointer)
  (map2 :pointer))
  
(define-isl-function "isl_union_map_read_from_str" :pointer
  (ctx :context)
  (x :string))

(define-isl-function "isl_union_set_intersect" :pointer
  (ctx :context)
  (a :pointer)
  (b :pointer))

(defcenum :isl-schedule-node-type
  (:isl_schedule_node_error 1)
  :isl_schedule_node_band
  :isl_schedule_node_context
  :isl_schedule_node_domain
  :isl_schedule_node_expansion
  :isl_schedule_node_extension
  :isl_schedule_node_filter
  :isl_schedule_node_leaf
  :isl_schedule_node_guard
  :isl_schedule_node_mark
  :isl_schedule_node_sequence
  :isl_schedule_node_set)

(defcenum :isl-dim-type
  :isl_dim_cst
  :isl_dim_param
  :isl_dim_in
  :isl_dim_out
  :isl_dim_set
  :isl_dim_div
  :isl_dim_all)

 
