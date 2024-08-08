(in-package :caten/ajit)
;; ~ CFFI Pointer Wrappers  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct isl-obj ptr)
(defun wrap-with-pointer (ptr) (make-isl-obj :ptr ptr))
(defcfun ("isl_ctx_alloc" %isl-ctx-alloc) :pointer)
(defcfun ("isl_ctx_free" %isl-ctx-free) :void (ctx :pointer))

(defstruct isl-ctx ptr)
(declaim (ftype (function () isl-ctx) isl-ctx-alloc))
(defun isl-ctx-alloc () (make-isl-ctx :ptr (%isl-ctx-alloc)))
(defun isl-ctx-free (ctx) (%isl-ctx-free (isl-ctx-ptr ctx)))

(defparameter *isl-context* (isl-ctx-alloc) "A place to bind isl-ctx")
(defun isl-error ()
  (error "isl yields an error: ~a"
	 (cffi:foreign-string-to-lisp
	  (foreign-funcall "isl_ctx_last_error_msg" :pointer (isl-ctx-ptr *isl-context*) :string)
	  :encoding :ascii)))

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
       (defun ,name (,@(loop for arg in args
			     for bind = (first arg)
			     for type = (second arg)
			     unless (eql type :context)
			       collect bind))
	 (assert *isl-context* () "*isl-context* is not initialized.")
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
		      collect `(isl-ctx-ptr *isl-context*)
		   else
		     collect bind)))))))
;; ~ Set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-isl-function "isl_set_read_from_str" :pointer
  (ctx :context)
  (str :string))

;; ~~ Union Set ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-isl-function "isl_union_set_read_from_str" :pointer
  (ctx :context)
  (str :string))

(define-isl-function "isl_union_set_copy" :pointer
  (set :pointer))

(define-isl-function "isl_union_set_intersect" :pointer
  (a :pointer)
  (b :pointer))

(define-isl-function "isl_union_set_to_str" :string
  (union-set :pointer))

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

(define-isl-function "isl_union_map_dump" :void
  (union-map :pointer))

(define-isl-function "isl_union_map_union" :pointer
  (map1 :pointer)
  (map2 :pointer))
  
(define-isl-function "isl_union_map_read_from_str" :pointer
  (ctx :context)
  (x :string))

(define-isl-function "isl_union_map_copy" :pointer
  (map :pointer))

(define-isl-function "isl_union_access_info_from_sink" :pointer
  (map :pointer))

(define-isl-function "isl_union_access_info_set_must_source" :pointer
  (access :pointer)
  (map :pointer))

(define-isl-function "isl_union_access_info_set_schedule" :pointer
  (access :pointer)
  (map :pointer))

(define-isl-function "isl_union_access_info_compute_flow" :pointer
  (access :pointer))

(define-isl-function "isl_union_flow_get_must_dependence" :pointer
  (access :pointer))
;; ~~ SCHEDULERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-isl-function "isl_schedule_from_domain" :pointer
  (domain :pointer))

(define-isl-function "isl_schedule_sequence" :pointer
  (s1 :pointer)
  (s2 :pointer))

(define-isl-function "isl_schedule_dump" :void (s1 :pointer))

(define-isl-function "isl_union_access_info_set_may_source" :pointer
  (access   :pointer)
  (may-read :pointer))

(define-isl-function "isl_union_flow_get_may_dependence" :pointer
  (flow :pointer))

(define-isl-function "isl_multi_union_pw_aff_read_from_str" :pointer
  (ctx :pointer)
  (x   :string))

(define-isl-function "isl_schedule_constraints_set_validity" :pointer
  (schedule :pointer)
  (map :pointer))

(define-isl-function "isl_schedule_constraints_set_proximity" :pointer
  (schedule :pointer)
  (map :pointer))

(define-isl-function "isl_schedule_constraints_on_domain" :pointer
  (schedule :pointer))
;; ~~ AST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-isl-function "isl_ast_build_from_context" :pointer (set :pointer))
(define-isl-function "isl_ast_build_node_from_schedule" :pointer
  (x :pointer)
  (y :pointer))

(define-isl-function "isl_ast_node_get_ctx" :pointer
  (ast :pointer))

(define-isl-function "isl_printer_to_str" :pointer
  (printer :pointer))

(define-isl-function "isl_printer_set_output_format" :pointer
  (printer :pointer)
  (format :int))

(define-isl-function "isl_printer_print_ast_node" :pointer
  (p :pointer)
  (ast :pointer))

(define-isl-function "isl_printer_get_str" :string
  (ast :pointer))

(define-isl-function "isl_schedule_node_n_children"
  :int
  (ptr :pointer))

;; ~~ ISL AST ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defcenum :isl-ast-node-type
  (:isl_ast_node_error -1)
  (:isl_ast_node_for 1)
  :isl_ast_node_if
  :isl_ast_node_block
  :isl_ast_node_mark
  :isl_ast_node_user)

(defcenum :isl-ast-expr-type
  (:isl_ast_expr_error -1)
  :isl_ast_expr_op
  :isl_ast_expr_id
  :isl_ast_expr_int)

(defcenum :isl-ast-expr-op-type
  (:isl_ast_expr_op_error -1)
  :isl_ast_expr_op_and
  :isl_ast_expr_op_and_then
  :isl_ast_expr_op_or
  :isl_ast_expr_op_or_else

  ;; maxmin
  :isl_ast_expr_op_max
  :isl_ast_expr_op_min

  ;; (- a)
  :isl_ast_expr_op_minus

  ;; Binary_Ops
  :isl_ast_expr_op_add
  :isl_ast_expr_op_sub
  :isl_ast_expr_op_mul
  :isl_ast_expr_op_div
  
  :isl_ast_expr_op_fdiv_q
  :isl_ast_expr_op_pdiv_q
  :isl_ast_expr_op_pdiv_r
  :isl_ast_expr_op_zdiv_r
  :isl_ast_expr_op_cond
  :isl_ast_expr_op_select
  :isl_ast_expr_op_eq
  :isl_ast_expr_op_le
  :isl_ast_expr_op_lt
  :isl_ast_expr_op_ge
  :isl_ast_expr_op_gt
  :isl_ast_expr_op_call
  :isl_ast_expr_op_access
  :isl_ast_expr_op_member
  :isl_ast_expr_op_address_of)
