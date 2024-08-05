(in-package :caten/ajit)

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

(defun parse-isl-ast (backend ex kernel explore-outermost-until count)
  (declare (type Kernel kernel))
  (with-inlined-foreign-funcall-mode
    (let ((type (%"isl_ast_node_get_type":isl-ast-node-type :pointer ex)))
      ;;(print type)
      (ecase type
	(:isl_ast_node_error
	 (error ":isl-ast-node-error"))
	(:isl_ast_node_for
	 (parse-isl-ast-for backend ex kernel explore-outermost-until count))
	(:isl_ast_node_if
	 ;;(parse-isl-ast-if    backend ex kernel)
	 (error "Not implemented: parse-isl-ast-if")
	 )
	(:isl_ast_node_block
	 (parse-isl-ast-block backend ex kernel explore-outermost-until count))
	(:isl_ast_node_mark
	 ;;(parse-isl-ast-mark  backend ex kernel)
	 "")
	(:isl_ast_node_user
	 (parse-isl-ast-user  backend ex kernel))))))

(defun parse-isl-ast-block (backend ex kernel explore-outermost-until count)
  (with-inlined-foreign-funcall-mode
    (let* ((children (%"isl_ast_node_block_get_children":pointer :pointer ex))
	   (n        (%"isl_ast_node_list_n_ast_node":int :pointer children)))
      (codegen-write-block
       backend
       (loop for i upfrom 0 below n
	     for child = (%"isl_ast_node_list_get_at":pointer :pointer children :int i)
	     collect
	     (parse-isl-ast backend child kernel explore-outermost-until count))
       kernel))))

(defun replace-body-with-new-ids (body new-ids)
  (flet ((shuffle-helper (aref)
	   (map-tree
	    #'(lambda (form)
		(if (or (symbolp form)
			(keywordp form))
		    (or (gethash form new-ids)
			form)
		    form))
	    aref)))
    (trivia:ematch body
      ;; (setf (aref X ...) scalar)
      ((list
	(or 'setf 'incf 'decf 'mulcf 'divcf)
	(list* 'aref (or (type symbol) (type keyword)) _)
	(or (type symbol) (type number)))
       (let ((aref (shuffle-helper (second body)))
	     (new-expr (copy-list body)))
	 (setf (second new-expr) aref)
	 new-expr))
      ;; (setf (aref X ...) (aref X ...)
      ((list
	(or 'setf 'incf 'decf 'mulcf 'divcf)
	(list* 'aref (or (type symbol) (type keyword)) _)
	(list* 'aref (or (type symbol) (type keyword)) _))
       (let ((aref (shuffle-helper (second body)))
	     (arg  (shuffle-helper (third  body))))
	 `(,(car body)
	   ,aref
	   ,arg)))	
      ;; ((or setf incf decf mulcf divcf) (aref X ...) (op ...)
      ((list
	(or 'setf 'incf 'decf 'mulcf 'divcf)
	(list* 'aref (or (type symbol) (type keyword)) _)
	(list* (type symbol) _))
       (let ((aref (shuffle-helper (second body)))
	     (args (map 'list #'shuffle-helper (cdr (third body)))))
	 `(,(car body)
	   ,aref
	   (,(car (third body)) ,@args)))))))

(defun parse-isl-ast-user (backend ex kernel)
  (declare (type foreign-pointer ex)
	   (type kernel kernel)
	   (type keyword backend))
  (with-inlined-foreign-funcall-mode
    (let ((expr (%"isl_ast_node_user_get_expr":pointer :pointer ex)))
      (%"isl_ast_expr_free":void :pointer expr)
      (let* ((first-expr (%"isl_ast_expr_op_get_arg":pointer :pointer expr :int 0))
	     (n          (%"isl_ast_expr_get_op_n_arg":int   :pointer expr))
	     (id         (prog1
			     (%"isl_ast_expr_id_get_id":pointer :pointer first-expr)
			   (%"isl_ast_expr_free":void :pointer first-expr)))
	     (name       (prog1
			     (%"isl_id_get_name":string :pointer id)
			   (%"isl_id_free":void :pointer id))))
	(loop for instruction across (kernel-instructions kernel)
	      if (string= (format nil "~(~a~)" (inst-op instruction)) (string-downcase name)) do
		(let ((old-ids
			(loop for domain across (kernel-domains kernel)
			      append
			      (loop for iname in (inst-depends-on instruction)
				    if (eql iname (domain-subscript domain))
				      collect iname)))
		      (new-ids (make-hash-table :test #'eql)))
		  (loop for i upfrom 1  below n
			for old-id   in old-ids
			for arg = (parse-isl-expr
				   backend
				   (%"isl_ast_expr_op_get_arg":pointer :pointer expr :int i)
				   kernel)
			do (setf (gethash old-id new-ids) arg))
		  ;;(maphash
		  ;; #'(lambda (k v)
		  ;;     (format t "~a -> ~a ~%" k v))
		  ;; new-ids)
		  (let ((replaced-body
			  (replace-body-with-new-ids (inst-body instruction) new-ids)))
		    ;; (setf (aref X ...) scalar)
		    (return-from
		     parse-isl-ast-user
		      (trivia:ematch replaced-body
			((list
			  (or 'setf 'incf 'decf 'mulcf 'divcf)
			  (list* 'aref (or (type symbol) (type keyword)) _)
			  (or (type symbol) (type number)))
			 (let* ((callexpr (intern (symbol-name (first replaced-body)) "KEYWORD"))
				(target   (second replaced-body))
				(target-buffer (find-if #'(lambda (x) (eql x (second target))) (kernel-args kernel) :key #'buffer-name))
				(source   (third  replaced-body)))
			   (codegen-write-set-scalar
			    backend
			    callexpr
			    replaced-body
			    target-buffer
			    source
			    kernel)))
			;; (setf (aref X ...) (aref X ...)
			((list
			  (or 'setf 'incf 'decf 'mulcf 'divcf)
			  (list* 'aref (or (type symbol) (type keyword)) _)
			  (list* 'aref (or (type symbol) (type keyword)) _))
			 (let* ((callexpr (intern (symbol-name (first replaced-body)) "KEYWORD"))
				(target   (second replaced-body))
				(target-buffer (find-if #'(lambda (x) (eql x (second target))) (kernel-args kernel) :key #'buffer-name))
				(source   (third replaced-body))
				(source-buffer (find-if #'(lambda (x) (eql x (second source))) (kernel-args kernel) :key #'buffer-name)))
			   (codegen-write-array-move
			    backend
			    callexpr
			    replaced-body
			    target-buffer
			    source-buffer
			    kernel)))
			;; ((or setf incf decf mulcf divcf) (aref X ...) (op ...)
			((list
			  (or 'setf 'incf 'decf 'mulcf 'divcf)
			  (list* 'aref (or (type symbol) (type keyword)) _)
			  (list* (type symbol) _))
			 (let* ((callexpr (intern (symbol-name (first replaced-body)) "KEYWORD"))
				(target   (second replaced-body))
				(target-buffer (find-if #'(lambda (x) (eql x (second target))) (kernel-args kernel) :key #'buffer-name))
				(sources  (cdr (third replaced-body)))
				(source-buffers (loop for src in sources
						      if (and (listp src) (eql (car src) 'aref))
							collect
							(find-if #'(lambda (x) (eql x (second src))) (kernel-args kernel) :key #'buffer-name)
						      else
							collect src)))
			   (codegen-write-instruction
			    backend
			    callexpr
			    replaced-body
			    target-buffer
			    source-buffers
			    kernel))))))))))))

(defun parse-isl-expr (backend ast kernel &key (determine-upper-bound-p nil))
  (declare (type foreign-pointer ast))
  (with-inlined-foreign-funcall-mode
    (let ((type (%"isl_ast_expr_get_type":isl-ast-expr-type :pointer ast)))
      (ecase type
	(:isl_ast_expr_error
	 (error ":isl_ast_expr_error"))
	(:isl_ast_expr_id
	 (let* ((id (%"isl_ast_expr_id_get_id":pointer :pointer ast))
		(name (prog1
			  (%"isl_id_get_name":string :pointer id)
			(%"isl_id_free":void :pointer id))))
	   (codegen-write-id backend name kernel)))
	(:isl_ast_expr_int
	 (let* ((id (%"isl_ast_expr_int_get_val":pointer :pointer ast))
		(num (prog1
			 (%"isl_val_get_d":double :pointer id)
		       (%"isl_val_free":void :pointer id))))
	   (let ((num (round num)))
	     (codegen-write-num backend num kernel))))
	(:isl_ast_expr_op
	 (let ((n-arg (%"isl_ast_expr_get_op_n_arg":int :pointer ast)))
	   (assert (or (= n-arg 1) (= n-arg 2)) () "Assertion Failed with nargs == 1 or 2")
	   (multiple-value-bind (lhs rhs)
	       (values
		(parse-isl-expr backend (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 0) kernel)
		(when (= n-arg 2)
		  (parse-isl-expr backend (%"isl_ast_expr_op_get_arg":pointer :pointer ast :int 1) kernel)))
	     (let ((op-type (%"isl_ast_expr_op_get_type":isl-ast-expr-op-type :pointer ast)))
	       (assert (not (eql op-type :isl_ast_expr_op_error)) () ":isl_ast_expr_op_error")
	       (when determine-upper-bound-p
		 ;; i < 10, here 10 is needed if determine-up-bound-p is set to t
		 (ecase op-type
		   ;; a < b
		   ;; ->
		   ;; a <= b-1
		   (:isl_ast_expr_op_lt ;; a < b
		    (return-from parse-isl-expr
		      (if (numberp (read-from-string rhs))
			  (codegen-write-num backend (1- (read-from-string rhs)) kernel)
			  (codegen-write-binary-op
			   backend
			   :-
			   rhs
			   (codegen-write-num backend 1 kernel)
			   kernel))))
		   ;; <=
		   (:isl_ast_expr_op_le ;; a <= b
		    (return-from parse-isl-expr rhs))))
	       (ecase op-type
		 (:isl_ast_expr_op_and
		  ;; LHS && RHS
		  (codegen-write-binary-op backend :and lhs rhs kernel))
		 (:isl_ast_expr_op_and_then
		  (codegen-write-binary-op backend :and lhs rhs kernel))
		 (:isl_ast_expr_op_or
		  ;; LHS || RHS
		  (codegen-write-binary-op backend :or lhs rhs kernel))
		 (:isl_ast_expr_op_or_else
		  (codegen-write-binary-op backend :or lhs rhs kernel))
		 ;; maxmin
		 (:isl_ast_expr_op_max
		  (codegen-write-binary-op backend :max lhs rhs kernel))
		 (:isl_ast_expr_op_min
		  (codegen-write-binary-op backend :min lhs rhs kernel))
		 
		 ;; (- a)
		 (:isl_ast_expr_op_minus
		  (codegen-write-minus backend lhs kernel))

		 ;; Binary_Ops
		 (:isl_ast_expr_op_add
		  (codegen-write-binary-op backend :+ lhs rhs kernel))
		 (:isl_ast_expr_op_sub
		  (codegen-write-binary-op backend :- lhs rhs kernel))
		 (:isl_ast_expr_op_mul
		  (codegen-write-binary-op backend :* lhs rhs kernel))
		 (:isl_ast_expr_op_div
		  (codegen-write-binary-op backend :/ lhs rhs kernel))
		 
		 (:isl_ast_expr_op_fdiv_q
		  (codegen-write-binary-op backend :floor-div-cast-to-int lhs rhs kernel))
		 (:isl_ast_expr_op_pdiv_q
		  (codegen-write-binary-op backend :/ lhs rhs kernel))		  
		 (:isl_ast_expr_op_pdiv_r
		  (codegen-write-binary-op backend :% lhs rhs kernel))
		 (:isl_ast_expr_op_zdiv_r
		  (codegen-write-binary-op backend :% lhs rhs kernel))
		 ;;(:isl_ast_expr_op_cond)
		 ;;(:isl_ast_expr_op_select)
		 (:isl_ast_expr_op_eq
		  (codegen-write-binary-op backend :equal lhs rhs kernel))
		 (:isl_ast_expr_op_le
		  (codegen-write-binary-op backend :<= lhs rhs kernel))
		 (:isl_ast_expr_op_lt
		  (codegen-write-binary-op backend :< lhs rhs kernel))
		 (:isl_ast_expr_op_ge
		  (codegen-write-binary-op backend :<= lhs rhs kernel))
		 (:isl_ast_expr_op_gt
		  (codegen-write-binary-op backend :> lhs rhs kernel))
		 ;;(:isl_ast_expr_op_call)
		 ;;(:isl_ast_expr_op_access)
		 ;;(:isl_ast_expr_op_member)
		 ;;(:isl_ast_expr_op_address_of)
		 )))))))))


(defun parse-isl-ast-for (backend ex kernel explore-outermost-until count)
  (with-inlined-foreign-funcall-mode
    (let* ((execute-once (%"isl_ast_node_for_is_degenerate":boolean :pointer ex))
	   (iter         (%"isl_ast_node_for_get_iterator":pointer  :pointer ex))
	   (id           (%"isl_ast_expr_get_id":pointer :pointer iter))
	   (name         (prog1
			     (%"isl_id_get_name":string :pointer id)
			   (%"isl_id_free":void :pointer id)))
	   (from          (parse-isl-expr 
			   backend
			   (%"isl_ast_node_for_get_init":pointer
			     :pointer ex)
			   kernel))
	   (by           (parse-isl-expr
			  backend
			  (%"isl_ast_node_for_get_inc":pointer :pointer ex)
			  kernel))
	   (to           (parse-isl-expr
			  backend
			  (%"isl_ast_node_for_get_cond":pointer :pointer ex)
			  kernel
			  :determine-upper-bound-p t))
	   ;; For comparison
	   (from1          (parse-isl-expr
			    :lisp
			    (%"isl_ast_node_for_get_init":pointer
			      :pointer ex)
			    kernel))
	   (by1           (parse-isl-expr
			   :lisp
			   (%"isl_ast_node_for_get_inc":pointer :pointer ex)
			   kernel))
	   (to1           (parse-isl-expr
			   :lisp
			   (%"isl_ast_node_for_get_cond":pointer :pointer ex)
			   kernel
			   :determine-upper-bound-p t))
	   (outermost-p  (or
			  ;; Explored all explore-outermost-until iterations, but there was no any loops that can be parallelized
			  ;;  -> try the last one.
			  (and count
			       (= (1- count) explore-outermost-until)
			       (let* ((from (read-from-string from1))
				      (to   (read-from-string to1))
				      (by   (read-from-string by1))
				      ;; [TODO] Improve how to determine cost-threshold
				      (cost-threshold (cl-cpus:get-number-of-processors))
				      (itersize
					(and
					 (listp to)
					 (string= (format nil "~a" (car to)) "MIN")
					 (listp to)
					 (listp (third to))
					 (not (null (third to)))
					 (third (third to)))))
				 ;; As long as the loop is tiled; try it.
				 ;; If the iteration is tiled;
				 ;; from = c0
				 ;; to = (min c0 (+. c0 itersize))
				 ;; by = nothing but number
				 ;; (print from)
				 ;; (print to)
				 ;; (print by)
				 (and
				  (numberp by)
				  (symbolp from)
				  (numberp itersize)
				  (>= itersize cost-threshold))))
			  ;; Finding the iteration which is enough large that parallelized.
			  (and
			   count
			   (<= count explore-outermost-until)
			   (let ((from (read-from-string from1))
				 (to   (read-from-string to1))
				 (by   (read-from-string by1)))
			     (and
			      (numberp from)
			      (numberp to)
			      (numberp by)
			      ;; The iteration is worth to parallelize?
			      ;; TODO: Compute the more accurate cost-threshold
			      (let ((itersize (/ (- to from) by))
				    (cost-threshold (cl-cpus:get-number-of-processors)))
				(>= itersize cost-threshold)))))))
	   (body          (with-deeper-indent
			    (parse-isl-ast
			     backend
			     (%"isl_ast_node_for_get_body":pointer :pointer ex)
			     kernel
			     explore-outermost-until
			     (if outermost-p ;; If outermost was found -> do not parallelize the subsequent loops
				 nil
				 (and count (1+ count)))))))
      (codegen-write-for
       backend kernel
       name from to by body execute-once outermost-p))))

