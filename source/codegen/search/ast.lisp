(defpackage :caten/codegen/search/ast
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/air :caten/aasm :caten/isl :caten/codegen/search/polyhedral :caten/codegen/search/directive)
  (:export
   #:apply-schedule))
(in-package :caten/codegen/search/ast)
;; ~~ ISL AST Generation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun gid (n) (intern (format nil "_gid_p~a" n)))

(defun compute-ast-from-schedule (schedule)
  "Build an ISL AST node from a schedule under fixed AST-build options.
Inputs:
  schedule : isl::schedule S
Effect:
  Copies S and constructs an isl_ast_build with options:
    ast_build_atomic_upper_bound=1, detect_min_max=1, exploit_nested_bounds=1,
    separation_bounds=0, prefer_pdiv=0, scale_strides=1, allow_else=0, allow_or=0.
  The iterator list is set to (gid_0,…,gid_{r−1}) and used when emitting loops.
rank is an optional parameter to gain better scheduled code. (c0, c1 indexing -> _gid0, _gid1, indexing)
Returns:
  isl::ast-node corresponding to S."
  (declare (type isl::schedule schedule))
  (macrolet ((set-option (name level)
	       `(cffi:foreign-funcall ,(format nil "isl_options_set_~(~a~)" name) :pointer (isl::context-handle isl::*context*) :int ,level  :void)))
    (set-option "ast_build_atomic_upper_bound" 1)
    (set-option "ast_build_group_coscheduled" 1)
    (set-option "ast_build_detect_min_max" 1)
    (set-option "ast_build_separation_bounds" 0)
    (set-option "ast_build_exploit_nested_bounds" 1)
    (set-option "ast_build_prefer_pdiv" 0)
    (set-option "ast_build_scale_strides" 1)
    (set-option "ast_build_allow_else" 0) ;; caten does not support else
    (set-option "ast_build_allow_or" 0))
  (let* ((schedule (isl:copy schedule))
         (rank
           (caten/codegen/search/schedule:schedule-node-count-bands
            (schedule-get-root schedule)))
	 (ast-build (isl:ast-build-from-context (isl:set-from-str "{:}")))
         (ast-build
           (isl:ast-build-set-iterators
            ast-build
            (apply #'isl:make-id-list (loop for i upfrom 0 below rank collect (gid i)))))
         (ast-build-node (isl:ast-build-node-from-schedule ast-build schedule)))
    ast-build-node))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Variable/Context Counter
(defstruct (parse-ctx
            (:constructor make-parse-ctx (blueprint ctx))
            (:conc-name pctx-))
  (blueprint blueprint :type Graph)
  (gid2range (make-hash-table) :type hash-table)
  (gid2offset (make-hash-table) :type hash-table)
  (variable-table (make-hash-table) :type hash-table)
  (scop-ctx ctx :type ctx)
  (expr2args (make-hash-table) :type hash-table)
  (band-cnt 0 :type fixnum)
  (gensym-counter 0 :type fixnum))

(defun pctx-gensym (pctx)
  (declare (ignore pctx))
  (intern (format nil "var_~a" (gensym))))

(defun pctx-register-gid (pctx id range offset)
  (declare (type parse-ctx pctx) (type symbol id))
  (labels ((find-suite (i cnt)
             (if (gethash i (pctx-gid2range pctx))
                 (find-suite (intern (format nil "~a_~a" id cnt)) (1+ cnt))
                 i)))
    (let ((registered-as (find-suite id 1)) (new-ctx (copy-parse-ctx pctx)))
      (setf (gethash registered-as (pctx-gid2range new-ctx)) range
            (gethash registered-as (pctx-gid2offset new-ctx)) offset
            (pctx-variable-table new-ctx) (alexandria:copy-hash-table (pctx-variable-table new-ctx))
            (gethash id (pctx-variable-table new-ctx)) registered-as)
      (values new-ctx registered-as))))
;; ~~ AST Parse (ISL Level) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun parse-isl-ast (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-node-get-type ast)))
    (ecase type
      (:ast-node-error (isl::isl-error))
      (:ast-node-for   (parse-isl-ast-for ctx ast))
      (:ast-node-if    (parse-isl-ast-if ctx ast))
      (:ast-node-block (parse-isl-ast-block ctx ast))
      (:ast-node-mark  (parse-isl-ast-mark ctx ast))
      (:ast-node-user  (parse-isl-ast-user ctx ast)))))

(defun parse-isl-ast-mark (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (incf (pctx-band-cnt ctx))
  (let* ((directive (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id ast)))))
         (user (parse-isl-ast ctx (isl::%isl-ast-node-mark-get-node ast)))
         (depth (directive-depth directive))
         (band-id (intern (format nil "B~a" (1- (pctx-band-cnt ctx))))))
    (labels ((rec (node count)
               (declare (type node node node) (type fixnum count))
               ;; (assert (eql (node-type node) :FOR))
               (when (not (eql (node-type node) :FOR))
                 ;; (warn "Skiped applying mark because the child is not :FOR")
                 (return-from parse-isl-ast-mark user))
               (setf (getattr node :band) band-id
                     (getattr node :directive) directive) ;; multiple directives can be applied
               (when (< count depth) (rec (id->value *ctx* (second (node-reads node))) (1+ count)))))
      (rec user 1))
    user))

(defun parse-isl-ast-block (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((children (isl::%isl-ast-node-block-get-children ast))
	 (n        (isl::%isl-ast-node-list-n-ast-node children)))
    (apply
     #'%progn
     (loop for i upfrom 0 below n
           for child = (isl::%isl-ast-node-list-get-at children i)
	   collect (parse-isl-ast ctx child)))))

(defun parse-isl-ast-if (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((condition (parse-isl-expr ctx (isl::%isl-ast-node-if-get-cond ast) :toplevel-p nil))
	 (then-node (parse-isl-ast ctx (isl::%isl-ast-node-if-get-then-node ast)))
	 (else-p (isl::%isl-ast-node-if-has-else-node ast)))
    (assert (not (eql else-p :bool-true)) () "Else statement is not allowed!")
    (%if condition then-node)))

(defun parse-isl-ast-cond (ctx ast idx)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-expr-get-type ast)))
    (assert (eql type :ast-expr-op))
    (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
           (args (loop for nth upfrom 0 below n-arg collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg ast nth) :toplevel-p nil)))
           (op-type (isl::%isl-ast-expr-op-get-type ast)))
      (multiple-value-bind (lhs rhs) (apply #'values args)
        (assert (= 2 (length args)))
        (assert (and (eql (node-type lhs) :LOAD) (eql (getattr lhs :value) idx)))
        ;; Assuming: gid < size
        (case op-type
          (:ast-expr-op-le (%add rhs (%iconst 1 :dtype :int64)))
          (:ast-expr-op-lt rhs)
          (otherwise (error "parse-isl-ast-cond: The loop bound should be atomic! did you turned on ast_build_atomic_upper_bound=1?")))))))

(defun parse-isl-ast-for (ctx ast)
  (declare (type cffi:foreign-pointer ast))
  (let* ((iter (isl::%isl-ast-node-for-get-iterator ast))
	 (id (isl::%isl-ast-expr-get-id iter))
	 (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	 (from (parse-isl-expr ctx (isl::%isl-ast-node-for-get-init ast) :toplevel-p nil))
	 (by (parse-isl-expr ctx (isl::%isl-ast-node-for-get-inc ast) :toplevel-p nil))
	 (to (parse-isl-ast-cond ctx (isl::%isl-ast-node-for-get-cond ast) (intern name)))
         (rid (gensym "R")))
    (multiple-value-bind (new-ctx gid) (pctx-register-gid ctx (intern name) rid from)
      ;; [TODO] by >= 1 assertion
      (let ((body (parse-isl-ast new-ctx (isl::%isl-ast-node-for-get-body ast))))
        (%range gid (%sub to from) body :step by :rid rid)))))

(defun parse-isl-expr (ctx ast &key (toplevel-p t))
  (declare (type cffi:foreign-pointer ast))
  (let* ((type (isl::%isl-ast-expr-get-type ast)))
    (funcall
     (if toplevel-p #'(lambda (x) (%expr (node->id x))) #'identity)
     (ecase type
       (:ast-expr-error (isl::isl-error))
       (:ast-expr-id
        (let* ((id (isl::%isl-ast-expr-id-get-id ast))
	       (name (intern (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id))))
               (is-gid (gethash name (pctx-variable-table ctx))))
          (if is-gid
              (let ((rid (gethash is-gid (pctx-gid2range ctx)))
                    (offset (gethash is-gid (pctx-gid2offset ctx))))
                (assert (and rid offset))
                (if (eql offset 0)
                    rid
                    (%add rid (if (numberp offset) (%iconst offset :dtype :int64) offset))))
              (%iconst name :dtype :int64))))
       (:ast-expr-int
        (let* ((id (isl::%isl-ast-expr-int-get-val ast))
	       (num (isl::%isl-val-get-d id)))
	  (declare (type number num))
          (%iconst num :dtype :int64)))
       (:ast-expr-op
        (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
	       (args (loop for nth upfrom 0 below n-arg collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg ast nth) :toplevel-p nil)))
	       (op-type (isl::%isl-ast-expr-op-get-type ast)))
	  (flet ((->expr (lhs rhs)
		   (assert (not (eql op-type :ast-expr-op-error)) () ":isl_ast_expr_op_error")
		   (ecase op-type
		     (:ast-expr-op-and (%and lhs rhs))
		     (:ast-expr-op-and-then (%and lhs rhs))
		     (:ast-expr-op-or (%or lhs rhs))
		     (:ast-expr-op-or-else (%or lhs rhs))
		     (:ast-expr-op-max (%max lhs rhs))
		     (:ast-expr-op-min  (%min lhs rhs))
		     (:ast-expr-op-minus (%neg lhs)) ;; (- a)
		     (:ast-expr-op-add (%add lhs rhs))
		     (:ast-expr-op-sub (%sub lhs rhs))
		     (:ast-expr-op-mul (%mul lhs rhs))
		     (:ast-expr-op-div (%idiv lhs rhs))		 
		     (:ast-expr-op-fdiv-q (%idiv lhs rhs))
		     (:ast-expr-op-pdiv-q (%idiv lhs rhs))
		     (:ast-expr-op-pdiv-r (%mod lhs rhs))
		     (:ast-expr-op-zdiv-r (%mod lhs rhs))
		     ;; (:expr-op-cond)
		     (:ast-expr-op-eq (%= nil :row lhs rhs))
                     ;; Early Rewrite LE to simplify the expression
		     (:ast-expr-op-le (%< nil :row lhs (%add rhs (%iconst 1 :dtype :int64)))) ;; <=
		     (:ast-expr-op-lt (%< nil :row lhs rhs)) ;; <
		     (:ast-expr-op-ge (%not (%< nil :row lhs rhs))) ;; >=
		     (:ast-expr-op-gt (%> nil :row lhs rhs)) ;; >
		     ;; (:expr-op-call)
		     ;; (:expr-op-access)
		     ;; (:expr-op-member)
		     ;; (:expr-op-address-of)
		     (otherwise  (error "~a is not supported by caten" op-type)))))
	    (if (= (length args) 1)
	        (->expr (car args) nil)
	        (case op-type
		  (:ast-expr-op-select
		   (assert (= (length args) 3))
                   (apply #'%where args))
		  (otherwise
		   (reduce #'->expr args)))))))))))

(defun parse-isl-ast-user (ctx ast &aux (visited (make-hash-table)))
  (declare (type cffi:foreign-pointer ast))
  (let ((expr (isl::%isl-ast-node-user-get-expr ast)))
    (let* ((first-expr (isl::%isl-ast-expr-op-get-arg expr 0))
	   (n          (isl::%isl-ast-expr-get-op-n-arg expr))
	   (id         (isl::%isl-ast-expr-id-get-id first-expr))
	   (name       (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	   (args       (loop for i upfrom 1 below n collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg expr i) :toplevel-p nil)))
           (node
             (or
              (find name (graph-nodes (pctx-blueprint ctx)) :key (alexandria:compose #'symbol-name #'node-id) :test #'equalp)
              (error "The node ~a is not found in the given blueprint during applying schedule." name)))
           (node-to-loops (reverse (gethash (node-id node) (ctx-node-to-loops (pctx-scop-ctx ctx)))))
           (rewrite-map (make-hash-table))
           (new-idx-map (make-hash-table)))
      (assert node () "The node ~a is not found from original blueprint." name)
      (assert (= (length args) (length node-to-loops)) () "Inconsistent domain loop args size")
      (loop for base-domain in node-to-loops
            for new-args in args
            do (setf (gethash (getf base-domain :idx) rewrite-map) new-args))
      ;; Creating a clone of expr subgraph but indexes are replaced.
      (labels ((e (id &aux (node (id->value (pctx-blueprint ctx) id)))
                 (when (or (null node) (gethash (node-id node) visited)) (return-from e (gethash id new-idx-map id)))
                 (when (eql (node-type node) :EXPR) (return-from e (gethash id new-idx-map id)))
                 (when (eql (node-type node) :DEFINE-GLOBAL) (emit node) (return-from e id))
                 ;; 2 case using gid:
                 ;; - Reference to RANGE
                 ;; - LOAD(value)
                 (when (eql (node-type node) :RANGE)
                   (let ((new-space (gethash (getattr node :idx) rewrite-map)))
                     (assert new-space)
                     (let ((n (copy-node new-space))
                           (new-id (pctx-gensym ctx)))
                       (assert (= 1 (length (node-writes n))))
                       (setf (gethash (car (node-writes n)) new-idx-map) new-id
                             (node-writes n) (list new-id)
                             (node-id n) (gensym "NID"))
                       (emit n)
                       (return-from e new-id))))
                 (when (and (eql (node-type node) :LOAD) (gethash (getattr node :value) rewrite-map))
                   (let ((new-space (gethash (getattr node :value) rewrite-map)))
                     (let ((n (copy-node new-space)) (new-id (pctx-gensym ctx)))
                       (assert (= 1 (length (node-writes n))))
                       (setf (gethash (car (node-writes n)) new-idx-map) new-id
                             (node-writes n) (list new-id)
                             (node-id n) (gensym "NID"))
                       (emit n)
                       (return-from e new-id))))
                 ;; [TODO] Replace %RANGE here if exists
                 (setf (gethash (node-id node) visited) t)
                 (let ((node (copy-node node)) (new-id (pctx-gensym ctx)))
                   (setf (gethash (car (node-writes node)) new-idx-map) new-id
                         (node-id node) (gensym "NID")
                         (node-reads node) (map 'list #'e (node-reads node))
                         (node-writes node) (list new-id))
                   (emit node)
                   new-id)))
        (let ((node (copy-node node)))
          (setf (node-id node) (gensym "NID")
                (node-reads node) (map 'list #'e (node-reads node)))
          (emit node)
          (setf (gethash (node-id node) (pctx-expr2args ctx))
                (loop for arg in args collect (cons arg (caten/aasm::ast-make-subgraph *ctx* (car (node-writes arg))))))
          node)))))
;; ~~ AST Generation (Caten/AASM Level) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Mostly following function deals w/ extra buffer allocation caused by Loop Fission
;; For example, this transformation is possible:
;; ```
;; for i = 0..10
;;  float acc_0 = 0.0f; // S(i) in the schedule
;;  for j = 0..10
;;    ...
;; ```
;; ==>
;; ```
;; for i = 0..10
;;   float acc_0 = 0.0f;
;; for i = 0..10
;;   for j = 0..10
;;     ...
;; ```
;; When scalar value is scheduled across different code blocks, the compiler have to mutate
;; acc_0 as a tensor temporary buffer!.
;; [TODO]
;; If the tmp buffer access is [i] -> [i] (bidijective) there should be more optimization path!
(defun kernel-fixup-loop-fission-args (kernels) ;; todo: delete
  ;; Insert DEFINE-GLOBAL if the definition was separated by Loop Fission
  (labels ((id->value-from-kernels (id)
             (loop for k in kernels
                   for v = (id->value k id)
                   if v do (return-from id->value-from-kernels v)))
           (id->tensor-info (id &aux (acc (id->value-from-kernels id)) (node (id->value-from-kernels (car (node-reads acc)))))
             (ecase (node-type acc)
               (:DEFINE-GLOBAL
                (values (getattr acc :dtype) (getattr acc :pointer-p)))
               (:EXPR
                (let ((rel (car (relay-writes (read-type-relay node)))))
                  (values (tensor-relay-dtype rel) (> (tensor-relay-nrank rel) 0)))))))
    (dolist (kernel kernels)
      (dolist (undef-var (graph-get-undefined-variables kernel))
        (dolist (user (id->users kernel undef-var))
          (when (eql (node-type user) :BIND)
            (dolist (usr (id->users kernel (car (node-writes user))))
              (setf (node-reads usr) (map 'list #'(lambda (x) (if (eql (car (node-writes user)) x) (getattr user :value) x)) (node-reads usr))))
            (multiple-value-bind (dtype pointer-p) (id->tensor-info (getattr user :value))
              (insert-nodes kernel (list (%global (getattr user :value) dtype pointer-p))))))))))

(defun bp-rewrite-scalar->buffer (parse-ctx ctx kernels scal-ids &aux (extra-allocs))
  (declare (type list scal-ids))
  (when (null scal-ids) (return-from bp-rewrite-scalar->buffer))
  (let* ((scal-ids (remove-duplicates scal-ids))
         (contexts (map 'list #'make-scop-ctx-from-blueprint kernels))
         (expr-subgraphs
           (loop for ctx in contexts for kernel in kernels
                 append
                 (loop for expr in (reverse (ctx-exprs ctx))
                       collect (cons expr (caten/aasm::ast-expr-graph kernel expr))))))
    (dolist (scal-id scal-ids)
      (labels ((lookup (node)
                 (loop for ctx in contexts
                       if (gethash (node-id node) (ctx-node-to-loops ctx)) do
                         (return-from lookup (gethash (node-id node) (ctx-node-to-loops ctx)))))
               (find-expr-from-user (user)
                 (loop for expr in expr-subgraphs
                       if (find (node-id user) (graph-nodes (cdr expr)) :key #'node-id)
                         do (return-from find-expr-from-user (car expr))))
               (id->value-from-kernels (id)
                 (loop for k in kernels
                       for v = (id->value k id)
                       if v do (return-from id->value-from-kernels v)))
               (id->tensor-info (id &aux (acc (id->value-from-kernels id)) (node (id->value-from-kernels (car (node-reads acc)))))
                 (assert (eql :EXPR (node-type acc)))
                 (values (tensor-relay-dtype (car (relay-writes (read-type-relay node)))) (lookup acc) acc))
               (compute-idx (acc stride load-node
                             &aux
                               (acc (or (find-expr-from-user acc) acc))
                               (load-node (or (find-expr-from-user load-node) load-node))
                               (acc-args (gethash (node-id acc) (pctx-expr2args parse-ctx)))
                               (load-args (gethash (node-id load-node) (pctx-expr2args parse-ctx))))
                 (let ((args (subseq load-args 0 (length acc-args))))
                   (dolist (arg args)
                     (map 'list #'(lambda (x) (emit x)) (graph-nodes (cdr arg))))
                   (reduce
                    #'%add
                    (loop for arg in args for s in stride collect (%mul (%load (%salloc :dtype :int64) s) (car arg))))))
               (swpid (id suffix) (intern (format nil "~a_~a" id suffix)))
               (make-new-aref (id scal-id read-from acc stride load-node)
                 (with-context-nodes
                     (out (%aref
                           (emit (make-node :JIT :BIND (list (gensym)) (list scal-id) :value read-from))
                           (compute-idx acc stride load-node) :out id))))
               (make-new-aref-bind (id bind-as base-read acc stride load-node)
                 (with-context-nodes
                     (out (%aref (emit (make-node :JIT :BIND (list (gensym)) (list base-read) :value bind-as)) (compute-idx acc stride load-node) :out id))))
               (make-new-initializer (read-from acc stride form load-node)
                 (with-context-nodes
                     (out (%expr (node->id (%setf (%aref read-from (compute-idx acc stride load-node)) form)) :out scal-id)))))
        (multiple-value-bind (dtype loops acc) (id->tensor-info scal-id)
          ;; [TODO] Get Shape/Stride
          ;; [TODO] Create extra alloc inserted to tuned runtime graph
          (assert (and dtype loops acc) () "Could not find the definition of scalar ~a" scal-id)
          (dolist (blueprint kernels)
            (let* ((rewrite-context (gethash scal-id (ctx-scal->access ctx)))
                   (shape (getf rewrite-context :shape)) (stride (getf rewrite-context :strides))
                   (argname (swpid scal-id "tmp")) (defglobal (%global argname dtype t)) (count 0))
              (assert rewrite-context)
              (push (%alloc (length shape) shape stride :dtype dtype :id argname) extra-allocs)
              ;; Rewrite the definition of scal-id if it exists in current blueprint
              (insert-nodes blueprint (list defglobal))
              (when (id->value blueprint scal-id)
                ;; Rewrite {val_2 = EXPR(0.0)} -> {val_2 = (%aref val2_tmp ...)}
                (insert-nodes blueprint (make-new-initializer argname acc stride (car (node-reads acc)) acc)))
              ;; Rewrite the user of scal-id
              (labels ((newid (node x)
                         (if (eql x scal-id)
                             (let ((id (swpid scal-id count)))
                               (incf count)
                               (insert-nodes blueprint (make-new-aref id scal-id argname acc stride node))
                               id)
                             x)))
                (loop for node in (graph-nodes blueprint)
                      ;; Case1. the user of scal-id
                      if (or (eql (node-type node) :EXPR) (not (eql (node-class node) :Render)))
                        do (let ((node (copy-node node)))
                             (setf (node-reads node) (map 'list #'(lambda (x) (newid node x)) (node-reads node)))
                             (insert-nodes blueprint (list node)))
                           ;; Case2. BIND(val_8, value=val_2) (insert the bind itself)
                           ;; Rewrite :BIND if there is no accumlator (if there is accumlator, :DEFINE-GLOBAL won't be purged)
                      if (and (eql (node-type node) :BIND) (eql scal-id (getattr node :value)))
                        do (if (id->value blueprint (car (node-reads node))) ;; two case: the reductor is defined in the same group, or
                               (insert-nodes blueprint (make-new-aref-bind (car (node-writes node)) argname (car (node-reads node)) acc stride node))
                               (insert-nodes blueprint (make-new-aref (car (node-writes node)) scal-id argname acc stride node)))))))))))
  (kernel-fixup-loop-fission-args kernels)
  (remove-duplicates (reverse extra-allocs) :key (alexandria:compose #'car #'node-writes)))

(defun verify-ast-with-context (parse-ctx ctx blueprint &aux (new-ctx (make-scop-ctx-from-blueprint blueprint)))
  ;; If there's any, rewrite val_2 -> val_2[_gid0 + gid1]
  (with-slots ((node-to-loops node-to-loops) (exprs exprs)) new-ctx
    (let ((expr-subgraphs (loop for expr in (reverse exprs) collect (cons expr (caten/aasm::ast-expr-graph blueprint expr)))))
      (labels ((lookup (node) (reverse (gethash (node-id node) node-to-loops)))
               (find-expr-from-user (user)
                 (loop for expr in expr-subgraphs
                       if (find (node-id user) (graph-nodes (cdr expr)) :key #'node-id)
                         do (return-from find-expr-from-user (car expr))))
               (get-users (id)
                 (nconc
                  (id->users blueprint id)
                  (loop for node in (graph-nodes blueprint) if (and (eql (node-type node) :BIND) (eql id (getattr node :value))) collect node)))
               (is-vectorize-p (loop-obj)
                 (let ((d (getattr (getf loop-obj :for-node) :directive)))
                   (and d (equalp (directive-type d) "VECTORIZE"))))
               (invalid-scope-p (acc-scope expr-scope)
                 (when (> (length acc-scope) (length expr-scope)) (return-from invalid-scope-p t))
                 ;; grid id is unique in the blueprint, we can utilize it.
                 ;; Note: @VECTORIZE is regarded as not creating a new scope, because it is later rewritten as EXPR.
                 (let ((acc-scope (loop for acc in acc-scope if (null (is-vectorize-p acc)) collect acc))
                       (expr-scope (loop for expr in expr-scope if (null (is-vectorize-p expr)) collect expr)))
                   (loop for acc in acc-scope for expr in expr-scope
                         when (not (eql (node-id (getf acc :range-node)) (node-id (getf expr :range-node))))
                           do (return-from invalid-scope-p t)))
                 nil)
               (mutate-to-tensor-p (id &aux (acc (id->value blueprint id)) (users (get-users id)) (visited (make-hash-table)))
                 ;; All users must be placed in the possible scope where acc is firstly defined.
                 ;; Otherwise, we have to allocate extra.
                 (assert (eql :EXPR (node-type acc)))
                 (loop with acc-scope = (lookup acc)
                       for user in users for expr = (find-expr-from-user user)
                       if (and expr (null (gethash (node-id expr) visited))) do
                         (setf (gethash (node-id expr) visited) t)
                         ;; Compare the scope of (lookup acc) and (lookup expr)
                         (when (invalid-scope-p acc-scope (lookup expr))
                           (return-from mutate-to-tensor-p t))) ;; if theres at least one violation
                 nil))
        (let ((rewrite-ids))
          (maphash
           #'(lambda (previously-scalar rewrite-context)
               (declare (ignore rewrite-context))
               (when (id->value blueprint previously-scalar)
                 (when (mutate-to-tensor-p previously-scalar)
                   (push previously-scalar rewrite-ids))))
           (ctx-scal->access ctx))
          (let ((extra-allocs (bp-rewrite-scalar->buffer parse-ctx ctx (list blueprint) rewrite-ids)))
            (values blueprint extra-allocs)))))))

(defun get-primogem-blueprints-from-schedule (parse-ctx schedule)
  "Convert ISL Polyhedral Representation back to blueprint graph. If loop fission was applied, generates multiple blueprint."
  (let* ((ast (isl::ast-node-handle (compute-ast-from-schedule schedule)))
         (type (isl::%isl-ast-node-get-type ast)))
    (case type
      (:ast-node-error (isl::isl-error))
      ((:ast-node-for :ast-node-mark :ast-node-user) ;; they are always single kernel
       (list (with-blueprint (:noopt t) (%progn (parse-isl-ast parse-ctx ast)))))
      (:ast-node-if (error ":ast-node-if should not be placed on the root!"))
      (:ast-node-block ;; they could be divided to multiple kernels, let's check first.
       (let* ((children (isl::%isl-ast-node-block-get-children ast))
	      (n        (isl::%isl-ast-node-list-n-ast-node children))
              (children (reverse (loop for i upfrom 0 below n collect (isl::%isl-ast-node-list-get-at children i))))
              (n-kernels 0)
              (kernels (make-hash-table)))
         (flet ((mark-is-tilegpu-p (mark)
                  (and (eql (isl::%isl-ast-node-get-type mark) :ast-node-mark)
                       (let ((d (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id mark))))))
                         (eql :TILEGPU (intern (directive-type d) "KEYWORD"))))))
           (loop with cannot-add-new-loop-mode = nil
                 for c in children ;; Reading from bottom
                 for type = (isl::%isl-ast-node-get-type c)
                 for nth upfrom 0
                 if (and cannot-add-new-loop-mode (find type '(:ast-node-mark :ast-node-for)))
                   do (incf n-kernels) (setf cannot-add-new-loop-mode nil)
                 if (or (mark-is-tilegpu-p c) (find type '(:ast-node-for :ast-node-mark)))
                   do (setf cannot-add-new-loop-mode t)
                 do (setf (gethash n-kernels kernels) (append (list c) (gethash n-kernels kernels))))
           (nreverse
            (loop for i upfrom 0 to n-kernels
                  for kernel-items = (gethash i kernels)
                  collect
                  (with-blueprint (:noopt t) (apply #'%progn (map 'list #'(lambda (x) (parse-isl-ast parse-ctx x)) kernel-items)))))))))))

(defun apply-directives (blueprint)
  (let ((bands (make-hash-table)))
    (loop for node in (tpsort-graph blueprint)
          if (and (eql (node-type node) :FOR) (getattr node :band) (getattr node :directive))
            do (if (gethash (getattr node :band) bands)
                   (push node (gethash (getattr node :band) bands))
                   (setf (gethash (getattr node :band) bands) (list node))))
    ;; Rewrite by each directive
    (maphash
     #'(lambda (band-id bands)
         (let ((new-bp
                 (caten/codegen/search/optimization-rule:optrule-apply-transform-on-blueprint
                  (intern (directive-type (getattr (car bands) :directive)) "KEYWORD")
                  (reverse bands) blueprint)))
           (assert (graph-p new-bp) () "optrule-apply-transform-on-blueprint must return a Graph, when processing ~a, ~a" (getattr (car bands) :directive) band-id)
           (setf blueprint new-bp)))
     bands)
    (simplify-ast blueprint)
    blueprint))

(defun finalize-primogem-bp->legal-bp (parse-ctx blueprint)
  (declare (type parse-ctx parse-ctx) (type FastGraph blueprint))
  (multiple-value-bind (new-bp extra-allocs)
      (verify-ast-with-context ;; Compare the scope of all scalar variables w/ context, if theres some changes, add them as tmp buffer.
       parse-ctx (pctx-scop-ctx parse-ctx)
       (caten/aasm::ast-simplify-expr-subgraph (caten/aasm::%simplify-ast blueprint)))
    (values (ast-concrete-sequence (ast-apply-cse (ast-concrete-sequence (apply-directives new-bp)))) extra-allocs)))

(defun apply-schedule (schedule blueprint &key (ctx (make-scop-ctx-from-blueprint blueprint)))
  "
Conceptually
```
Blueprint_New = ApplySchedule(Schedule, Blueprint)
```
Note: Specify ctx as possible to optimize the transformation.

Return (value (list kernels) tmp-buffer-allocations)
"
  (declare (type ctx ctx) (type isl::schedule schedule) (type FastGraph blueprint))
  (let* ((pctx (make-parse-ctx blueprint ctx))
         (kernels (get-primogem-blueprints-from-schedule pctx schedule)))
    (if (= 1 (length kernels))
        (multiple-value-bind (bp allocs) (finalize-primogem-bp->legal-bp pctx (car kernels))
          (values (list bp) allocs))
        (let* ((all-nodes (apply #'append (map 'list #'graph-nodes kernels)))
               (all-nodes (loop for n in all-nodes if (not (eql (node-class n) :Render)) collect n))
               (common-buffer-among-kernels ;; a list of buffers which must be mutated into :DEFINE-GLOBAL
                 (loop for kernel in kernels
                       append (graph-get-undefined-variables kernel)))
               (common-buffer-among-kernels
                 ;; If the symbol was used as :BIND, replace them w/ :value
                 (loop for c in common-buffer-among-kernels
                       for user = (find c all-nodes :test #'find :key #'node-reads)
                       do (assert user)
                       if (eql (node-type user) :BIND) collect (getattr user :value) else collect c))
               (common-buffer-among-kernels
                 (loop for c in common-buffer-among-kernels
                       if (gethash c (ctx-scal->access ctx))
                         collect c)))
          (let ((extra-allocs (bp-rewrite-scalar->buffer pctx ctx kernels common-buffer-among-kernels)))
            (values
             (loop for kernel in kernels
                   collect
                   (multiple-value-bind (k alcs) (finalize-primogem-bp->legal-bp pctx kernel)
                     (dolist (a alcs) (push a extra-allocs))
                     k))
             (remove-duplicates extra-allocs :key (alexandria:compose #'car #'node-writes))))))))
;; ~~ Printer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(cffi:defcfun (%isl-ast-node-map-descendant-bottom-up "isl_ast_node_map_descendant_bottom_up")
    :pointer
  (x :pointer) (y :pointer) (z :pointer))

(cffi:defcfun (%isl-ast-node-foreach-descendant-top-down "isl_ast_node_foreach_descendant_top_down")
    :int
  (x :pointer) (y :pointer) (z :pointer))

(defparameter *tmpwrite* nil)
(defparameter *tmpread* nil)
(cffi:defcallback ast-user-add-annot :pointer
    ((ast-node :pointer) (user :pointer))
  (declare (ignore user))
  (let ((write-umap *tmpwrite*) (read-umap *tmpread*))
    (assert (and write-umap read-umap))
    (if (eql (isl::%isl-ast-node-get-type ast-node) :ast-node-user)
        (let* ((str (isl::%isl-ast-node-to-c-str ast-node))
               (expr (isl::%isl-ast-expr-op-get-arg (isl::%isl-ast-node-user-get-expr ast-node) 0))
               (id   (isl::%isl-ast-expr-id-get-id expr))
	       (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id))))
          (assert name)
          (setf str (subseq str 0 (- (length str) 2))) ;; remove newline
          (setf str (format nil "{~a -> ~{~a~^, ~} <- ~a(~{~a~^, ~})};"
                            (subseq str (length name))
                            (caten/codegen/search/schedule::umap-get-set-list-on-id write-umap name)
                            name
                            (caten/codegen/search/schedule::umap-get-set-list-on-id read-umap name)))
          (isl::%isl-ast-node-set-annotation ast-node (isl::identifier-handle (isl::make-id-from-str str))))
        ast-node)))

(cffi:defcallback ast-user-add-annot/read :pointer
    ((ast-node :pointer) (user :pointer))
  ast-node)

(defun ast-annotate-dataflow-graph (ast read write)
  (let ((*tmpread* read) (*tmpwrite* write))
    (isl::%make-ast-node
     (%isl-ast-node-map-descendant-bottom-up
      (isl::ast-node-handle (isl::__isl_take ast))
      (cffi:callback ast-user-add-annot)
      (cffi:null-pointer)))))

(cffi:defcallback ast-print-apply-annot :int
    ((ast-node :pointer) (user :pointer))
  (let ((annot (isl::%isl-ast-node-get-annotation ast-node)))
    (when (not (cffi:null-pointer-p annot))
      (setf (cffi:mem-ref user :string)
            (cl-ppcre:regex-replace
             (let ((str (isl::%isl-ast-node-to-c-str ast-node)))
               (setf str (cl-ppcre:regex-replace "\\(" str "\\\\("))
               (setf str (cl-ppcre:regex-replace "\\)" str "\\\\)"))
               (subseq str 0 (1- (length str))))
             (cffi:mem-ref user :string)
             (isl::%isl-id-to-str annot)))))
  1)

(defun ast->str (ast &key (indent 0) (polyhedron))
  (when polyhedron
    (setf ast (ast-annotate-dataflow-graph ast (psi-read-union-map polyhedron) (psi-write-union-map polyhedron))))
  (let* ((p     (isl::%isl-printer-to-str (isl::context-handle isl::*context*)))
         (p     (isl::%isl-printer-set-output-format p 4)) ;; 4 == Clang
         (p     (isl::%isl-printer-set-indent p indent))
         (q     (isl::%isl-printer-print-ast-node p (isl::ast-node-handle ast)))
         (str   (isl::%isl-printer-get-str q)))
    (cffi:with-foreign-object (str* :string)
      (setf (cffi:mem-ref str* :string) str)
      (%isl-ast-node-foreach-descendant-top-down (isl::ast-node-handle ast) (cffi:callback ast-print-apply-annot) str*)
      (cffi:mem-ref str* :string))))

(defmethod print-object ((pg Polyhedral-Schedule-Item) stream)
  (print-unreadable-object (pg stream :type t :identity t)
    (format stream "~%~a~%  :history ~a" (ast->str (compute-ast-from-schedule (psi-theta pg))) (psi-opt-history pg))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO]
;; - 1. Coalesce: ASTNode Levelで絶対やる (Bugが減る，意味が超簡単になる)
;;   - CoalesceはMark+ForのParseをTriggerとして実施する？
;;     - Parseた後AST Level Transformationとして先にやる
;;     - これはEXPRのSubstituteを使えば簡単にできるし，こっちの方が断然簡単そう。
;;     - 絶対こっちでやる。DIRECTIVEにCOALESCE=Tを追加するのが手っ取り早い
;; - 2. MOVE Optimization Path (Bidijective Path)
;; - 3. AST OffsetのScaleなどもASTLevelで実施する
;; - 4. PrintをSimpleにする (e.g.: for i in range(10, 4):)
