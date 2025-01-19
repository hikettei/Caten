(defpackage :caten/codegen/ast-parser
  (:documentation "
Transform the Polyhedral IR into the Blueprint IR.
```
[ISL Polyhedral IR] ==> <Polyhedral-AST> ===> Caten Blueprint IR
```
scop.lisp for the opposite things.

To generate a simplified kernel, caten/codegen assumes the following constraints to the input:
- The loops are starting with zero.
- The tiled loops are also starting with zero.
")
  (:use :cl :caten/codegen/expr :caten/codegen/expr-cache :caten/air :caten/codegen/shape-inference :trivia
        :caten/codegen/polyhedral-ast :caten/codegen/transform :caten/codegen/directive)
  (:import-from
   :caten/codegen/packing
   #:ast-rewrite-vectorize)
  (:export #:lower-into-bp-from-polyhedral))

(in-package :caten/codegen/ast-parser)

(defstruct Context
  (dynamic-shape-table (make-hash-table) :type hash-table)
  (n-global-offset 0 :type fixnum)
  (n-global-dims 0 :type fixnum))

(defmethod ctx-get-rank ((ctx Context))
  (if (= 0 (context-n-global-offset ctx))
      (context-n-global-dims ctx)
      (- (context-n-global-offset ctx) (context-n-global-dims ctx))))

(defmethod ctx-intern ((ctx Context) obj)
  (let ((key (string-upcase (princ-to-string obj))))
    (or (gethash key (context-dynamic-shape-table ctx))
        (intern key))))

(declaim (ftype (function (context cffi:foreign-pointer list) t) parse-isl-ast))
(defun parse-isl-ast (ctx ast directives)
  (declare (type cffi:foreign-pointer ast))
  (let ((type (isl::%isl-ast-node-get-type ast)))
    (ecase type
      (:ast-node-error (isl::isl-error))
      (:ast-node-for   (parse-isl-ast-for ctx ast directives))
      (:ast-node-if    (parse-isl-ast-if ctx ast directives))
      (:ast-node-block (parse-isl-ast-block ctx ast directives))
      (:ast-node-mark  (parse-isl-ast-mark ctx ast directives))
      (:ast-node-user  (parse-isl-ast-user ctx ast directives)))))

(declaim (ftype (function (context cffi:foreign-pointer list) ASTBlock) parse-isl-ast-block))
(defun parse-isl-ast-block (ctx ast directives)
  (declare (type cffi:foreign-pointer ast))
  (let* ((children (isl::%isl-ast-node-block-get-children ast))
	 (n        (isl::%isl-ast-node-list-n-ast-node children)))
    (make-block
     (loop for i upfrom 0 below n
	   for child = (isl::%isl-ast-node-list-get-at children i)
	   collect
	   (parse-isl-ast ctx child directives)))))

(declaim (ftype (function (context cffi:foreign-pointer list) User) parse-isl-ast-user))
(defun parse-isl-ast-user (ctx ast directives)
  (declare (type cffi:foreign-pointer ast))
  (let ((expr (isl::%isl-ast-node-user-get-expr ast)))
    (let* ((first-expr (isl::%isl-ast-expr-op-get-arg expr 0))
	   (n          (isl::%isl-ast-expr-get-op-n-arg expr))
	   (id         (isl::%isl-ast-expr-id-get-id first-expr))
	   (name       (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	   (args       (loop for i upfrom 1 below n
			     collect
			     (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg expr i) directives))))
      (make-user name args))))

(declaim (ftype (function (context cffi:foreign-pointer list) t) parse-isl-ast-mark))
(defun parse-isl-ast-mark (ctx ast directives)
  "Inserts multi-dimensional marks into the ASTFor. e.g.:
```
@DIRECTIVE(type=GLOBAL, amount=(2 2), visible=NIL)
for (int i = 0; i < 10; i++) {
  for (int j = 0; j < 10; j++) {
    S1(i, j)
  }
```
The @DIRECTIVE is transformed and mapped as:
```
@DIRECTIVE(type=GLOBAL, amount=(2), visible=NIL)
for (int i = 0; i < 10; i++) {
  @DIRECTIVE(type=GLOBAL, amount=(2), visible=NIL)
  for (int j = 0; j < 10; j++) {
    S1(i, j)
  }
```
The mapped ast is finally transformed by the ast-rewrite-directive function.
"
  ;; [TODO] quite hard to read this code ...
  (let* ((directive (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id ast)))))
         (type (directive-type directive))
         (stop-type (cond ((equalp type "LOCAL") "GLOBAL") ((equalp type "PACKED_INNER") "PACKED_OUTER")))
         (corresponding-size nil)
         (directives
           (loop for directives-channel in directives
                 for nth upfrom 0
                 collect
                 (loop for d in directives-channel
                       when (equalp (directive-type d) stop-type)
                         do (setf corresponding-size (length (nth nth directives)))
                       else
                         collect d)))
         (directives-new
           (loop for amt in (directive-amount directive)
                 collect (directive (directive-type directive) amt (directive-visible directive)))))
    (when corresponding-size
      (setf directives-new (subseq directives-new 0 (- (length directives-new) corresponding-size))))
    (parse-isl-ast ctx (isl::%isl-ast-node-mark-get-node ast) (append directives (list directives-new)))))

(declaim (ftype (function (context (or cffi:foreign-pointer isl:ast-node) list) (values Expr &optional)) parse-isl-expr))
(defun parse-isl-expr (ctx ast directives)
  (declare (type (or cffi:foreign-pointer isl:ast-node) ast))
  (let* ((ast (if (isl:ast-node-p ast)
		  (isl::ast-node-handle ast)
		  ast))
	 (type (isl::%isl-ast-expr-get-type ast)))
    (ecase type
      (:ast-expr-error (isl::isl-error))
      (:ast-expr-id
       (let* ((id (isl::%isl-ast-expr-id-get-id ast))
	      (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
              (cache (restore-expr name)))
	 (declare (type string name))
         (if cache
             cache
             (expr-const (ctx-intern ctx name) :int64))))
      (:ast-expr-int
       (let* ((id (isl::%isl-ast-expr-int-get-val ast))
	      (num (isl::%isl-val-get-d id)))
	 (declare (type number num))
	 (let ((num (round num)))
           (expr-const num :int64))))
      (:ast-expr-op
       (let* ((n-arg (isl::%isl-ast-expr-get-op-n-arg ast))
	      (args (loop for nth upfrom 0 below n-arg
			  collect (parse-isl-expr ctx (isl::%isl-ast-expr-op-get-arg ast nth) directives)))
	      (op-type (isl::%isl-ast-expr-op-get-type ast)))
	 (flet ((->expr (lhs rhs)
		  (assert (not (eql op-type :ast-expr-op-error)) () ":isl_ast_expr_op_error")
		  (ecase op-type
		    (:ast-expr-op-and (expr-and lhs rhs))
		    (:ast-expr-op-and-then (expr-and lhs rhs))
		    (:ast-expr-op-or (expr-or lhs rhs))
		    (:ast-expr-op-or-else (expr-or lhs rhs))
		    (:ast-expr-op-max (expr-max lhs rhs))
		    (:ast-expr-op-min  (expr-min lhs rhs))
		    (:ast-expr-op-minus (expr-neg lhs)) ;; (- a)
		    (:ast-expr-op-add (expr-add lhs rhs))
		    (:ast-expr-op-sub (expr-sub lhs rhs))
		    (:ast-expr-op-mul (expr-mul lhs rhs))
		    (:ast-expr-op-div (expr-idiv lhs rhs))		 
		    (:ast-expr-op-fdiv-q (expr-idiv lhs rhs))
		    (:ast-expr-op-pdiv-q (expr-idiv lhs rhs))
		    (:ast-expr-op-pdiv-r (expr-mod lhs rhs))
		    (:ast-expr-op-zdiv-r (expr-mod lhs rhs))
		    ;; (:expr-op-cond)
		    (:ast-expr-op-eq (expr-= lhs rhs))
                    ;; Rewrite LE to simplify the expression assuming rhs is an integer.
		    (:ast-expr-op-le (expr-< lhs (expr-add rhs (expr-const 1 :int64)))) ;; <=
		    (:ast-expr-op-lt (expr-< lhs rhs)) ;; <
		    (:ast-expr-op-ge (expr-not (expr-< lhs rhs))) ;; >=
		    (:ast-expr-op-gt (expr-> lhs rhs)) ;; >
		    ;; (:expr-op-call)
		    ;; (:expr-op-access)
		    ;; (:expr-op-member)
		    ;; (:expr-op-address-of)
		    (otherwise
		     (error "~a is not supported by caten" op-type)))))
	   (if (= (length args) 1)
	       (->expr (car args) nil)
	       (case op-type
		 (:ast-expr-op-select
		  (assert (= (length args) 3))
		  (apply #'expr-where args))
		 (otherwise
		  (reduce #'->expr args))))))))))

(declaim (ftype (function (context cffi:foreign-pointer list) ASTFor) parse-isl-ast-for))
(defun parse-isl-ast-for (ctx ast directives)
  (declare (type cffi:foreign-pointer ast))
  (let* ((iter (isl::%isl-ast-node-for-get-iterator ast))
	 (id (isl::%isl-ast-expr-get-id iter))
	 (name (cffi:foreign-string-to-lisp (isl::%isl-id-get-name id)))
	 (from (parse-isl-expr ctx (isl::%isl-ast-node-for-get-init ast) directives))
	 (by (parse-isl-expr ctx (isl::%isl-ast-node-for-get-inc ast) directives))
	 (to (parse-isl-expr ctx (isl::%isl-ast-node-for-get-cond ast) directives))
         (directives (loop for d in directives if d collect d))
         (mark (map 'list #'car directives))
         (directives (loop for d in (map 'list #'cdr directives) if d collect d))
	 (body (parse-isl-ast ctx (isl::%isl-ast-node-for-get-body ast) directives)))
    (let ((for (make-for name from to by body)))
      (setf (astfor-marks for) mark)
      for)))

(declaim (ftype (function (context cffi:foreign-pointer list) AstIf) parse-isl-ast-if))
(defun parse-isl-ast-if (ctx ast directives)
  (declare (type cffi:foreign-pointer ast))
  (let* ((condition
	   (parse-isl-expr ctx (isl::%isl-ast-node-if-get-cond ast) directives))
	 (then-node
	   (parse-isl-ast ctx (isl::%isl-ast-node-if-get-then-node ast) directives))
	 (else-p (isl::%isl-ast-node-if-has-else-node ast))
	 (else-node
	   (when (eql else-p :bool-true)
	     (parse-isl-ast ctx (isl::%isl-ast-node-if-get-else-node ast) directives))))
    (make-if condition then-node else-node)))
;; ~~ Directive Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-rewrite-tile-pair (ast)
  "Pairs GLOBAL and LOCAL, PACKED_OUTER and PACKED_INNER."
  (labels ((handler (ast globals packs)
             (etypecase ast
               (User (copy-user ast))
               (AstBlock
                (let ((new-block (copy-astblock ast)))
                  (setf (astblock-body new-block) (map 'list #'(lambda (x) (handler x globals packs)) (astblock-body ast)))
                  new-block))
               (AstFor
                ;; Pair GLOBAL and LOCAL, PACKED_OUTER and PACKED_INNER.
                (let ((new-astfor (copy-astfor ast))
                      (globals (if (find "GLOBAL" (astfor-marks ast) :test #'equalp :key #'directive-type)
                                   (append globals (list ast)) globals))
                      (packs   (if (find "PACKED_OUTER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                                   (append packs (list ast)) packs)))
                  (when (find "LOCAL" (astfor-marks ast) :test #'equalp :key #'directive-type)
                    (assert globals () "Mismatch pair of GLOBAL and LOCAL")
                    (setf (astfor-tile-parent new-astfor) (car globals)
                          globals (cdr globals)))
                  (when (find "PACKED_INNER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                    (assert packs () "Mismatch pair of PACKED_OUTER and PACKED_INNER")
                    (setf (astfor-tile-parent new-astfor) (car packs)
                          packs (cdr packs)))
                  (setf (astfor-body new-astfor) (handler (astfor-body ast) globals packs))
                  new-astfor))
               (AstExpr ast)
               (AstIf
                (let ((new-if (copy-astif ast)))
                  (setf (astif-then-node new-if) (handler (astif-then-node ast) globals packs)
                        (astif-else-node new-if) (handler (astif-else-node ast) globals packs))
                  new-if))
               (null))))
    (handler ast nil nil)))

(defun astfor-make-reminder-astfor (ctx astfor n-unroll unrolled-body reminder-body)
  "
Consider this loop unrolling pattern:
```
for (int i=0; i<X; i+=N_UNROLL)
  S(i) S(i+1) S(i+2) ... S(i+N_UNROLL-1)
```
Here, the last statement S(i+N_UNROLL-1) should satisfy the following condition to avoid out-of-bound access:

{ i+N_UNROLL-1 <= X } <=> { i <= X - (N_UNROLL-1) }

This is a subset of the original iteration space. So we also have to generate a reminder part:
```
for (int i=X-N_REMINDER; i<X; i+=1)
  S(i)
```
So the final code looks like (also simplified by the simplifier if shapes are static)
```
int alu_0 = X;
int alu_1 = alu_0 - N_UNROLL-1
for (int i=0; i<alu_1; i+=N_UNROLL)
  S(i) S(i+1) S(i+2) ... S(i+N_UNROLL-1)
for (int i=alu_1; i<alu_0; i+=1)
  S(i)
```"
  (let* ((alu0-id (ctx-intern ctx (format nil "_alu0_~a" (astfor-idx astfor))))
         (alu1-id (ctx-intern ctx (format nil "_alu1_~a" (astfor-idx astfor))))
         (alu0 (expr-const alu0-id :int64))
         (alu1 (expr-const alu1-id :int64))
         (alu0-expr (expr-detach-loop-bound (astfor-to astfor)))
         (alu1-expr (expr-sub alu0 (expr-const (- n-unroll 1) :int64)))
         (type1 (make-inferred-type nil (list (caten/runtime:make-buffer nil nil :int64 nil :device 'caten/codegen/shape-inference::RelayBuffer))))
         (type2 (make-inferred-type nil (list (caten/runtime:make-buffer nil nil :int64 nil :device 'caten/codegen/shape-inference::RelayBuffer))))
         (idx (expr-const (ctx-intern ctx (astfor-idx astfor)) :int64))
         
         (packed-upfrom (expr-const 0 :int64))
         (packed-to (expr-< idx alu1))
         (packed-by (expr-const n-unroll :int64))

         (reminder-upfrom alu1)
         (reminder-to (expr-< idx alu0))
         (reminder-by (expr-const 1 :int64)))
    (setf (relay-write-iters type1) (list (make-iteration-space))
          (relay-write-iters type2) (list (make-iteration-space)))
    (values
     (make-astexpr (make-node :JIT :EXPR (list alu0-id) nil :EXPR alu0-expr :declare-type `(t) :_type_relay type1) nil)
     (make-astexpr (make-node :JIT :EXPR (list alu1-id) nil :EXPR alu1-expr :declare-type `(t) :_type_relay type2) nil)
     (make-for (astfor-idx astfor) packed-upfrom packed-to packed-by unrolled-body)
     (make-for (astfor-idx astfor) reminder-upfrom reminder-to reminder-by reminder-body))))

(defun ast-rewrite-directive (ctx ast)
  "Rewrites the PACKED_INNER and PACKED_OUTER directives:
- PACKED_INNER is a trigger to generate the unrolled part and the reminder part.
- PACKED_OUTER is completly unrolled and the astfor is removed."
  (labels ((handler (ast vectorized-idx)
             ;; Reminder-idx-list
             (etypecase ast
               (User (copy-user ast))
               (AstBlock
                (let ((new-block (copy-astblock ast)))
                  (setf (astblock-body new-block) (map 'list #'(lambda (x) (handler x vectorized-idx)) (astblock-body ast)))
                  new-block))
               (ASTFor
                (let ((new-astfor (copy-astfor ast))
                      (marks (astfor-marks ast))
                      (mark))
                  (assert (<= (length marks) 2))
                  (setf (astfor-marks new-astfor)
                        (loop for m in marks
                              if (find (directive-type m) `("GLOBAL" "LOCAL" "PARALLEL") :test #'equalp)
                                collect m
                              else
                                do (setf mark m)))
                  (if mark
                      (cond
                        ((equalp (directive-type mark) "PACKED_INNER")
                         (setf (astfor-body new-astfor) (handler (astfor-body ast) vectorized-idx))
                         (let* ((n-pack (directive-amount mark))
                                (n-pack (the fixnum (if (numberp n-pack) n-pack (car n-pack))))
                                (parent (astfor-tile-parent ast))
                                (allow-vectorized (and parent (find (astfor-idx parent) vectorized-idx :test #'string=))))
                           (if allow-vectorized
                               ;; Unroll_IDX = 0, 1, 2, ...
                               (packing-ast (astfor-body new-astfor) (astfor-idx ast) (astfor-idx parent) n-pack)
                               ;; Unroll_Idx = 0
                               (packing-ast (astfor-body new-astfor) (astfor-idx ast) (astfor-idx parent) 1))))
                        ((equalp (directive-type mark) "PACKED_OUTER")
                         (let* ((n-pack (directive-amount mark))
                                (n-pack (the fixnum (if (numberp n-pack) n-pack (car n-pack))))
                                (packed-body (handler (astfor-body ast) (append vectorized-idx (list (astfor-idx ast)))))
                                (reminder-body (handler (astfor-body ast) vectorized-idx)))
                           (multiple-value-bind (alu0 alu1 packed-for reminder-for)
                               (astfor-make-reminder-astfor ctx ast n-pack packed-body reminder-body)
                             ;; [TODO] Test LOCAL+VECTORIZE Fusion generating a reminder.
                             (setf (astfor-marks packed-for) (astfor-marks new-astfor))
                             (make-block (list alu0 alu1 packed-for reminder-for)))))
                        (T (error "The directive ~a transformation is not supported." mark)))
                      (progn
                        (setf (astfor-body new-astfor) (handler (astfor-body ast) vectorized-idx))
                        new-astfor))))
               (AstExpr ast)
               (AstIf
                (let ((new-if (copy-astif ast)))
                  (setf (astif-then-node new-if) (handler (astif-then-node ast) vectorized-idx)
                        (astif-else-node new-if) (handler (astif-else-node ast) vectorized-idx))
                  new-if))
               (null))))
    (handler ast nil)))
         
(defun ast-rewrite-grids (ast)
  "Rewrites PARALLEL/GLOBAL/LOCAL directives."
  ast
  )
;; ~~ ISL Object <--> Blueprint ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun r/for (idx upfrom below by scope)
  (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by :scope scope))

(defun r/endfor (idx)
  (make-node :Render :ENDFOR nil nil :idx idx))

(defun r/if (condition idx)
  (make-node :Render :IF nil nil :condition condition :idx idx))

(defun r/endif (idx)
  (make-node :Render :ENDIF nil nil :idx idx))

(defun create-rendering-graph-nodes (lisp-ast items &aux (space))
  (let ((new-graph))
    (labels ((find-user (node-id args)
               (let ((node (find (princ-to-string node-id) items
                                 :key (alexandria:compose #'princ-to-string #'node-id)
                                 :test #'equalp)))
                 (assert node () "~a is not found in ~a" node-id items)
                 (assert (eql (node-type node) :EXPR))
                 (setf node (copy-node node))
                 (let ((base (getattr node :iterations)))
                   (setf (getattr node :iterations) args)
                   (if (and (null args) (> (length base) 0))
                       (setf (getattr node :iterations) base)
                       (progn
                         (setf (getattr node :iterations) (ensure-iteration-space-length (length base) (getattr node :iterations)))
                         (assert (= (length (getattr node :iterations)) (length base)) () "Before and after the polyhedral compilation, the rank of iteration space should not be changed. ~a -> ~a" (getattr node :iterations) args))))
                 node))
             (lower (object)
	       (when (listp object) (return-from lower (map 'list #'lower object)))
	       (ematch object
		 ((ASTBlock :body body) (map 'list #'lower body))
		 ((AstFor :idx idx :from upfrom :to to :by by :body body :scope scope)
                  ;; [TODO] Generalize this
                  (let ((is-tile-band (find "TILE_BAND" (astfor-marks object) :test #'equalp)))
                    (when (not (expr-scalar-equivalent-p upfrom (expr-detach-loop-bound to)))
                      (when (null is-tile-band) (push idx space))
		      (push (r/for idx upfrom to by scope) new-graph)
		      (lower body)
                      (when (null is-tile-band) (setf space (remove idx space :test #'string=)))
		      (push (r/endfor idx) new-graph))))
		 ((User :name name :args args)
                  (push (unroll-expr (reverse space) (find-user name args) object) new-graph))
		 ((AstIf :condition cond :then-node then :else-node else)
                  (let ((id (gensym "IF")))
		    (push (r/if cond id) new-graph)
		    (lower then)
                    (assert (null else) () "else should be none")
		    (push (r/endif id) new-graph)))
                 ((AstExpr :expr expr :is-defglobal-p defglobal-p)
                  (assert (eql (node-type expr) :EXPR) () "ASTEXPR can only have an EXPR.")
                  (when defglobal-p
                    (push (string-downcase (princ-to-string (car (node-writes expr)))) space))
                  (push expr new-graph))
		 (_
		  (error "create-rendering-graph: ~a should not occur here!" object)))))
      (lower lisp-ast))
    (nreverse new-graph)))

(defun dynamic-shape-table (item)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (name . dtype) in (getattr item :dynamic-shapes)
          do (setf (gethash (string-upcase (princ-to-string name)) table) name))
    table))

(defun lower-into-bp-from-polyhedral (ast scheduled-item &key (n-global-offset 0) (vectorizes nil))
  (declare (type isl:ast-node ast))
  (assert (eql (node-type scheduled-item) :Schedule-Item))
  ;; debug
  (setf vectorizes nil)
  
  (let* ((ctx (make-context :n-global-offset n-global-offset :dynamic-shape-table (dynamic-shape-table scheduled-item)))
         (ast (parse-isl-ast ctx (isl::ast-node-handle ast) nil))
         (ast (ast-rewrite-tile-pair ast))
         (ast (ast-rewrite-directive ctx ast))
         (ast (ast-rewrite-vectorize ast vectorizes scheduled-item))
         (bp (create-rendering-graph-nodes ast (getattr scheduled-item :blueprint)))
         (bp (caten/codegen/packing:blueprint-upcast-inference bp scheduled-item)))
    bp))

;; TMP
#|
(defun parse-isl-ast-mark (ctx ast)
  "@DIRECTIVE Transformation Triggers"
  (let* ((directive (str->directive (cffi:foreign-string-to-lisp (isl::%isl-id-get-name (isl::%isl-ast-node-mark-get-id ast)))))
         (user (parse-isl-ast ctx (isl::%isl-ast-node-mark-get-node ast))))
    ;; A Sequence of Mark => Set Priority
    ;; [TODO] Sort marks in the isl-schedule level (e.g.: GLOBAL should come after UNROLL)
    (macrolet ((is (name) `(equalp (directive-type directive) ,name)))
      (typecase user
        (AstFor
         ;; Entry point for transformations
         (cond
           ((is "GLOBAL")
            ;; TODO:
            ;; const idx1 = amount * blockIdx.X;
            ;; dont use amount use by
            (let ((replacement (astfor-mutate-global user (ctx-get-rank ctx) (directive-amount directive))))
              (incf (context-n-global-dims ctx))
              (return-from parse-isl-ast-mark replacement)))
           ((is "LOCAL")
            ;; TODO
            ;; If (min ...) is scheduled, insert IF
            ;; If (min m - c0 -1)
            ;; const idx2 = threadIdx.y;
            ;; index = idx1 + idx2;
            
            )
           ((is "PARALLEL")
            ;; TODO: The order does not matter
            (setf (astfor-scope user) :global))
           ;; UNROLL_OUTER + UNROLL_INNER = UNROLL
           ((is "UNROLL_OUTER")
            (let ((body (astfor-body user)))
              (when (or
                     (not (typep body 'ASTFor))
                     (null (and (astfor-marks body) (every #'(lambda (x) (equalp (directive-type x) "UNROLL_INNER")) (astfor-marks body)))))
                (return-from parse-isl-ast-mark user))
              (let* ((n-unroll (directive-amount directive))
                     (user     (copy-astfor user))
                     (unrolled (make-unrolled-body user body n-unroll))
                     (reminder (compute-reminder-for-unroll user body n-unroll)))
                (setf (astfor-body user) unrolled)
                (return-from parse-isl-ast-mark (make-block (list user reminder))))))
           ((is "UNROLL_INNER")
            ;; UNROLL_BODY is triggered by the UNROLL_PARENT. Without it the form is ignored.
            (assert (null (astfor-marks user)) () "UNROLL_INNER should be orthogonal with other directives.")
            (setf (astfor-marks user) (list directive)))
           ;; PACKED_OUTER+PACKED_INNER = PACKED
           ((is "PACKED_OUTER")
            (let ((body (astfor-body user)))
              (when (or
                     (not (typep body 'ASTFor))
                     (null (and (astfor-marks body) (every #'(lambda (x) (equalp (directive-type x) "PACKED_INNER")) (astfor-marks body)))))
                (return-from parse-isl-ast-mark user))
              (let* ((n-pack (directive-amount directive))
                     (user (copy-astfor user))
                     (packed (make-packed-body user (copy-ast body) n-pack))
                     (reminder (compute-reminder-for-unroll user (copy-ast body) n-pack)))
                (setf (astfor-body user) packed)
                (push directive (astfor-marks user))
                (return-from parse-isl-ast-mark (make-block (list user reminder))))))
           ((is "PACKED_INNER")
            (assert (null (astfor-marks user)) () "PACKED_INNER should be orthogonal with other directives.")
            (setf (astfor-marks user) (list directive)))))
        ;; [TODO] remove this case!
        ;; [TODO] Test all cases for multiple directives per single loop.
        ;; - 1. TILE+PACKING (high priority!)
        ;; - 2. UNROLL+PACKING
        (AstBlock
         ;; Nested Directive Transformations
         (cond
           ((is "PARALLEL")
            (when (astfor-p (car (astblock-body user)))
              (setf (astfor-scope (car (astblock-body user))) :global)))
           ((is "GLOBAL")
            ;; @DIRECTIVE(GLOBAL)
            ;; for (...;...;+=4) {...} // BODY
            ;; for (...;...;+=1) {...} // Reminder
            (when (and
                   (= (length (astblock-body user)) 2)
                   (typep (first (astblock-body user)) 'AstFor)
                   (typep (second (astblock-body user)) 'AstFor))
              (multiple-value-bind (body reminder) (apply #'values (astblock-body user))
                (let ((replacement
                        (make-block
                         (list
                          (astfor-mutate-global body (ctx-get-rank ctx) (directive-amount directive))
                          (astfor-mutate-reminder-global (astfor-idx body) reminder)))))
                  (incf (context-n-global-dims ctx))
                  (return-from parse-isl-ast-mark replacement)))))))))
user))
|#
