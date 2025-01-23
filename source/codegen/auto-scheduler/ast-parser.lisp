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
  (n-global-dims 0 :type fixnum))

(defmethod ctx-intern ((ctx Context) obj)
  (let ((key (string-upcase (princ-to-string obj))))
    (or (gethash key (context-dynamic-shape-table ctx))
        (intern key))))

(defmethod ctx-shared-mem-id ((ctx Context) obj)
  (ctx-intern ctx (format nil "_~a_s" obj)))

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
         (stop-type (cond ((equalp type "LOCAL") "GLOBAL") ((equalp type "PACKED_INNER") "PACKED_OUTER")
                          ((equalp type "PREFETCH_INNER") "PREFETCH_OUTER") ((equalp type "TILE_OUTER") "TILE_INNER")))
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
    ;; Rewriting for (int c=A; c < B; c++) ==> for (int c=0; c < B-A; c++)
    (let ((bound (expr-detach-loop-bound to :allow-failed t)))
      (assert bound () "parse-isl-ast-for: The loop bound scheduled by isl is not a constant.")
      (setf to (expr-< (expr-const (ctx-intern ctx name) :int64) (expr-sub bound from))
            from (expr-const 0 :int64)))
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
  (labels ((handler (ast globals packs prefetchs tiles)
             (etypecase ast
               (User (copy-user ast))
               (AstBlock
                (let ((new-block (copy-astblock ast)))
                  (setf (astblock-body new-block) (map 'list #'(lambda (x) (handler x globals packs prefetchs tiles)) (astblock-body ast)))
                  new-block))
               (AstFor
                ;; Pair GLOBAL and LOCAL, PACKED_OUTER and PACKED_INNER.
                (let ((new-astfor (copy-astfor ast))
                      (globals (if (find "GLOBAL" (astfor-marks ast) :test #'equalp :key #'directive-type)
                                   (append globals (list ast)) globals))
                      (packs   (if (find "PACKED_OUTER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                                   (append packs (list ast)) packs))
                      (prefetchs (if (find "PREFETCH_OUTER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                                     (append prefetchs (list ast)) prefetchs))
                      (tiles (if (find "TILE_OUTER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                                 (append tiles (list ast)) tiles)))
                  (when (find "LOCAL" (astfor-marks ast) :test #'equalp :key #'directive-type)
                    (assert globals () "Mismatch pair of GLOBAL and LOCAL")
                    (setf (astfor-tile-parent new-astfor) (car globals)
                          globals (cdr globals)))
                  (when (find "PACKED_INNER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                    (assert packs () "Mismatch pair of PACKED_OUTER and PACKED_INNER")
                    (setf (astfor-tile-parent new-astfor) (car packs)
                          packs (cdr packs)))
                  (when (find "PREFETCH_INNER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                    (assert prefetchs () "Mismatch pair of PREFETCH_OUTER and PREFETCH_INNER")
                    (setf (astfor-tile-parent new-astfor) (car prefetchs) prefetchs (cdr prefetchs)))
                  (when (find "TILE_INNER" (astfor-marks ast) :test #'equalp :key #'directive-type)
                    (assert tiles () "Mismatch pair of TILE_OUTER and TILE_INNER")
                    (setf (astfor-tile-parent new-astfor) (car tiles) tiles (cdr tiles)))
                  (setf (astfor-body new-astfor) (handler (astfor-body ast) globals packs prefetchs tiles))
                  new-astfor))
               (AstExpr ast)
               (AstIf
                (let ((new-if (copy-astif ast)))
                  (setf (astif-then-node new-if) (handler (astif-then-node ast) globals packs prefetchs tiles)
                        (astif-else-node new-if) (handler (astif-else-node ast) globals packs prefetchs tiles))
                  new-if))
               (null))))
    (handler ast nil nil nil nil)))

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
int alu_2 = X - (mod X N_UNROLL)
for (int i=0; i<alu_1; i+=N_UNROLL)
  S(i) S(i+1) S(i+2) ... S(i+N_UNROLL-1)
for (int i=alu_2; i<alu_0; i+=1)
  S(i)
```"
  (let* ((alu0 (expr-detach-loop-bound (astfor-to astfor)))
         (alu1 (expr-sub alu0 (expr-const (- n-unroll 1) :int64)))
         (idx (expr-const (ctx-intern ctx (astfor-idx astfor)) :int64))
         
         (packed-upfrom (expr-const 0 :int64))
         (packed-to (expr-< idx alu1))
         (packed-by (expr-const n-unroll :int64))

         (reminder-upfrom (expr-sub alu0 (expr-mod alu0 (expr-const n-unroll :int64))))
         (reminder-to (expr-< idx alu0))
         (reminder-by (expr-const 1 :int64)))
    (values
     (make-for (astfor-idx astfor) packed-upfrom packed-to packed-by unrolled-body)
     (make-for (astfor-idx astfor) reminder-upfrom reminder-to reminder-by reminder-body))))
;; ~~ Shared Memory Transfer Optimization ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-rewrite-astfor-idx (ctx ast from-idx to-idx &aux (to-idx-s (ctx-intern ctx to-idx)) (from-idx-s (ctx-intern ctx from-idx)))
  (labels ((rewrite-expr (expr)
             (let ((cpexpr (copy-expr expr)))
               (setf (graph-nodes (expr-graph cpexpr))
                     (loop for node in (graph-nodes (expr-graph cpexpr))
                           if (and (eql (node-type node) :Load) (eql from-idx-s (getattr node :value)))
                             collect
                             (let ((n (copy-node node)))
                               (setf (getattr n :value) to-idx-s)
                               n)
                           else
                             collect node))
               cpexpr))    
           (handler (ast)
             (etypecase ast
               (User
                (let ((cp (copy-user ast)))
                  (setf (user-args cp) (map 'list #'rewrite-expr (user-args ast)))
                  cp))
               (AstBlock (make-block (map 'list #'handler (astblock-body ast))))
               (AstFor
                (let ((copy-astfor (copy-astfor ast)))
                  (setf (astfor-idx copy-astfor) (if (equalp from-idx (astfor-idx ast)) to-idx (astfor-idx ast))
                        (astfor-from copy-astfor) (rewrite-expr (astfor-from ast))
                        (astfor-to copy-astfor) (rewrite-expr (astfor-to ast))
                        (astfor-body copy-astfor) (handler (astfor-body copy-astfor)))
                  copy-astfor))
               (AstExpr ast)
               (AstIf
                (make-if (astif-condition ast) (handler (astif-then-node ast)) (handler (astif-else-node ast))))
               (null ast))))
    (handler ast)))

(defun ast-make-prefetch-barrier (ctx data-reuse prefetch-inner transfer narefs)
  "
for (int _gid0=0; _gid0<512; _gid0+=128) {
  for (int _gid1=0; _gid1<512; _gid1+=128) {
    for (int _gid2=0; _gid2<128; _gid2+=1) {
      for (int _gid3=0; _gid3<128; _gid3+=1) {
        float val_2_0_0_0 = 0.0;
        for (int _gid4=0; _gid4<512; _gid4+=128) {
          if (_gid2 == 0 and _gid3 == 0) {   /*/ This function creates this form /*/
              As_Aligned[...]=[...];         /*/ Transfer /*/
              Bs_Aligned[...]=[...];         /*/ Transfer /*/
          }                                  /*/ Transfer /*/
          for (int _gid5=0; _gid5<128; _gid5+=1) {
            val_2_0_0_0 = (val_2_0_0_0+((*(val_4+((512*(_gid0+_gid2))+(_gid4+_gid5))))*(*(val_6+((_gid1+_gid3)+(512*(_gid4+_gid5)))))));
          }
        }
        (*(val_9+((512*(_gid0+_gid2))+(_gid1+_gid3)))) = val_2_0_0_0;
      }
    }
  }
}

Constraints:
- data-reuse[list of astfor]
  - Mark must be either of:
    - every mark is LOCAL      => A barrier is inserted.
    - every mark is TILE_INNER => A if statement is inserted.
"
  (declare (type list transfer narefs))
  (labels ((identify-for (for)
             (if (find "LOCAL" (astfor-marks for) :test #'equalp :key #'directive-type)
                 :LOCAL
                 (if (or
                      (find "TILE_INNER" (astfor-marks for) :test #'equalp :key #'directive-type)
                      (find "PREFETCH_INNER" (astfor-marks for) :test #'equalp :key #'directive-type))
                     :TILE_INNER
                     (error "The AstFor ~a should have either of :LOCAL or :TILE_INNER" for))))
           (butnil (&rest list) (loop for l in list if l collect l))
           (newband (parent-band body)
             (declare (type astfor parent-band))
             (ecase (identify-for parent-band)
               (:LOCAL body)
               (:TILE_INNER
                (let ((new-astfor (copy-astfor parent-band)))
                  ;; [TODO] Remove extra bands in new-astfor?
                  ;; [TODO] Rewrite BY=1 (since this region is never vectorized.)
                  (setf (astfor-by new-astfor) (expr-const 1 :int32)
                        (astfor-body new-astfor) body)
                  new-astfor)))))
    (let ((insert-barrier-p (some #'(lambda (x) (eql (identify-for x) :LOCAL)) data-reuse))
;;           (guard-triggers (loop for d in data-reuse if (eql (identify-for d) :TILE_INNER) collect d))
          (transfer
            (make-block
             (loop for aref in narefs
                   for tfexpr in transfer
                   for tfbody = (make-block (list tfexpr))
                   for ids = nil
                   collect
                   (progn
                     (loop for band in (append (list prefetch-inner) (reverse data-reuse))
                           for stride in (reverse (iteration-space-strides (getattr aref :space)))
                           unless (expr-equal-to stride 0) do
                             (push (astfor-idx band) ids)
                             (setf tfbody (newband band tfbody)))
                     (loop for idx in ids
                           for new = (format nil "~a_1" idx)
                           do (setf tfbody (ast-rewrite-astfor-idx ctx tfbody idx new)))
                     tfbody)))))
      (make-block
       (butnil
        transfer
        (if insert-barrier-p (make-astexpr (make-node :Render :BARRIER nil nil) nil) nil))))))

(defun ast-gather-buffer-loads (ast blueprints)
  (let ((loads))
    (labels ((find-from-user (id)
               (find (princ-to-string id) blueprints :key (alexandria:compose #'princ-to-string #'node-id) :test #'equalp))
             (handler (ast)
               (etypecase ast
                 (User
                  (let ((usr (find-from-user (user-name ast))))
                    (assert usr () "The user ~a is not appeared in the original blueprint." ast)
                    (when (eql :EXPR (node-type usr))
                      (dolist (a (caten/codegen/blueprint:expr-gather-buffer-loads usr))
                        (assert (eql (node-type a) :Aref))
                        (push a loads)))))
                 (AstBlock (mapc #'handler (astblock-body ast)))
                 (AstFor (handler (astfor-body ast)))
                 (AstExpr ast)
                 (AstIf (handler (astif-then-node ast)) (handler (astif-else-node ast)))
                 (null))))
      (handler ast)
      (remove-duplicates loads :key #'(lambda (x) (getattr x :storage-id))))))

(defun ast-make-transfer-body (ctx loads narefs astfor-list)
  (declare (type list loads))
  (assert (every #'(lambda (x) (eql (node-type x) :Aref)) loads))
  (assert (every #'astfor-tile-parent astfor-list))
  (loop for aref in loads
        for target in narefs
        collect
        (let ((space (caten/codegen/shape-inference::copy-iteration-space (getattr aref :space)))
              (type-relay (make-inferred-type (list (getattr aref :buffer)) (list (getattr target :buffer)))))
          (setf (iteration-space-views space) (copy-list (iteration-space-views space)))
          (loop for offset in (iteration-space-views space)
                for nth upfrom 0
                ;; [TODO] If the tile was nested for multiple times?
                for idx = (expr-const (ctx-intern ctx (astfor-idx (astfor-tile-parent (nth nth astfor-list)))) :int64)
                for upfrom = (or (car (nth nth (iteration-space-views space))) 0)
                for view = (or (nth nth (iteration-space-views space)) (list 0 1 1 t))
                do (setf (nth nth (iteration-space-views space)) view
                         (car (nth nth (iteration-space-views space))) (expr-add (expr-const upfrom :int64) idx)))
          (setf (relay-write-iters type-relay) (list (getattr target :space))
                (relay-read-iters type-relay) (list space))
          (make-astexpr
           (make-node :JIT :EXPR (list (ctx-shared-mem-id ctx (getattr aref :storage-id))) (list (getattr aref :storage-id))
                      :EXPR (make-expr :graph (make-graph aref) :out aref)
                      :_type_relay type-relay
                      :iterations (map 'list #'(lambda (x) (expr-const (ctx-intern ctx (format nil "~a_1" (astfor-idx x))) :int64)) astfor-list))
           nil))))

(defun aref->shared-memory-decl (aref)
  (declare (type node aref))
  (assert (eql (node-type aref) :Aref))
  (make-astexpr
   (make-node :Render :DEFINE-SHARED-MEMORY (list (getattr aref :storage-id)) nil
              :dtype (caten/runtime:buffer-dtype (getattr aref :buffer))
              :size (reduce #'* (caten/runtime:buffer-shape (getattr aref :buffer))))
   nil))

(defun aref->tiled-aref (ctx aref blocksize offsets &key (use-offsets nil))
  (declare (type node aref))
  (assert (eql (node-type aref) :Aref))
  (let ((shape) (stride) (last-stride 1)
        (negative-offsets (map 'list #'(lambda (x) (expr-neg (expr-const (ctx-intern ctx x) :int64))) offsets)))
    (loop for s in (reverse (iteration-space-strides (getattr aref :space)))
          if (expr-equal-to s 0)
            do (push 1 shape) (push 0 stride)
          else
            do (push blocksize shape) (push last-stride stride) (setf last-stride (* blocksize last-stride)))
    (flet ((asc (x) (expr-const x :int64)))
      (make-node :JIT :Aref (node-writes aref) (node-reads aref)
                 :storage-id (ctx-shared-mem-id ctx (getattr aref :storage-id))
                 :buffer (caten/runtime:make-buffer
                          shape stride (caten/runtime:buffer-dtype (getattr aref :buffer))
                          (loop repeat (length shape) collect nil)
                          :device 'caten/codegen/shape-inference::RelayBuffer)
                 :space (make-iteration-space :shape (map 'list #'asc shape) :strides (map 'list #'asc stride)
                                              :views (loop for o in negative-offsets collect (list (if use-offsets o 0) 1 1)))))))

(defun make-aref-rewriter (base new)
  #'(lambda (x)
      (when (and (eql (node-type x) :Aref)
                 (eql (getattr x :storage-id) (getattr base :storage-id)))
        new)))

(defun ast-rewrite-expr (rewriters blueprints ast)
  (labels ((rewrite (node)
             (dolist (r rewriters)
               (let ((o (funcall r node)))
                 (when o (return-from rewrite o)))))
           (handler (ast)
             (etypecase ast
               (User
                (labels ((find-from-user (id)
                           (find (princ-to-string id) blueprints :key (alexandria:compose #'princ-to-string #'node-id) :test #'equalp)))
                  (let ((usr (find-from-user (user-name ast))))
                    (assert usr () "The user ~a is not appeared in the original blueprint." ast)
                    (when (eql :EXPR (node-type usr))
                      (setf (graph-nodes (expr-graph (getattr usr :EXPR)))
                            (loop for node in (graph-nodes (expr-graph (getattr usr :EXPR)))
                                  if (eql (node-type node) :Aref)
                                    collect (or (rewrite node) node)
                                  else
                                    collect node))))))
               (AstBlock (mapc #'handler (astblock-body ast)))
               (AstFor (handler (astfor-body ast)))
               (AstExpr)
               (AstIf (handler (astif-then-node ast)) (handler (astif-else-node ast)))
               (null))))
    (handler ast)
    ast))

(defun ast-rewrite-directive (ctx ast blueprints &aux (prefetch-loops))
  "Rewrites the PACKED_INNER and PACKED_OUTER directives:
- PACKED_INNER is a trigger to generate the unrolled part and the reminder part.
- PACKED_OUTER is completly unrolled and the astfor is removed."
  (labels ((handler (ast vectorized-idx data-reuse)
             ;; Reminder-idx-list
             (etypecase ast
               (User (copy-user ast))
               (AstBlock
                (let ((new-block (copy-astblock ast)))
                  (setf (astblock-body new-block) (map 'list #'(lambda (x) (handler x vectorized-idx data-reuse)) (astblock-body ast)))
                  new-block))
               (ASTFor
                (let ((new-astfor (copy-astfor ast))
                      (marks (astfor-marks ast))
                      (mark)
                      (data-reuse
                        (append
                         data-reuse
                         (when (some #'(lambda (x) (find x (astfor-marks ast) :key #'directive-type :test #'equalp)) `("TILE_INNER" "LOCAL"))
                           (list ast)))))
                  (assert (<= (length marks) 2)) ;; Note: This assertion is not enough to detect PACK_OUTER+TILE_INNER ...
                  (setf (astfor-marks new-astfor)
                        (loop for m in marks
                              ;; GLOBAL/LOCAL/PARALLEL -> Rewriting is not done in this function
                              ;; TILE_OUTER/TILE_INNER -> Just a trigger for marking data reuse points.
                              if (find (directive-type m) `("GLOBAL" "LOCAL" "PARALLEL") :test #'equalp)
                                collect m
                              else if (null (find (directive-type m) `("TILE_INNER" "TILE_OUTER") :test #'equalp))
                                     do (setf mark m)))
                  (if mark
                      (cond
                        ((equalp (directive-type mark) "PACKED_INNER")
                         (setf (astfor-body new-astfor) (handler (astfor-body ast) vectorized-idx data-reuse))
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
                                (packed-body (handler (astfor-body ast) (append vectorized-idx (list (astfor-idx ast))) data-reuse))
                                (reminder-body (handler (astfor-body ast) vectorized-idx data-reuse)))
                           (multiple-value-bind (packed-for reminder-for)
                               (astfor-make-reminder-astfor ctx ast n-pack packed-body reminder-body)
                             ;; [TODO] Test LOCAL+VECTORIZE Fusion generating a reminder.
                             (setf (astfor-marks packed-for) (astfor-marks new-astfor)
                                   (astfor-tile-parent packed-for) (astfor-tile-parent new-astfor))
                             (make-block (list packed-for reminder-for)))))
                        ((equalp (directive-type mark) "PREFETCH_OUTER")
                         ;; Note:
                         ;; - Here, we assume the undernearth block is TILED by BLOCKDIM x BLOCKDIM x BLOCKDIM.
                         ;;   - 1. Transfer the buffers used in the child into the Shared Memory.
                         ;;     - Here, A/B may not be needed to be transferred. Increment the pointer instead.
                         ;;   - 2. Insert DEFINE-SHARED-MEMORY
                         ;;   - 3. Insert the BARRIER
                         ;;   - 4. Prefetch MUST BE 1D RIGHT?
                         ;; 1. handlerを先に展開する
                         ;; 2. Handler内部で読まれているAREFの一覧を取得する
                         ;; 3. それら全部SharedMemoryに移すように書けばいい
                         ;; 4. PREFETCH should be used at once in a single kernel.
                         ;; 5. PACKED_OUTER ===> Can be much easier to rewrite? Only the innermost loops should be updated.
                         (assert (and (astfor-p (astfor-body ast)) (find "PREFETCH_INNER" (astfor-marks (astfor-body ast)) :key #'directive-type :test #'equalp))
                                 ()
                                 "Nothing can be inserted between PREFETCH_INNER and PREFETCH_OUTER, and it should be one-dimensional!")
                         (let* ((loads (ast-gather-buffer-loads (astfor-body ast) blueprints)) ;; List of aref used in the child.
                                (blocksize (directive-amount mark))
                                (blocksize (if (listp blocksize) (car blocksize) blocksize))
                                (ls (append data-reuse (list (astfor-body ast))))
                                (tids (map 'list (alexandria:compose #'astfor-idx #'astfor-tile-parent) ls))
                                (narefs (map 'list #'(lambda (x) (aref->tiled-aref ctx x blocksize tids)) loads))
                                (narefs-shifted (map 'list #'(lambda (x) (aref->tiled-aref ctx x blocksize tids :use-offsets t)) loads))
                                (transfer-body (ast-make-transfer-body ctx loads narefs ls))
                                (guard (ast-make-prefetch-barrier ctx data-reuse (astfor-body ast) transfer-body narefs))
                                (rewriters (map 'list #'make-aref-rewriter loads narefs-shifted))
                                (smemdecls (map 'list #'aref->shared-memory-decl narefs)))
                           (assert (every #'astfor-tile-parent ls))
                           ;; Note:
                           ;; 1. make shared memory strided array
                           ;; 2. rewrite-shared-access rewriting rule (replace aref)
                           ;; 3. todo: tweak the cache size, val_6/val_4 only requires BLOCKSIZE * BLOCKSIZE region
                           ;; Moved to the top of the iteration
                           (push (list new-astfor smemdecls guard) prefetch-loops)
                           (ast-rewrite-expr rewriters blueprints (handler (astfor-body ast) vectorized-idx data-reuse))))
                        ((equalp (directive-type mark) "PREFETCH_INNER")
                         (setf (astfor-body new-astfor) (handler (astfor-body ast) vectorized-idx data-reuse))
                         new-astfor)
                        (T (error "The directive ~a transformation is not supported." mark)))
                      (progn
                        (setf (astfor-body new-astfor) (handler (astfor-body ast) vectorized-idx data-reuse))
                        new-astfor))))
               (AstExpr ast)
               (AstIf
                (let ((new-if (copy-astif ast)))
                  (setf (astif-then-node new-if) (handler (astif-then-node ast) vectorized-idx data-reuse)
                        (astif-else-node new-if) (handler (astif-else-node ast) vectorized-idx data-reuse))
                  new-if))
               (null))))
    (let ((final-ast (handler ast nil nil)))
      (labels ((newband (prefetch body)
                 (multiple-value-bind (band smemdecl transfer) (apply #'values prefetch)
                   ;; Sink the band undernarth of the outermost band.
                   (if (and (typep body 'AstFor)
                            (or
                             (null (astfor-tile-parent body))
                             (find "LOCAL" (astfor-marks body) :test #'equalp :key #'directive-type)))
                       (let ((new-astfor (copy-astfor body)))
                         (setf (astfor-body new-astfor) (newband prefetch (astfor-body new-astfor)))
                         new-astfor)
                       (if (typep body 'AstIf) ;; Sink
                           (let ((new-astif (copy-astif body)))
                             (assert (null (astif-else-node new-astif)) () "ElseForm is not supported.")
                             (setf (astif-then-node new-astif) (newband prefetch (astif-then-node new-astif)))
                             new-astif)
                           (progn ;; Insert Prefetch Points
                             (setf (astfor-body band) (make-block (append smemdecl (list transfer) (list body))))
                             band))))))
        (dolist (r prefetch-loops)
          (setf final-ast (newband r final-ast)))
        final-ast))))

(defun ast-rewrite-grids (ast &aux (count 0) (idx2count (make-hash-table :test #'equal)) (idx2band (make-hash-table :test #'equal)))
  "Rewrites PARALLEL/GLOBAL/LOCAL directives."
  (labels ((handler (ast)
             (etypecase ast
               (User ast)
               (AstBlock (make-block (map 'list #'handler (astblock-body ast))))
               (AstFor
                (when (null (astfor-marks ast))
                  (return-from handler ast))
                (let ((mark (car (astfor-marks ast))))
                  (cond
                    ((equalp (directive-type mark) "PARALLEL")
                     (setf (astfor-scope ast) :global)
                     ast)
                    ((equalp (directive-type mark) "GLOBAL")
                     ;; idx = global size * thread stride
                     (setf (gethash (astfor-idx ast) idx2count) count
                           (gethash (astfor-idx ast) idx2band) ast)
                     (let* ((new-astfor (copy-astfor ast))
                            (ls (directive-amount mark))
                            (ls (if (numberp ls) ls (car ls)))
                            (gid (astfor-mutate-global new-astfor count ls #'(lambda (body) (incf count) (handler body)))))
                       gid))
                    ((equalp (directive-type mark) "LOCAL")
                     (assert (astfor-tile-parent ast) () "LOCAL should have a parent.")
                     (assert (gethash (astfor-idx (astfor-tile-parent ast)) idx2count) () "GLOBAL should be defined before LOCAL.")
                     (let* ((new-astfor (copy-astfor ast))
                            (ls (directive-amount mark))
                            (ls (if (numberp ls) ls (car ls)))
                            (p (gethash (astfor-idx (astfor-tile-parent ast)) idx2band))
                            (lid (astfor-mutate-local new-astfor p (gethash (astfor-idx (astfor-tile-parent ast)) idx2count) ls #'handler)))
                       lid))
                    ((equalp (directive-type mark) "PREFETCH_INNER") ast)
                    (T (error "Not supported directive: ~a" mark)))))
               (AstExpr Ast)
               (AstIf
                (make-if (astif-condition ast) (handler (astif-then-node ast)) (handler (astif-else-node ast))))
               (null))))
    (handler ast)))
;; ~~ ISL Object <--> Blueprint ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun r/for (idx upfrom below by scope)
  (make-node :Render :FOR nil nil :idx idx :upfrom upfrom :below below :by by :scope scope))

(defun r/endfor (idx)
  (make-node :Render :ENDFOR nil nil :idx idx))

(defun r/if (condition idx)
  (make-node :Render :IF nil nil :condition condition :idx idx))

(defun r/endif (idx)
  (make-node :Render :ENDIF nil nil :idx idx))

(defun create-rendering-graph-nodes (ctx lisp-ast items &aux (space))
  (let ((new-graph) (maximum-rank (ast-get-rank lisp-ast)))
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
                  (when (not
                         (let ((d (expr-detach-loop-bound to)))
                           (and
                            d
                            (eql :LOAD (node-type (expr-out d)))
                            (numberp (getattr (expr-out d) :value))
                            (expr-scalar-equivalent-p upfrom d))))
                    (push idx space)
		    (push (r/for idx upfrom to by scope) new-graph)
		    (lower body)
                    (setf space (remove idx space :test #'string=))
		    (push (r/endfor idx) new-graph)))
		 ((User :name name :args args)
                  (push
                   (unroll-expr (reverse space) (find-user name args)
                                object maximum-rank (context-dynamic-shape-table ctx))
                   new-graph))
		 ((AstIf :condition cond :then-node then :else-node else)
                  (let ((id (gensym "IF")))
		    (push (r/if cond id) new-graph)
		    (lower then)
                    (assert (null else) () "else should be none")
		    (push (r/endif id) new-graph)))
                 ((AstExpr :expr expr :is-defglobal-p defglobal-p)
                  ;; (assert (eql (node-type expr) :EXPR) () "ASTEXPR can only have an EXPR.")
                  (when defglobal-p
                    (push (string-downcase (princ-to-string (car (node-writes expr)))) space))
                  (push expr new-graph))
		 (_
		  (error "create-rendering-graph: ~a should not occur here!" object)))))
      (lower lisp-ast))
    (nreverse new-graph)))

(defun ast-get-rank (ast &aux (rank 0))
  (labels ((handler (ast)
             (etypecase ast
               (User (setf rank (max rank (length (user-args ast)))))
               (AstBlock (loop for x in (astblock-body ast) do (handler x)))
               (AstFor (handler (astfor-body ast)))
               (AstIf (handler (astif-then-node ast)) (handler (astif-else-node ast)))
               (AstExpr)
               (null))))
    (handler ast)
    rank))

(defun dynamic-shape-table (item)
  (let ((table (make-hash-table :test 'equal)))
    (loop for (name . dtype) in (getattr item :dynamic-shapes)
          do (setf (gethash (string-upcase (princ-to-string name)) table) name))
    table))

(defun lower-into-bp-from-polyhedral (ast scheduled-item &key (vectorizes nil))
  (declare (type isl:ast-node ast))
  (assert (eql (node-type scheduled-item) :Schedule-Item))
  (let* ((ctx (make-context :dynamic-shape-table (dynamic-shape-table scheduled-item)))
         (ast (parse-isl-ast ctx (isl::ast-node-handle ast) nil))
         (ast (ast-rewrite-tile-pair ast))
         (ast (ast-rewrite-directive ctx ast (getattr scheduled-item :blueprint)))
         (ast (ast-rewrite-grids (print ast)))
         (ast (ast-rewrite-vectorize ast vectorizes scheduled-item))
         ;; [TODO] Define ASTTree Simplifier. (e.g.: Unrolling a small astfor etc)
         (bp (create-rendering-graph-nodes ctx ast (getattr scheduled-item :blueprint)))
         (bp (caten/codegen/packing:blueprint-upcast-inference bp scheduled-item)))
    bp))
