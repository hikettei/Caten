(defpackage :caten/codegen/packing
  (:documentation "`caten/codegen/packing` provides a transformation tool to
replace the packed region (defined in auto-scheduler.lisp, class Packing) with the vectorized ops.
The class `Vectorize` will provide the information of the vectorized region, passed via define-auto-scheduler macro.
TensorCore optimization is also implemented as a part of Vectorize.
")
  (:use :cl :caten/air :caten/codegen/polyhedral-ast)
  (:export
   #:Vectorize
   #:TensorCore
   #:ast-rewrite-vectorize)
  (:export
   #:Vectorize-Config
   #:Vectorize-Config-Vectorize
   #:Vectorize-Config-Expr)
  (:export
   #:expr-node-wmma-p))

(in-package :caten/codegen/packing)

(defstruct (Vectorize
            (:constructor vectorize (name dims &key (applicable-p #'identity) (rewriter nil))))
  (name name :type keyword)
  (dims dims :type list)
  (applicable-p applicable-p :type function)
  (rewriter rewriter :type function))

(defstruct Vectorize-Config
  "applicable-p, rewriter will receive the necessary information via this structure to complete vectorize."
  (vectorize (error "vectorize must occur") :type Vectorize)
  (expr (error "exrp must occur") :type node))

(defun apply-vectorize (user vectorize schedule-item)
  (declare (type user user))
  (when (< (length (user-vectorize user)) (length (vectorize-dims vectorize)))
    (return-from apply-vectorize nil))
  (let ((padded
          (append (loop repeat (- (length (user-vectorize user)) (length (vectorize-dims vectorize))) collect 1)
                  (vectorize-dims vectorize))))
    (assert (= (length padded) (length (user-vectorize user))))
    (assert (every #'zerop (map 'list #'second (user-vectorize user))) () "apply-vectorze: range should start from zero.")
    (let ((applicable-p (every #'(lambda (x y) (= 0 (mod x y))) (map 'list #'third (user-vectorize user)) padded))
          (unroll (map 'list #'(lambda (x y) (/ x y)) (map 'list #'third (user-vectorize user)) padded))
          (env (make-vectorize-config :vectorize vectorize :expr (find-user user (user-name user) schedule-item))))
      (when (and
             applicable-p
             ;; [TODO] Judge the elements are contiguous in the memory! (stride must be one if simd)
             (funcall
              (vectorize-applicable-p vectorize)
              env))
        
        ;; -> will return a new render node which computes vectorize-dims region.
        (let ((new-user (funcall (vectorize-rewriter vectorize) env)))
          ;; (replace-blueprint )
          (assert (node-p new-user) () "vectorizer-rewrite must return a node, getting ~a. (vectorize-rule=~a)" new-user vectorize)
          (setf (user-name user) (string-upcase (princ-to-string (node-id new-user))))
          (push new-user (getattr schedule-item :blueprint))
          (late-rewrite-pack->unroll user :unrolled-as unroll))))))

(defun TensorCore (dims &key (name :TensorCore))
  (declare (type list dims))
  (assert (and (= (length dims) 3) (every #'integerp dims)) () "TensorCore: dims are three-dimensional list of integers.")
  (Vectorize name dims :applicable-p #'identity :rewriter #'identity))
;; ~~ Vectorizer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] find-user has a lot of reuse with the find-user in ast-parser.lisp
(defun find-user (user node-id schedule-item &aux (items (getattr schedule-item :blueprint)))
  (declare (type user user) (type node schedule-item))
  (let ((node (find (princ-to-string node-id) items
                    :key (alexandria:compose #'princ-to-string #'node-id)
                    :test #'equalp)))
    (assert node () "~a is not found in ~a" node-id items)
    (assert (eql (node-type node) :EXPR))
    (setf node (copy-node node))
    (let ((base (getattr node :iterations)))
      (setf (getattr node :iterations) (user-args user))
      (if (and (null (user-args user)) (> (length base) 0))
          (setf (getattr node :iterations) base)
          (progn
            (setf (getattr node :iterations) (caten/codegen/shape-inference:ensure-iteration-space-length (length base) (getattr node :iterations)))
            (assert (= (length (getattr node :iterations)) (length base)) () "Before and after the polyhedral compilation, the rank of iteration space should not be changed. ~a -> ~a" (getattr node :iterations) (user-args user)))))
    node))

(defun try-rules (ast vectorize-rules schedule-item)
  (loop for r in vectorize-rules
        for new-ast = (apply-vectorize ast r schedule-item)
        when new-ast do (return-from try-rules new-ast)))

(defun ast-rewrite-vectorize (ast vectorize-rules schedule-item)
  "Rewrites the packed user in the AST with the vectorize-rules. The earlier rules have the higher priority.
If some users are failed to be vectorized, they are rewritten as unroll."
  (declare (type list vectorize-rules))
  (assert (eql (node-type schedule-item) :Schedule-Item))
  (map-ast-tree
   #'(lambda (ast &rest forms)
       (etypecase ast
         (User
          ;; Find the first vectorize item which satisfies applicable-p, otherwise unroll.
          (let ((ast (copy-user ast)))
            (setf (user-args ast) (copy-list (user-args ast))
                  (user-unroll ast) (copy-list (user-unroll ast))
                  (user-vectorize ast) (copy-list (user-vectorize ast))
                  (user-late-unroll-info ast) (copy-list (user-late-unroll-info ast)))
            (if (null (user-vectorize ast))
                ast
                (or (try-rules ast vectorize-rules schedule-item) (late-rewrite-pack->unroll ast)))))
         ;; Otherwise
         (AstBlock
          (assert (= (length forms)))
          (make-block (first forms)))
         (AstFor
          (assert (= (length forms) 1))
          (let ((new-for (copy-astfor ast)))
            (setf (astfor-body new-for) (first forms)
                  (astfor-from new-for) (astfor-from new-for)
                  (astfor-to new-for) (astfor-to new-for)
                  (astfor-by new-for) (astfor-by new-for))
            new-for))
         (AstExpr (make-astexpr (astexpr-expr ast) (astexpr-is-defglobal-p ast)))
         (AstIf
          (assert (= (length forms) 1))
          (let ((new-if (copy-astif ast)))
            (setf (astif-then-node new-if) (first forms)
                  (astif-condition new-if) (astif-condition new-if))
            new-if))))
   ast))
;; ~~ WMMA Rewriter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %expr-node-wmma-p (expr)
  "Returns T if the expr is directly rewritable as TensorCore."
  (declare (type node expr))
  (assert (eql (node-type expr) :EXPR))
  ;; [TODO] Switch to use wmma-rewriter
  ;; (print (caten/codegen/rewriting-rules::wmma-rewriter ))
  (and
   (getattr expr :reduction)
   (let ((nodes (map 'list #'node-type (graph-nodes (caten/codegen/expr:expr-graph (getattr expr :EXPR))))))
     ;; temporary condition just used for the testing...
     (equal nodes `(:AREF :AREF :MOVE :AREF :AREF :MUL :AREF :AREF :ADD)))))

(defun expr-node-wmma-p (env)
  (declare (type Vectorize-Config env))
  (%expr-node-wmma-p (vectorize-config-expr env)))
