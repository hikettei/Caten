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
   #:ast-rewrite-vectorize))

(in-package :caten/codegen/packing)

(defstruct (Vectorize
            (:constructor vectorize (dims &key (applicable-p #'identity) (rewriter nil))))
  (dims dims :type list)
  (applicable-p applicable-p :type function)
  (rewriter rewriter :type function))

(defun apply-vectorize (vectorize)

  )

(defun TensorCore (dims)
  (declare (type list dims))
  (assert (and (= (length dims) 3) (every #'integerp dims)) () "TensorCore: dims are three-dimensional list of integers.")
  (Vectorize dims :applicable-p #'identity :rewriter #'identity))
;; ~~ Vectorizer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun retrive-expr-with-id (id schedule-item)
  (declare (type string id) (type node schedule-item))
  
  )

(defun ast-rewrite-vectorize (ast vectorize-rules schedule-item)
  "Rewrites the packed user in the AST with the vectorize-rules. The earlier rules have the higher priority.
If some users are failed to be vectorized, they are rewritten as unroll."
  (declare (type list vectorize-rules))
  (assert (eql (node-type schedule-item) :Schedule-Item))
  (when (null vectorize-rules)
    (return-from ast-rewrite-vectorize ast))
  
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
                (or (find-if #'(lambda (rule) (apply-vectorize rule)) vectorize-rules)
                    (late-rewrite-pack->unroll ast)))))
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
   
