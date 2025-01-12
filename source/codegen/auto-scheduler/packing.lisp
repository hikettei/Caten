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
   #:Vectorize-Config-Vectorize))

(in-package :caten/codegen/packing)

(defstruct (Vectorize
            (:constructor vectorize (name dims &key (applicable-p #'identity) (rewriter nil))))
  (name name :type keyword)
  (dims dims :type list)
  (applicable-p applicable-p :type function)
  (rewriter rewriter :type function))

(defstruct Vectorize-Config
  "applicable-p, rewriter will receive the necessary information via this structure to complete vectorize."
  (vectorize (error "vectorize must occur") :type Vectorize))

(defun apply-vectorize (user vectorize)
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
          (env (make-vectorize-config :vectorize vectorize)))
      (when (and
             applicable-p
             ;; [TODO] Judge the elements are contiguous in the memory! (stride must be one if simd)
             (funcall
              (vectorize-applicable-p vectorize)
              env))
        (print unroll)
        (print vectorize)
        (print user)
        (print (funcall (vectorize-rewriter vectorize) env))
        ;; And finally return the vectorized ...
        nil))))

(defun TensorCore (dims &key (name :TensorCore))
  (declare (type list dims))
  (assert (and (= (length dims) 3) (every #'integerp dims)) () "TensorCore: dims are three-dimensional list of integers.")
  (Vectorize name dims :applicable-p #'identity :rewriter #'identity))
;; ~~ Vectorizer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun retrive-expr-with-id (id schedule-item)
  (declare (type string id) (type node schedule-item))
  nil
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
                (or (find-if #'(lambda (rule) (apply-vectorize ast rule)) vectorize-rules)
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
   
