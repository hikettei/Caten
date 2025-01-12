(defpackage :caten/codegen/polyhedral-ast
  (:documentation "An intermidate AST to parse ISL AST into Blueprint.")
  (:use :cl :caten/codegen/expr :caten/codegen/expr-cache :caten/air :caten/codegen/shape-inference :trivia)
  (:export
   #:ASTBlock #:astblock-p #:astblock-body
   #:make-block
   #:satblock-body

   #:User #:user-p #:copy-user
   #:make-user
   #:user-name
   #:user-args
   #:user-unroll
   #:user-vectorize
   #:user-late-unroll-info
   
   #:ASTFor #:astfor-p
   #:make-for
   #:copy-astfor
   #:astfor-idx
   #:astfor-from
   #:astfor-to
   #:astfor-by
   #:astfor-body
   #:astfor-scope
   #:astfor-marks

   #:AstIf #:astif-p #:copy-astif
   #:make-if
   #:astif-condition
   #:astif-then-node
   #:astif-else-node

   #:ASTExpr #:make-astexpr #:astexpr-p #:astexpr-expr #:astexpr-is-defglobal-p

   #:map-ast-tree
   #:copy-and-assign-expr
   #:copy-and-assign-ast-tree
   #:unroll-ast
   #:packing-ast
   #:late-rewrite-pack->unroll))

(in-package :caten/codegen/polyhedral-ast)
;; ~~ ISL AST <-> Lisp Intermidate Object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defstruct (ASTBlock
              (:constructor make-block (body)))
    (body body :type list))

  (defstruct (User
              (:constructor make-user (name args)))
    "T_name(index)"
    (name name :type string) (args args :type list) (unroll nil :type list) (vectorize nil :type list) (late-unroll-info nil :type list))

  (defstruct (ASTExpr
              (:constructor make-astexpr (expr is-defglobal-p)))
    (expr expr :type Node)
    (is-defglobal-p is-defglobal-p :type boolean))

  (defstruct (ASTFor
              (:constructor make-for (idx from to by body)))
    (idx idx :type string)
    (from from :type Expr)
    (to to :type Expr)
    (by by :type Expr)
    (body body :type (or ASTBlock User ASTFor ASTIF))
    (scope :local :type (member :local :global))
    (marks nil :type list))

  (defstruct (AstIf
              (:constructor make-if (condition then-node else-node)))
    (condition condition :type Expr)
    (then-node then-node :type (or ASTBlock User ASTFOR ASTIF))
    (else-node else-node :type (or ASTBlock User ASTFOR ASTIF null))))

(defun map-ast-tree (f ast)
  "f = (lambda (ast &rest args) ...)"
  (declare (type function f))
  (etypecase ast
    (AstBlock
     (let ((new-body (map 'list #'(lambda (x) (map-ast-tree f x)) (astblock-body ast))))
       (funcall f ast new-body)))
    (User (funcall f ast))
    (AstExpr ast)
    (AstFor
     (let ((new-body (map-ast-tree f (astfor-body ast))))
       (funcall f ast new-body)))
    (AstIf
     (let ((new-body (map-ast-tree f (astif-then-node ast))))
       (assert (null (astif-else-node ast)) () "map-ast-tree: astif-else-node should be nil")
       (funcall f ast new-body)))))
               
(defun copy-and-assign-expr (expr idx value)
  (declare (type string idx) (type expr expr))
  (let* ((new-expr (make-expr :graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (expr-graph expr)))) :out (copy-node (expr-out expr)))))
    (loop for node in (graph-nodes (expr-graph new-expr))
          if (and (eql (node-type node) :Load)
                  (symbolp (getattr node :value))
                  (equalp idx (princ-to-string (getattr node :value))))
            do (setf (getattr node :value) value))
    new-expr))

(defun copy-and-assign-ast-tree (ast idx value &key (unroll-at nil) (mode :unroll) (pack-size nil))
  (declare (type string idx) (type (member :unroll :packing) mode))
  (flet ((e (expr) (copy-and-assign-expr expr idx value)))
    (map-ast-tree
     #'(lambda (ast &rest forms)
         (etypecase ast
           (AstBlock
            (assert (= (length forms) 1))
            (make-block (first forms)))
           (User
            (ecase mode
              (:unroll
               (let ((user (copy-user ast)))
                 (setf (user-args user) (print (map 'list #'e (print (user-args user))))
                       (user-unroll user) (copy-list (user-unroll user))
                       (user-vectorize user) (copy-list (user-vectorize user))
                       (user-late-unroll-info user) (copy-list (user-late-unroll-info user)))
                 (when unroll-at (push (cons unroll-at value) (user-unroll user)))
                 user))
              (:packing
               (let ((user (copy-user ast)))
                 (setf (user-args user) (user-args user)
                       (user-unroll user) (copy-list (user-unroll user))
                       (user-vectorize user) (copy-list (user-vectorize user)) (user-late-unroll-info user) (copy-list (user-late-unroll-info user))
                       (user-late-unroll-info user) (copy-list (user-late-unroll-info user)))
                 (when unroll-at
                   (assert (numberp pack-size) () "Packing size should be constant!")
                   (push (list unroll-at value pack-size) (user-vectorize user))
                   (push (list idx pack-size unroll-at) (user-late-unroll-info user)))
                 user))))
           (AstFor
            (assert (= (length forms) 1))
            (let ((new-for (copy-astfor ast)))
              (setf (astfor-body new-for) (first forms)
                    (astfor-from new-for) (e (astfor-from new-for))
                    (astfor-to new-for) (e (astfor-to new-for))
                    (astfor-by new-for) (e (astfor-by new-for)))
              new-for))
           (AstExpr (make-astexpr (astexpr-expr ast) (astexpr-is-defglobal-p ast)))
           (AstIf
            (assert (= (length forms) 1))
            (let ((new-if (copy-astif ast)))
              (setf (astif-then-node new-if) (first forms)
                    (astif-condition new-if) (e (astif-condition new-if)))
              new-if))))
     ast)))

(defun shift-ast (ast idx unroll-at n-unroll &key (mode :unroll))
  (declare (type (member :unroll :packing) mode))
  (map-ast-tree
   #'(lambda (ast &rest forms)
       (etypecase ast
         (AstBlock
          (assert (= (length forms) 1))
          (make-block (first forms)))
         (User
          (ecase mode
            (:unroll
             (let ((users
                     (map 'list #'(lambda (n) (copy-and-assign-ast-tree ast idx n :unroll-at unroll-at :mode mode)) (alexandria:iota n-unroll))))
               (make-block users)))
            (:packing
             (copy-and-assign-ast-tree ast idx 0 :unroll-at unroll-at :mode mode :pack-size n-unroll))))
         (AstExpr (make-astexpr (astexpr-expr ast) (astexpr-is-defglobal-p ast)))
         (AstFor
          (assert (= (length forms) 1))
          (let ((new-for (copy-astfor ast)))
            (setf (astfor-body new-for) (first forms)
                  (astfor-from new-for) (astfor-from new-for)
                  (astfor-to new-for) (astfor-to new-for)
                  (astfor-by new-for) (astfor-by new-for))
            new-for))
         (AstIf
          (assert (= (length forms) 1))
          (let ((new-if (copy-astif ast)))
            (setf (astif-then-node new-if) (first forms)
                  (astif-condition new-if) (astif-condition new-if))
            new-if))))
   ast))

(defun unroll-ast (ast idx unroll-at n-unroll)
  (shift-ast ast idx unroll-at n-unroll :mode :unroll))

(defun packing-ast (ast idx packing-at n-packing)
  (shift-ast ast idx packing-at n-packing :mode :packing))

(defun late-rewrite-pack->unroll (ast &key (unrolled-as nil))
  "Rewrites the packed user (which is failed to vectorize) as an unrolled user."
  (check-type ast user)
  (flet ((unroll (info prev-ast n-unroll-as)
           (multiple-value-bind (idx n-unroll unroll-at) (apply #'values info)
             (let ((n-unroll (or n-unroll-as n-unroll)))
               (make-block
                (map 'list #'(lambda (n) (copy-and-assign-ast-tree prev-ast idx n :unroll-at unroll-at :mode :unroll)) (alexandria:iota n-unroll)))))))
    (let ((info (user-late-unroll-info ast)))
      (dotimes (i (length info))
        (setf ast (unroll (nth i info) ast (nth i unrolled-as))))
      ast)))
