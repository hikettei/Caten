(defpackage :caten/codegen/polyhedral-ast
  (:documentation "An intermidate AST to parse ISL AST into Blueprint.")
  (:use :cl :caten/codegen/expr :caten/codegen/expr-cache :caten/air :caten/codegen/shape-inference :trivia)
  (:export
   #:ASTBlock
   #:make-block
   #:satblock-body

   #:User
   #:make-user
   #:user-name
   #:user-args

   #:ASTFor
   #:make-for
   #:copy-astfor
   #:astfor-idx
   #:astfor-from
   #:astfor-to
   #:astfor-by
   #:astfor-body
   #:astfor-scope
   #:astfor-marks

   #:AstIf
   #:make-if
   #:astif-condition
   #:astif-then-node
   #:astif-else-node

   #:map-ast-tree
   #:copy-and-assign-expr
   #:copy-and-assign-ast-tree))

(in-package :caten/codegen/polyhedral-ast)
;; ~~ ISL AST <-> Lisp Intermidate Object ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(eval-when (:execute :compile-toplevel :load-toplevel)
  (defstruct (ASTBlock
              (:constructor make-block (body)))
    (body body :type list))

  (defstruct (User
              (:constructor make-user (name args)))
    "T_name(index)"
    (name name :type string) (args args :type list))

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
    (AstFor
     (let ((new-body (map-ast-tree f (astfor-body ast))))
       (funcall f ast new-body)))
    (AstIf
     (let ((new-body (map-ast-tree f (astif-then-node ast))))
       (assert (astif-else-node ast) () "map-ast-tree: astif-else-node should be nil")
       (funcall f ast new-body)))))
               
(defun copy-and-assign-expr (expr idx value)
  (declare (type string idx) (type fixnum value) (type expr expr))
  (let* ((new-expr (make-expr :graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (expr-graph expr)))) :out (copy-node (expr-out expr)))))
    (loop for node in (graph-nodes (expr-graph new-expr))
          if (and (eql (node-type node) :Load)
                  (symbolp (getattr node :value))
                  (equalp idx (princ-to-string (getattr node :value))))
            do (setf (getattr node :value) value))
    new-expr))

(defun copy-and-assign-ast-tree (ast idx value)
  (declare (type string idx) (type fixnum value))
  (flet ((e (expr) (copy-and-assign-expr expr idx value)))
    (map-ast-tree
     #'(lambda (ast &rest forms)
         (etypecase ast
           (AstBlock
            (assert (= (length forms) 1))
            (make-block (first forms)))
           (User
            (make-user (user-name ast) (map 'list #'e (user-args ast))))
           (AstFor
            (assert (= (length forms) 1))
            (let ((new-for (copy-astfor ast)))
              (setf (astfor-body new-for) (first forms)
                    (astfor-from new-for) (e (astfor-from new-for))
                    (astfor-to new-for) (e (astfor-to new-for))
                    (astfor-by new-for) (e (astfor-by new-for)))
              new-for))
           (AstIf
            (assert (= (length forms) 1))
            (let ((new-if (copy-astif ast)))
              (setf (astif-then-node new-if) (first forms)
                    (astif-condition new-if) (e (astif-condition new-if)))
              new-if))))
     ast)))
