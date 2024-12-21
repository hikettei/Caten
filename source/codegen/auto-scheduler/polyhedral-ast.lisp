(defpackage :caten/codegen/polyhedral-ast
  (:documentation "
Transform the Polyhedral IR into the Blueprint IR.
```
[ISL Polyhedral IR] ==> <Polyhedral-AST> ===> Caten Blueprint IR
```
scop.lisp for the opposite things.
")
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
   #:astfor-idx
   #:astfor-from
   #:astfor-to
   #:astfor-by
   #:astfor-body
   #:astfor-scope

   #:AstIf
   #:make-if
   #:astif-condition
   #:astif-then-node
   #:astif-else-node))

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
    (scope :local :type (member :local :global)))

  (defstruct (AstIf
              (:constructor make-if (condition then-node else-node)))
    (condition condition :type Expr)
    (then-node then-node :type (or ASTBlock User ASTFOR ASTIF))
    (else-node else-node :type (or ASTBlock User ASTFOR ASTIF null))))
