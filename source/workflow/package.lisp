(in-package :cl-user)

(defpackage :caten/workflow
  (:use :cl :caten/air :trivia)
  (:import-from
   :caten/lang
   #:Context
   #:a/defun
   #:ctx-render
   #:ctx-render-function
   #:ctx-compile
   #:ctx-get-code
   #:make-context-from-list
   #:ctx-run
   #:ctx-outputs

   #:ctx-define-and-make-funcall-from-expr
   #:ctx-define-and-make-funcall-from-expr-and-args
   #:ctx-declare-sized-local-var
   #:ctx-declare-local-var
   #:ctx-register-variable
   #:ctx-add-dependency
   #:ctx-make-funcall
   #:Parsed-Form
   #:make-parsed-form
   #:Parsed-Form-Nodes
   #:Parsed-Form-Expr
   #:Parsed-Form-Type
   #:stash-forms
   #:make-const-buffer)
  (:export
   #:defaction
   #:run-action
   ))
