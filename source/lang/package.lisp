(in-package :cl-user)

(defpackage :caten/lang
  (:use :cl :caten/air :caten/common.dtype :trivia :caten/common.documentation)
  ;; from script.lisp
  (:export
   #:a/macroexpand-all
   #:a/defun
   #:a/defmacro
   #:make-context-from-list
   #:Context
   #:ctx-args
   #:ctx-outputs
   #:ctx-render
   #:ctx-render-function
   #:ctx-get-code
   #:ctx-compile
   #:ctx-run

   #:ctx-define-and-make-funcall-from-expr
   #:ctx-define-and-make-funcall-from-expr-and-args
   #:ctx-declare-sized-local-var
   #:ctx-declare-local-var
   #:ctx-make-funcall
   #:ctx-register-variable
   #:ctx-add-dependency
   #:Parsed-Form
   #:make-parsed-form
   #:Parsed-Form-Nodes
   #:Parsed-Form-Expr
   #:Parsed-Form-Type

   #:stash-forms
   #:make-const-buffer
   ))
