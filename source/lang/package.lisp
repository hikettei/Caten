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
   #:ctx-render
   #:ctx-render-function
   #:ctx-compile
   ))
