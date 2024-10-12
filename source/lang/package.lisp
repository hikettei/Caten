(in-package :cl-user)

(defpackage :caten/lang
  (:use :cl :caten/air :caten/common.dtype :trivia)
  ;; from script.lisp
  (:export
   #:a/macroexpand-all
   #:a/defun
   #:a/defmacro
   #:make-context-from-list
   ))
