(in-package :cl-user)

(defpackage :caten/lang
  (:use :cl :caten/air :trivia)
  ;; from script.lisp
  (:export
   #:a/macroexpand-all
   #:a/defun
   #:a/defmacro
   ))
