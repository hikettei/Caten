(defpackage :caten/codegen/directive
  (:documentation "Provides various hand-written blueprint rewriting rule for marks.")
  (:use :cl :caten/codegen/expr :caten/codegen/polyhedral-ast :caten/codegen/unroll))

(in-package :caten/codegen/directive)

(defun compute-reminder-for-unroll (mark upfrom below by n-unroll)
  "Creates an AstFor for the reminder part of the unrolled directive"
  (let ((n-unroll (parse-unroll-directive mark)))
    
    ))
