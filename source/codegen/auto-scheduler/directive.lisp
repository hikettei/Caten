(defpackage :caten/codegen/directive
  (:documentation "Provides various hand-written blueprint rewriting rule for marks.")
  (:use :cl :caten/codegen/expr :caten/codegen/polyhedral-ast))

(in-package :caten/codegen/directive)

(defun compute-reminder-for-unroll (upfrom below by n-unroll)
  "Renders a loop for the reminder part"
  
  )
