(defpackage :caten/codegen/directive
  (:documentation "Provides various hand-written blueprint rewriting rule for marks.")
  (:use :cl :caten/codegen/expr :caten/codegen/polyhedral-ast :caten/codegen/unroll)
  (:export
   #:make-unrolled-body
   #:compute-reminder-for-unroll))

(in-package :caten/codegen/directive)

(defun make-unrolled-body (body nth)
  "Creates a copy of ASTFor body with the idx is set to nth."
  (declare (type ASTFor body) (type fixnum nth))
  (copy-and-assign-ast-tree (astfor-body body) (astfor-idx body) nth))

(defun compute-reminder-size ()
  ;; (mod n-unroll)
  )
(defun compute-reminder-for-unroll (user body n-unroll)
  "Creates an AstFor for the reminder part of the unrolled directive"
  (declare (type ASTFor user body) (type fixnum n-unroll))
  
  )
