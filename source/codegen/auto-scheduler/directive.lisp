(defpackage :caten/codegen/directive
  (:documentation "Provides various hand-written blueprint rewriting rule for marks.")
  (:use :cl :caten/codegen/expr :caten/codegen/polyhedral-ast :caten/codegen/unroll :caten/air)
  (:import-from :caten/codegen/scop #:expr-detach-loop-bound)
  (:export
   #:make-unrolled-body
   #:compute-reminder-for-unroll))

(in-package :caten/codegen/directive)

(defun make-unrolled-body (body nth)
  "Creates a copy of ASTFor body with the idx is set to nth."
  (declare (type ASTFor body) (type fixnum nth))
  (copy-and-assign-ast-tree (astfor-body body) (astfor-idx body) nth))

(defmethod astfor-compute-reminder ((astfor ASTFor))
  (let ((below (expr-detach-loop-bound (astfor-to astfor)))
        (incremental (astfor-by astfor)))
    (assert (eql :Load (node-type (expr-out incremental))) () "The AstFor is not an SCOP.")
    ;; TODO(hikettei): wanna assert below > from
    (values (expr-mod (expr-sub below (astfor-from astfor)) incremental) below)))

(defun compute-reminder-for-unroll (user body)
  "Creates an AstFor for the reminder part of the unrolled directive."
  (declare (type ASTFor body))
  (multiple-value-bind (reminder size) (astfor-compute-reminder user)
    (make-for
     (astfor-idx body)
     (expr-add (astfor-from body) (expr-sub size reminder))
     (expr-< (expr-const (intern (astfor-idx body)) :int64) (expr-add (astfor-from body) size))
     (expr-const 1 :int64)
     (copy-and-assign-ast-tree (astfor-body body) (astfor-idx user) 0))))
