(defpackage :caten/codegen/expr-cache
  (:use :cl :caten/codegen/expr :caten/codegen/renderer)
  (:export
   #:Expr-Cache
   #:with-expr-cache
   ))

(in-package :caten/codegen/expr-cache)

(defvar *expr-cache*)

(defclass Expr-Cache ()
  ((cache :type hash-table :initform (make-hash-table :test 'equal) :accessor cache-table)
   (global-counter :initform 0 :type fixnum :accessor expr-cache)))

(defmacro with-expr-cache (() &body body)
  `(let ((*expr-cache* (make-instance 'Expr-Cache)))
     ,@body))

(defun expr-id (num) (format nil "_expr_id_~a" num))

(defmethod stash-expr ((expr-cache Expr-Cache) (expr Expr))

  )
