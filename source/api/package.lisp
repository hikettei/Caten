(in-package :cl-user)
(defpackage :caten/api
  (:use :cl :caten/air)
  ;; (:nicknames :caten

  ;; from tensor-ir.lisp
  (:export
   #:*default-float*
   #:dtype-t
   
   #:%alloc
   #:%scalar
   #:%load
   )
  ;; from ctx.lisp
  (:export
   #:with-context
   #:*ctx*
   #:emit)
  ;; from ops.lisp
  (:export
   #:%add
   #:%sub
   #:%mul
   #:%div
   #:%neg
   #:%recip
   #:%sqrt
   ))
