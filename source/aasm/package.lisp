(in-package :cl-user)
(defpackage :caten/aasm
  (:use :cl :trivia :caten/air :alexandria)
  ;; (:nicknames :caten

  ;; from tensor-ir.lisp
  (:export
   #:*default-float*
   #:*default-uint*
   #:dtype-t
   
   #:%alloc
   #:%salloc
   #:%load
   #:%stride
   #:%shape
   #:%make-tensor
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
