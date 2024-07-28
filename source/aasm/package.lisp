(in-package :cl-user)
(defpackage :caten/aasm
  (:use :cl :trivia :caten/air :alexandria)
  ;; (:nicknames :caten

  ;; from tensor-ir.lisp
  (:export
   #:*default-order*
   #:*default-float*
   #:*default-uint*
   #:*default-int*
   #:dtype-t

   #:%uconst
   #:%iconst
   #:%fconst
   
   #:%alloc
   #:%salloc
   #:%load
   #:%store
   #:%stride
   #:%shape
   #:default-permute
   #:%make-tensor
   #:%index-components
   )
  ;; from ctx.lisp
  (:export
   #:with-context
   #:with-context-nodes
   #:with-asm
   #:*ctx*
   #:emit)
  ;; from ops.lisp
  (:export
   ;; Binary
   #:%add
   #:%sub
   #:%mul
   #:%div
   #:%move
   #:%and
   #:%or
   #:%max
   #:%gcd

   ;; Unary
   #:%neg
   #:%recip
   #:%sqrt
   #:%not
   #:%cast

   ;; CMP
   #:%!= #:%=
   #:%< #:%> #:%<= #:%>=
   )
  ;; from constant-folding.lisp
  (:export
   #:fold-constant)
  ;; from view.lisp
  (:export
   #:infer-tensor-info
   #:%view
   #:%reshape
   )
  ;; from logical.lisp
  (:export
   #:%where)

  ;; from helpers.lisp
  (:export #:dtype->lisp #:dtype/cast)
  ;; from optimizers.lisp
  (:export #:optimize-aasm)
  )
