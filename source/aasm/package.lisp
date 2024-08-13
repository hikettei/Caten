(in-package :cl-user)
(defpackage :caten/aasm
  (:use :cl :trivia :caten/air :alexandria)
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from tensor-ir.lisp
  (:export
   #:*default-order*
   #:*default-float*
   #:*default-uint*
   #:*default-int*

   #:%uconst
   #:%iconst
   #:%fconst
   
   #:%alloc
   #:%salloc
   #:%load
   #:%store
   #:%stride
   #:%shape
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
   #:%xor
   #:%max
   #:%gcd

   ;; Unary
   #:%sin
   #:%log2
   #:%exp2
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
  ;; from optimizers.lisp
  (:export #:optimize-aasm)
  )
