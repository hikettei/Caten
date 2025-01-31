(in-package :cl-user)
(defpackage :caten/ir
  (:use :cl :trivia :caten/air :alexandria :caten/common.documentation)
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from attrs.lisp
  (:export
   #:JITAble)
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
   #:%index-components)
  ;; from ctx.lisp
  (:export
   #:with-context
   #:with-context-nodes
   #:with-context-from-parents
   #:with-asm
   #:*ctx*
   #:emit)
  ;; from ops.lisp
  (:export
   ;; Binary
   #:*wrap-around-mode*
   #:%add
   #:%sub
   #:%mul
   #:%div
   #:%idiv
   #:%mod
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
  (:export #:optimize-aasm #:minimize-duplicated-symbolic-path))
