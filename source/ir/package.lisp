(defpackage :caten/ir
  (:use :cl :trivia :caten/air :alexandria :caten/common.documentation)
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from ops.lisp
  (:export
   #:JITAble)
  ;; from tensor-ops.lisp
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
  ;; from helpers.lisp
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
   #:%max #:%min
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
   #:infer-tensor-info
   #:%view
   #:%reshape
   #:%where)
  ;; from simplifier.lisp
  (:export #:optimize-aasm #:minimize-duplicated-symbolic-path #:fold-constant)
  ;; from render-ops.lisp
  ;; AST
  (:export #:with-blueprint #:get-caten-function)
  ;; ASTOps
  (:export #:%range #:%dotimes #:%if #:%when #:%progn #:%global #:%barrier #:%bind #:%aref #:%function #:%expr #:%setf #:%defsmem #:%defun)
  ;; Typed
  (:export #:TypedNode #:Typed #:typed-dtype #:typed-pointer-p)
  ;; Optimization Ops
  (:export
   ))
