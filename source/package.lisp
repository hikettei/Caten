(in-package :cl-user)
(defpackage :caten
  (:documentation "Frontends for ASM/VM/JIT etc, including:
- AbstractTensor Frontend
- Shape Tracker
- Merge View Solver
- ASM Bindings
- Graph Caller")
  (:use :cl :alexandria :trivia :cl-ppcre :caten/aasm :caten/air :caten/avm)
  ;; from tensor.lisp
  (:export
   #:make-tensor
   #:make-view-internal
   #:fconst #:uconst #:iconst
   
   #:Tensor
   #:tensor-p
   #:tensor-shape
   #:tensor-buffer
   #:tensor-dtype
   #:tensor-order
   #:tensor-id
   #:tensor-op
   #:tensor-views
   #:tensor-requires-grad
   #:tensor-grad
   #:tensor-variables
   )
  ;; from shape-tracker.lisp
  (:export #:st)

  ;; from module.lisp
  (:export
   #:Module
   #:impl
   #:defmodule
   #:module-outputs
   #:module-attrs
   #:module-sv4bws

   )

  ;; from function.lisp
  (:export
   ;; Binary
   #:!add #:!+
   #:!sub #:!-
   #:!mul #:!*
   #:!div #:!/

   ;; Unary
   #:!neg #:!recip
   ;; Logical
   #:!< #:!> #:!<= #:!>= #:!eq #:!neq
   ;; TernaryOps
   #:!where
   ))
