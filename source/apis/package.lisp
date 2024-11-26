(in-package :cl-user)
(defpackage :caten/apis
  (:nicknames :caten)
  (:documentation "Frontends for ASM/VM/JIT etc, including:
- AbstractTensor Frontend
- Shape Tracker
- Merge View Solver
- ASM Bindings
- Graph Caller")
  (:use :cl :alexandria :trivia :cl-ppcre :caten/aasm :caten/air :caten/avm)
  (:local-nicknames (:docs :caten/common.documentation))
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp)
  ;; from aot-compilation.lisp
  (:export
   #:*aot-jit*
   #:*aot-vm*
   #:*invoke-aot-function*
   #:caten/defun[t]
   #:caten/defun[all]
   #:caten/defun[float]
   #:caten/defun[int]
   #:caten/defun[uint])
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
   #:grad
   #:shape
   #:ndim
   #:dtype-of
   #:order

   #:*external-simplifiers*
   #:proceed

   #:inf #:-inf #:nan
   #:float-type-of
   )
  ;; from model.lisp
  (:export #:defmodel #:call)
  ;; from conditions.lisp
  (:export
   #:caten-forward-error
   #:caten-backward-error)
  ;; from shape-tracker.lisp
  (:export #:st #:bc)

  ;; from module.lisp
  (:export
   #:Module
   #:impl
   #:defmodule
   #:module-outputs
   #:module-attrs
   #:module-sv4bws
   ;; State-Dict
   #:State-Dict
   #:Make-State-Dict
   #:State-Dict-Entry
   #:State-Dict-p
   #:Copy-State-Dict
   #:->state-dict
   #:get-state-dict
   #:load-state-dict)
  ;; from high-level-ops.lisp
  (:export
   ;; reductions
   #:SumNode
   #:!sum
   #:MeanNode
   #:!mean
   #:!matmul
   #:!softmax

   ;; composed mathematical functions
   #:!sinh #:!cosh #:!tanh
   #:!truncate #:!ceiling #:!floor

   ;; linalg
   #:!tril
   #:!triu

   ;; argmax/min
   #:!argmax #:!argmin

   ;; statical
   #:!variance #:!std
   ;; dimension manipulation
   #:!split
   #:!chunk
   #:!concatenate)
  ;; from helpers.lisp
  (:export
   #:with-no-grad
   #:with-attrs
   #:print-avm
   #:normalize-axis
   #:normalize-axes)
  ;; from iseq.lisp
  (:export
   #:%compile-toplevel
   #:caten
   #:forward
   #:backward
   #:proceed
   #:%run
   #:with-jit)

  ;; from function.lisp
  (:export
   #:Func
   #:func-variables
   #:lower
   #:!identity
   ;; shaping
   #:!view #:!view-from-base #:!reshape #:!repeat #:!contiguous #:!copy #:!permute #:!t #:!transpose
   #:!expand
   #:!uprank
   ;; Binary
   #:!add #:!+
   #:!sub #:!-
   #:!mul #:!*
   #:!div #:!/
   #:!idiv
   #:!move
   #:!maximum #:!minimum
   #:!max #:!min
   #:!gcd #:!lcm

   ;; Unary
   #:!neg #:!recip
   #:!cast #:!signum #:!abs
   #:!sin #:!cos #:!tan
   #:!exp #:!exp2
   #:!log #:!log2
   #:!sqrt
   ;; Logical
   #:!< #:!> #:!<= #:!>= #:!eq #:!neq
   ;; TernaryOps
   #:!where
   ;; more
   #:!const
   #:!stride
   #:!index-components
   #:!xor #:!or #:!and
   ;; utils
   #:!rsqrt #:!square
   )
  ;; from initializers.lisp
  (:export
   #:*inference-mode*
   #:with-inference-mode
   #:make-input
   #:make-param
   
   #:set-manual-seed
   #:with-manual-seed
   #:ax+b
   #:!full
   #:!rand
   #:!randn
   #:!normal
   #:!uniform
   #:!randint

   ;; static
   #:rand
   #:uniform
   #:randn
   #:normal
   #:randint
   #:linspace
   #:xavier-uniform
   #:xavier-gaussian
   ))
