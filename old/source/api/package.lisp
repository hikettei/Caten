(in-package :cl-user)
(defpackage :caten/api
  (:nicknames :caten)
  (:documentation "API frontend for ASM/VM/JIT etc, including:
- AbstractTensor Frontend
- Shape Tracker
- Merge View Solver
- ASM Bindings
- Graph Caller")
  (:use :cl :alexandria :trivia :cl-ppcre :caten/aasm :caten/air :caten/runtime)
  (:local-nicknames (:docs :caten/common.documentation))
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp)
  
  ;; from aot-compilation.lisp
  (:export
   #:caten/defun[t]
   #:caten/defun[all]
   #:caten/defun[float]
   #:caten/defun[int]
   #:caten/defun[uint])
  
  ;; from tensor.lisp
  (:export
   #:*global-runtime*
   #:get-global-runtime
   #:make-tensor
   #:make-view-internal
   #:tensor-graph
   #:tensor-lowered-graph
   #:fconst #:uconst #:iconst
   #:->iconst #:->fconst #:->uconst
   
   #:Tensor
   #:tensor-p
   #:tensor-shape
   #:tensor-buffer
   #:tensor-dtype
   #:tensor-order
   #:tensor-id
   #:tensor-op
   #:tensor-tr
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
   #:float-type-of)
  
  ;; from model.lisp
  (:export
   #:defmodel
   #:call
   #:defcall
   #:defsequence
   #:asnode)
  
  ;; from conditions.lisp
  (:export
   #:caten-forward-error
   #:caten-backward-error)
  
  ;; from shape-tracker.lisp
  (:export
   #:st
   #:bc)
  
  ;; from merge-views.lisp
  (:export
    #:Tracker
    #:tr-shape #:tr-base-shape #:tr-stride #:tr-order #:tr-broadcast #:tr-mask #:tr-permute #:tr-contiguous
    #:canonicalize-int
    #:un1d
    #:start-tracking
    #:tr-apply-permute #:tr-apply-reshape #:tr-apply-uprank #:tr-reshapeable-p #:tr-apply-slice #:tr-apply-broadcast)
  
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
   #:!concatenate

   #:!square #:!rsqrt #:!gid #:!normalize-axis)
  
  ;; from helpers.lisp
  (:export
   #:with-no-grad
   #:with-attrs
   #:normalize-axis
   #:normalize-axes)
  
  ;; from iseq.lisp
  (:export
   #:%compile-toplevel
   #:caten
   #:forward
   #:backward
   #:proceed
   #:%run)

  ;; from function.lisp
  (:export
   #:Func
   #:func-variables
   #:lower
   #:!identity
   ;; shaping
   #:!view #:!view-from-base #:!reshape #:!flatten #:!repeat #:!contiguous #:!copy #:!permute #:!t #:!transpose
   #:!expand
   #:!uprank
   ;; Binary
   #:!add #:!+
   #:!sub #:!-
   #:!mul #:!*
   #:!div #:!/
   #:!mod
   #:!idiv
   #:!move
   #:!assign
   #:!maximum #:!minimum
   #:!max #:!min
   #:!gcd #:!lcm

   ;; Unary
   #:!neg #:!recip
   #:!cast #:!signum #:!abs
   #:!sin #:!cos #:!tan
   #:!exp #:!exp2
   #:!log #:!log2
   #:!sqrt #:!expt
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
   #:!clip #:!erf
   )
  
  ;; from facets.lisp
  (:export
   #:change-facet
   #:with-facet
   #:with-facets)
  
  ;; from initializers.lisp
  (:export
   #:*inference-mode*
   #:with-inference-mode
   #:make-input
   #:make-param
   
   #:set-manual-seed
   #:with-manual-seed
   #:*rng-counter*
   #:get-rng-counter
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
