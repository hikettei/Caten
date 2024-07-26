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
   #:Tensor
   #:tensor-p
   #:tensor-shape
   #:tensor-dtype
   )
  ;; from shape-tracker.lisp
  (:export #:st)
  )
