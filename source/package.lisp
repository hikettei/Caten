(in-package :cl-user)
(defpackage :caten
  (:documentation "Frontends for ASM/VM/JIT etc, including:
- AbstractTensor Frontend
- Shape Tracker
- Merge View Solver
- ASM Bindings
- Graph Caller")
  (:use :cl :caten/aasm :caten/air :caten/avm)
  ;; from tensor.lisp
  (:export
   #:make-tensor
   )
  )
