(defpackage :caten/runtime/ops
  (:documentation ":caten/runtime/ops defines the major components for constructing GraphRuntime.")
  (:use :cl :caten/air)
  (:export

   ))
(in-package :caten/runtime/ops)
;; How about this? we move the definition and constructor of irs to:
;; ./source/ir
;; ./source/ir/tensor/
;; ./source/ir/runtime/
;; ./source/ir/codegen/

;; Our Goal is turn every node as a compilable (e.g.: creating any runtime from our IR)
(eval-when (:compile-toplevel :load-toplevel :execute)

  (defnode (:Runtime :KERNEL_CALL) ()
           "The node :KERNEL_CALL calls a compiled kernel."

           )

)

;; [TODO]
;; - Remove defnode in jit.lisp (e.g.: JIT_KERNEL), move to here!
;; - [Feat] TypeInferenceを統一的にしたい。
;; -

(defun %kernel_call ()
  )

;; [TO COME] Caten V2 TensorParallel
(defun %shard ()) ;; Breaks the tensor (todo: key axis)
(defun %gather ())


