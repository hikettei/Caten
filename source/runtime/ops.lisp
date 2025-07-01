(defpackage :caten/runtime/ops
  (:documentation ":caten/runtime/ops defines the major components for constructing GraphRuntime.")
  (:use :cl :caten/air)
  (:export

   ))
(in-package :caten/runtime/ops)
;; [TODO]
;; - Remove defnode in jit.lisp (e.g.: JIT_KERNEL), move to here!
;; -
;; -

(defun %fcall ()

  )

;; [TODO] Caten V2 TensorParallel
(defun %shard ())
(defun %gather())


