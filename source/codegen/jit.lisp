(defpackage :caten/codegen/jit
  (:documentation "This is an entry point for JIT. JIT compiles unoptimized AVM into optimized AVM.")
  (:use :cl)
  (:import-from
   :caten/avm
   #:AVM)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules)
  (:export
   #:jit))

(in-package :caten/codegen/jit)

(defun jit (avm)
  (declare (type AVM avm))
  ;; 1. Running the type inference
  (run-type-infer avm)
  ;; 2. JIT Specific Simplifier
  (apply-rewriting-rules avm)
  
  )
