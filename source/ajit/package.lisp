(in-package :cl-user)
(defpackage :caten/ajit
  (:use :cl :alexandria :caten/aasm :caten/air :caten/avm :caten/isl :cffi)
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from helpers.lisp
  (:export
   #:apply-static-gensym)
  ;; from isl-objects.lisp
  (:export
   #:form
   )
  ;; from isl-ast-helpers.lisp
  (:export
   #:Expr
   #:Expr-x
   #:Expr-y
   )
  ;; from device.lisp
  (:export
   #:Device
   #:default-device)
  ;; from scheduler.lisp
  (:export
   #:create-polyhedral-model
   #:auto-schedule
   #:jit
   #:render-isl-aref
   #:JIT-Info
   #:jit-info-p
   #:jit-info-n-kernels
   #:copy-jit-info
   #:jit-info-code
   #:jit-info-lang
   #:jit-info-caller)
  ;; from type-relay.lisp
  (:export
   #:run-type-infer
   #:deploy-type-infer-results
   #:Inferred-Types
   #:read-type-relay
   #:relay-reads
   #:relay-writes)
  ;; from polyhedral.lisp
  (:export
   #:Polyhedral
   ;; keep other slots of Polyhedral private
   #:poly-avm
   #:poly-pipeline
   )
  ;; from memory-planner.lisp
  (:export
   #:node/in-place-mutation)
  ;; from renderer.lisp
  (:export
   #:%render-function-caller
   #:%render-compile
   #:%render-program-toplevel
   #:%render-function
   #:%render-body
   #:%render-expr
   #:%render-nodes
   #:render-expr
   #:Argument
   #:argument-name
   #:argument-p
   #:argument-pointer-p
   #:argument-dtype
   #:argument-type
   #:argument-io
   #:argument-metadata
  ))

(in-package :caten/ajit)
