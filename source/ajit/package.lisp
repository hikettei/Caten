(in-package :cl-user)
(defpackage :caten/ajit
  (:use :cl :alexandria :caten/aasm :caten/air :caten/avm :caten/isl :cffi)
  (:local-nicknames (:docs :caten/common.documentation))
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from helpers.lisp
  (:export
   #:apply-static-gensym
   #:unroll-suffix)
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
   #:default-device
   #:device-parallel-depth
   #:device-packed-by)
  ;; from scheduler.lisp
  (:export
   #:create-polyhedral-model
   #:auto-schedule
   #:jit
   #:render-isl-aref
   #:isl-access-expr
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
   #:relay-writes
   #:make-inferred-type
   )
  ;; from polyhedral.lisp
  (:export
   #:Polyhedral
   ;; keep other slots of Polyhedral private
   #:poly-avm
   #:poly-pipeline
   )
  ;; from renderer.lisp
  (:export
   #:%render-function-caller
   #:%render-compile
   #:%render-program-toplevel
   #:%render-function
   #:%render-body
   #:%render-expr
   #:%render-nodes
   #:render-index-components
   #:render-expr
   #:render-aref
   #:Argument
   #:argument-name
   #:argument-p
   #:argument-pointer-p
   #:argument-dtype
   #:argument-type
   #:argument-io
   #:argument-metadata

   #:op/expr)
  ;; from render-graph.lisp
  (:export
   #:r/funcall-string
   #:r/funcall
   #:r/if
   #:r/else
   #:r/endif
   #:r/for
   #:r/endfor
   #:r/while
   #:r/endwhile)
  ;; Expr construction
  (:export
   #:make-expr))

(in-package :caten/ajit)
