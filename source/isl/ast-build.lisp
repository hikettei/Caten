(in-package :caten/isl)

(define-isl-object ast-build
  :free %isl-ast-build-free
  :copy %isl-ast-build-copy)

(define-isl-function create-ast-build %isl-ast-build-alloc
  (:give ast-build)
  (:parm context *context*))

(define-isl-function ast-build-from-context %isl-ast-build-from-context
  (:give ast-build)
  (:take set))

(define-isl-function ast-build-node-from-schedule %isl-ast-build-node-from-schedule
  (:give ast-node)
  (:keep ast-build)
  (:take schedule))
