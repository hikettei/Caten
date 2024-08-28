(in-package :caten/isl)

(define-isl-object ast-build
  :free %isl-ast-build-free
  :copy %isl-ast-build-copy)

(define-isl-function create-ast-build %isl-ast-build-alloc
  (:give ast-build)
  (:parm context *context*))
