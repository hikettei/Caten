(defpackage :caten/codegen/vectorize
  (:use :cl))

(in-package :caten/codegen/vectorize)

(defgeneric %mutate-vectorize (op))
