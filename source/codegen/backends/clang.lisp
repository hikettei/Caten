(defpackage :caten/codegen/backends/clang
  (:use :cl :caten/air :cffi :caten/codegen/renderer :caten/codegen/helpers
   :caten/codegen/shape-inference :caten/runtime :caten/codegen/expr)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler))

(in-package :caten/codegen/backends/clang)
