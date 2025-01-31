(in-package :cl-user)

(defpackage :caten/codegen
  (:use :cl)
  (:import-from :caten/codegen/jit :jit)
  (:export :jit))
