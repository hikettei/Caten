(defpackage :caten/byoc/clang
  (:use :cl :caten/runtime/buffer :caten/common.dtype)
  (:import-from :caten/byoc/lisp #:LispBuffer))

(in-package :caten/byoc/clang)

(defclass ClangBuffer (LispBuffer) nil)
