;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; Caten/Lang Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-lang-example
  (:use :cl :caten/api :caten/lang))

(in-package :caten-lang-example)
