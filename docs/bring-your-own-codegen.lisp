;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression
(ql:quickload :caten)
(defpackage :byoc-example
  (:use :cl))
(in-package :byoc-example)

;; draft:
;; Mainly consisted of the following components

;; 1. AbstractBuffer
;; 2. GraphRunner
;; 3. Renderer
;; 4. Auto-Scheduler


;; define-backend
