;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; The usage of GPT2
(ql:quickload '(:caten :caten/apps))
(defpackage :gpt2-inference
  (:use :cl :caten/apps.gpt2))
(in-package :gpt2-inference)

(setf (ctx:getenv :JIT) 1 (ctx:getenv :JIT_DEBUG) 4)

(defparameter *gpt2* (make-gpt2 :gpt2)) ;; we also have :gpt2-medium and :gpt2-large, :gpt2-xl

(print (gpt2-generate *gpt2* "Say hello to me!"))
