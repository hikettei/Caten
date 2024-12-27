(defpackage :caten/byoc/clang
  (:use :cl :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime :caten/codegen/backend)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler)
  (:import-from :caten/byoc/lisp #:LispBuffer))

(in-package :caten/byoc/clang)

(defclass ClangBuffer (LispBuffer) nil)
(defclass ClangRuntime (GraphRuntime) nil)
(define-auto-scheduler (Clang-Auto-Scheduler (&key (n-global-loop (1- (ctx:getenv :OMP)))))
                       ;; Use outermost loop parallelism for maximize memory locality (better softmax/layernorm scheduling)
                       :n-global-loop n-global-loop ;; OMP=1 -> The outermost loop is GLOBAL, otherwise everything is a local loop
                       :tile-size 32) ;; [TODO] Autotune

(define-backend :clang ClangBuffer ClangRuntime Clang-Auto-Scheduler t)
