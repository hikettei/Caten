(defpackage :caten/runtime/runtime
  (:documentation "
This package provides GraphRuntime, which is a class to run an air graph.
")
  (:use :cl :caten/graph :caten/ir :caten/runtime/buffer)
  (:import-from :alexandria :compose)
  (:export
   #:backend-get-runtime-cls
   #:make-runtime
   #:open-runtime
   #:close-runtime))

(in-package :caten/runtime/runtime)

(defgeneric backend-get-runtime-cls (backend-id))
(defgeneric open-runtime (runtime))
(defgeneric close-runtime (runtime))

(defun make-runtime ()
  (open-runtime (->fast-graph (make-graph) :cls (backend-get-runtime-cls (ctx:getenv :BACKEND)))))
