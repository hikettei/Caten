(defpackage :caten/codegen/backend
  (:use :cl)
  (:export
   #:define-backend
   #:get-backend-buffer
   #:get-backend-runtime
   #:get-backend-renderer
   #:get-backend-auto-scheduler
   #:get-backend-jit-p))

(in-package :caten/codegen/backend)

(defgeneric get-backend-buffer (backend))
(defgeneric get-backend-runtime (backend))
(defgeneric get-backend-auto-scheduler (backend))
(defgeneric get-backend-renderer (backend))
(defgeneric get-backend-jit-p (backend))

(defmacro define-backend (name buffer-class runtime-class renderer-class auto-scheduler-class is-jit-p)
  "
```
(define-backend name buffer-class runtime-class renderer auto-scheduler-class is-jit-p)
```
Registers a new backend.
"
  `(progn
     (defmethod get-backend-buffer ((backend (eql ,name))) ',buffer-class)
     (defmethod get-backend-runtime ((backend (eql ,name))) ',runtime-class)
     (defmethod get-backend-renderer ((backend (eql ,name))) ',renderer-class)
     (defmethod get-backend-auto-scheduler ((backend (eql ,name))) ',auto-scheduler-class)
     (defmethod get-backend-jit-p ((backend (eql ,name))) ,is-jit-p)))