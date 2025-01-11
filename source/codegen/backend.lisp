(defpackage :caten/codegen/backend
  (:use :cl)
  (:export
   #:define-backend
   #:get-backend-buffer
   #:get-backend-runtime
   #:get-backend-renderer
   #:get-backend-auto-scheduler
   #:get-backend-jit-p
   #:jit-mode-p
   #:get-buffer-type
   #:get-runtime-type))

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

(defun jit-mode-p (&key (backend (ctx:getenv :BACKEND)))
  "Returns T if the current device uses JIT compilation."
  (get-backend-jit-p backend))

(defun get-buffer-type (&key (backend (ctx:getenv :BACKEND)))
  "Returns the buffer type for the current device."
  (get-backend-buffer backend))

(defun get-runtime-type (&key (backend (ctx:getenv :BACKEND)))
  "Returns the runtime type for the current device."
  (get-backend-runtime backend))
