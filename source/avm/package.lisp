(cl:in-package :cl-user)
(defpackage :caten/avm
  (:use :cl :caten/aasm :caten/air)
  ;; from buffer.lisp
  (:export
   #:*device*
   #:with-device
   #:Buffer #:buffer-nrank #:buffer-value #:buffer-dtype #:buffer-shape #:buffer-stride #:buffer-views
   #:%vm/allocate-buffer
   #:realize-buffer)
  ;; from opset.lisp
  (:export
   #:%impl
   ))
