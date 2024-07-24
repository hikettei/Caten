(cl:in-package :cl-user)
(defpackage :caten/avm
  (:use :cl :caten/aasm :caten/air :alexandria)
  ;; from buffer.lisp
  (:export
   #:*device*
   #:with-device
   #:Buffer #:copy-buffer #:buffer-p #:buffer-nrank #:buffer-value #:buffer-dtype #:buffer-shape #:buffer-stride #:buffer-views
   #:%vm/allocate-buffer
   #:realize-buffer)
  ;; from helpers.lisp
  (:export
   #:parse-allocate-node
   #:parse-view-node)
  ;; from opset.lisp
  (:export
   #:%impl
   )
  ;; from runtime.lisp
  (:export
   #:%realize)
  )
