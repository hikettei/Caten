(cl:in-package :cl-user)
(defpackage :caten/avm
  (:use :cl :caten/aasm :caten/air :alexandria)
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from buffer.lisp
  (:export
   #:*device*
   #:*max-display-len*
   #:with-device
   #:Buffer #:make-buffer #:copy-buffer #:buffer-p #:buffer-nrank #:buffer-value #:buffer-dtype #:buffer-shape #:buffer-stride #:buffer-views
   #:%vm/allocate-buffer
   #:%vm/read-index
   #:realize-buffer
   #:pprint-buffer)
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
   #:AVM
   #:make-avm #:avm-graph #:avm-name #:avm-fw-outputs #:avm-bw-outputs #:copy-avm #:deepcopy-avm
   #:avm-id2tensor #:avm-tape-length #:avm-pc #:avm-variables
   #:*vm*
   #:%realize
   #:vm/readvar
   #:vm/setvar
   #:vm/step
   #:vm/forward
   #:vm/backward
   #:vm/set-params)
  ;; from conditions.lisp
  (:export
   #:avm-runtime-error))
