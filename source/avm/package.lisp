(defpackage :caten/avm
  (:use :cl :caten/aasm :caten/air :alexandria)
  (:local-nicknames (:docs :caten/common.documentation))
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast)
  ;; from profile.lisp
  (:export
   :with-real-time
   :render-avm-position)
  ;; from buffer.lisp
  (:export
   #:*device*
   #:*max-display-len*
   #:*max-display-matrix*
   #:with-device
   #:Buffer #:make-buffer #:copy-buffer #:buffer-p #:buffer-nrank #:buffer-value #:buffer-dtype #:buffer-shape #:buffer-stride #:buffer-views #:buffer-depend-idx-list
   #:buffer-orig-buffer-shape
   #:buffer-inferred-permute
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
   #:avm-id2tensor #:avm-tape-length #:avm-pc #:avm-variables #:avm-params-to-optimize
   #:*vm*
   #:%realize
   #:vm/readvar
   #:vm/setvar
   #:vm/step
   #:vm/forward
   #:vm/backward
   #:vm/set-params
   #:avm-reset)
  ;; from conditions.lisp
  (:export
   #:avm-runtime-error)
  ;; from lisp-backend.lisp
  (:export
   #:map-view))
