(defpackage :caten/runtime
  (:use :cl :caten/runtime/buffer :caten/runtime/runtime)
  ;; Buffer
  (:export
   #:AbstractBuffer
   #:buffer-shape
   #:buffer-storage-size
   #:buffer-stride
   #:buffer-dtype
   #:buffer-views
   #:buffer-nrank
   #:buffer-value
   #:make-buffer
   #:copy-buffer
   #:buffer-p
   
   #:open-buffer
   #:close-buffer
   #:transfer-from-array
   #:transfer-into-array
   #:bref
   #:copy-buffer-value
   #:buffer-ref
   #:pprint-buffer)
  ;; Runtime
  (:export
   #:*supress-allocate-mode*
   #:GraphRuntime
   #:runtime-graph
   #:runtime-id2tensor
   #:runtime-fw-outputs
   #:runtime-bw-outputs
   #:runtime-pc
   #:runtime-variables
   #:runtime-params
   #:realize-graph
   #:make-runtime
   #:free-runtime
   #:runtime-gather-args
   #:runtime-setvar
   #:runtime-getvar
   #:runtime-step
   #:realize-node
   #:runtime-forward
   #:runtime-backward
   #:runtime-buffer-type
   #:runtime-renderer
   #:runtime-error
   #:runtime-invoke-jit-kernel)
  ;; Profile
  (:export
   #:*jit-time*
   #:*vm-time*
   #:*allocate-time*
   #:with-real-time
   #:render-runtime-position
   #:report-allocation
   #:start-profile
   #:report-profile-result))
