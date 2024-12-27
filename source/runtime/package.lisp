(defpackage :caten/runtime
  (:use :cl :caten/runtime/buffer)
  (:export
   #:AbstractBuffer
   #:buffer-shape
   #:buffer-stride
   #:buffer-dtype
   #:buffer-views
   #:buffer-nrank
   #:buffer-value
   #:make-buffer
   
   #:open-buffer
   #:close-buffer
   #:transfer-from-array
   #:transfer-into-array
   #:bref))
