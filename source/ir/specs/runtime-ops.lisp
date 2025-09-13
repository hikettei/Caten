(in-package :caten/ir)

;; = [Summary of RuntimeOps] ==========================================
;; :SINK | Pauses the pc
;; :
;; +)__________________________________________________________________
;;                                                             | 26 Ops
(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass RuntimeOps () nil)

(defnode (:RuntimeOps :GLOBAL/ALLOCATE) (RuntimeOps)
	 "Allocates a new matrix of scalar value in the VM.
```
out = allocate_global(*shape, *stride)
```

The same as :Allocate in TensorGraph, but is produced by jit-compiling TensorGraph. Allocates *shape array on the global memory.

- dtype[dtype-t] dtype to allocate.
- nrank[(unsigned-byte 32)] a rank of tensor. If set to 0, allocates a scalar.
- from[symbol or buffer or null] If specified, instead of allocating, an already allocated Buffer is used. If a symbol is specified, a buffer is already defined in the variable table of GraphRuntime. If buffer is specified, use the buffer directly.
- pool[null or Buffer] A place to store the result of the previous allocation. Allocation will be performed only after this slot is set to nil, or size are different due to dynamic shape.
"
	 :slots ((nrank :type (unsigned-byte 32))
		 (dtype :type dtype-t)
		 (from :initform nil)
                 (pool :initform nil :type (or null Buffer)))
         :type-relay #'(lambda (id->type node)
                         nil))

(defnode (:RuntimeOps :GLOBAL/VIEW) (RuntimeOps)
         ""
         :slots nil)

(defnode (:RuntimeOps :ROOT) (RuntimeOps)
         "The root of RuntimeGraph"
         :placeholder -1
         :type-relay #'(lambda (id->type node) nil))

(defnode (:RuntimeOps :LAUNCH) (RuntimeOps) "Launch kernel")

;; [TODO] Support Control Flow
;; - [ ] Support RNN/LSTM/GRU
;; (defnode (:RuntimeOps :WHEN) (RuntimeOps))
;; (defnode (:RuntimeOps :LOOP) (RuntimeOps) "")
;; [TODO]
;; - [ ] MultiGPU Support
)
