(in-package :caten/aasm)

;; = [Summary of RuntimeOps] ==========================================
;; :SINK | Pauses the pc
;; :
;; +)__________________________________________________________________
;;                                                             | 26 Ops
(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass RuntimeOps () nil)

;; [TODO] Remove ./api/attrs.lisp after switching to use :SINK
(defnode (:SPECIAL/VM :SINK) (JITAble RuntimeOps)
	 "During VM execution, forward computation is paused at the point where this node exists."
	 :placeholder -1
         ;; :slots nil (TODO: Add :forward/:backward)
         :type-relay #'(lambda (id->type node) (list (gethash (car (node-reads node)) id->type))))

;; [TODO] Remove ./codegen/jit.lisp :JIT_KERNEL after replacing them
(defnode (:RUNTIME :KERNEL) (RuntimeOps)
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM.
```
KERNEL_ID <- KERNEL(KERNEL_ID1, KERNEL_ID2, ..., ARG1, ARG2, ...)
```
"
	 :slots ((output-buffer-n :type fixnum) (kernel-info :type Compiled-Kernel) (dtypes :type list) (cached-p :type boolean)))

;; [TODO] Caten Multi GPU IR
;; %shared
;; %gather

)
