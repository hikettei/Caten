(in-package :caten/aasm)

;; = [Summary of RuntimeOps] ==========================================
;; :SINK | Pauses the pc
;; :
;; +)__________________________________________________________________
;;                                                             | 26 Ops
(eval-when (:compile-toplevel :load-toplevel :execute)

(defclass RuntimeOps () nil)

;; [TODO] Remove ./api/attrs.lisp after switching to use :SINK
(defnode (:RUNTIME :RETURN) (RuntimeOps)
	 "During VM execution, forward computation is paused at the point where this node exists.
```
ID <- RETURN(OUT_ID, K1, K2, K3, ...,mode=:forward or :backward)
```
"
	 :placeholder -1
         ;; :slots nil (TODO: Add :forward/:backward)
         :type-relay #'(lambda (id->type node) (list (gethash (car (node-reads node)) id->type))))

(defnode (:RUNTIME :SYNCHRONIZE) (RuntimeOps)
         "Runs ARG1, ARG2, ARG3 after KERNEL_ID1, KERNEL_ID2, ... was executed.
KERNEL_ID <- SYNCHRONIZE(KERNEL_ID1, KERNEL_ID2, ..., ARG1, ARG2, ...) 
"
         :slots ((n-kernel-args :type fixnum)))

;; [TODO] Remove ./codegen/jit.lisp :JIT_KERNEL after replacing them
(defnode (:RUNTIME :KERNEL) (RuntimeOps)
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM.
```
KERNEL_ID <- KERNEL(KERNEL_ID1, KERNEL_ID2, ..., ARG1, ARG2, ...)
```
"
	 :slots ((n-kernel-args :type fixnum) (kernel-info) (dtypes :type list) (cached-p :type boolean) (optimized-p :type boolean :initform nil)))

;; [TODO] Caten Multi GPU IR
;; %shared
;; %gather

)
