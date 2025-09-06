(in-package :caten/ir)
;; [TODO] Remove
;; [TODO] Make RuntimeGraph always exportable to any language
(defun $return (required-kernels)
  ;; [TODO]
  )

(defun $sync (read-kernels read-args &key (out (gensym "K")))
  (emit (make-node :RUNTIME :SYNCHRONIZE (list out) (map 'list #'node->id1 (append read-kernels read-args)) :n-kernel-args (length read-kernels))))

(defun $kernel (read-kernels read-args kernel-info &key (dtypes) (out (gensym "K")) (optimized-p))
  "
```
KERNEL_ID <- Kernel(KernelID1, Kernel2, ..., ARG1, ARG2, ...)
```
- kernel-info[AbstractKernel] a kernel to launch
"
  (declare (type list read-kernels read-args))
  (emit (make-node :RUNTIME :KERNEL (list out) (map 'list #'node->id1 (append read-kernels read-args)) :n-kernel-args (length read-kernels) :kernel-info kernel-info :dtypes dtypes :optimized-p optimized-p)))
