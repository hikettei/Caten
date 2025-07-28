;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; An FlashAttention Compilation Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-matmul
  (:use :cl :caten/api :caten/lang))
(in-package :caten-matmul)

(in-caten-toplevel)

(defstruct Config (N 512) (X) (Y))
(defparameter *config* (make-config))

(defmethod make-inputs-from-config ((config Config))
  (with-slots ((N N) (X X) (Y Y)) config
    (ctx:with-contextvar (:BEAM 0)
      (values
       (setf X (or X (proceed (!randn `(,N ,N)))))
       (setf Y (or Y (proceed (!randn `(,N ,N)))))))))
;; ~~ Settings ~~ ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun caten-matmul (config)
  (multiple-value-bind (x y) (make-inputs-from-config config)
    (ctx:with-contextvar (:BEAM 10)
      (caten (!matmul x y)))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun benchmark (&key (impls (list #'caten-matmul)) (n 10) &aux (results))
  (loop for impl in impls
        for kernel = (funcall impl *config*) do
          (forward kernel)
          (push (list impl (forward kernel) (caten/runtime/profile:with-real-time (dotimes (i n) (forward kernel)))) results))
  results)

(print (benchmark))
