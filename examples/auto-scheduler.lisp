;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; Caten/Lang Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-auto-scheduler-example
  (:use :cl :caten/api :caten/lang))

(in-package :caten-auto-scheduler-example)

(in-caten-toplevel)

(progn
  @caten.jit () { ;; TODO: n-profile
  (defun Gemm ((Pointer Z Type (M K)) (Pointer X Type (M N)) (Pointer Y Type (N K)))
    ;; [Note] Why nothing is scheduled?
    (for i = (Range 0 M) do
         (for j = (Range 0 K) do
              (with-locals ((acc 0.0))
                (for k = (Range 0 N) do
                     (setf acc (+ acc (* (aref X (+ (* N i) k)) (aref Y (+ (* K k) j))))))
                (setf (aref Z (+ (* K i) j)) acc)))))})

(defun !matmul-jit (a b)
  (let ((out (st "A[i j] B[j k] -> A[i k]" (a b))))
    (Gemm out a b)))

;; TODO:

;; SearchSpace
;; - [ ] Tile
;; - [ ] ParallelND
;; - [ ] ...

;; Compute GFlops, compare the result w/ OpenBLAS
