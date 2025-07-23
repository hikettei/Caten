;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; Caten/Lang Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-auto-scheduler-example
  (:use :cl :caten/api :caten/lang))

(in-package :caten-auto-scheduler-example)

(setf (ctx:getenv :BACKEND) "NATIVE" ;; Caten/Lang requires JIT enabled backend
      (ctx:getenv :BEAM) 10
      (ctx:getenv :JIT_DEBUG) 3) ;; Recommended to know what happening w/ BEAM>=1
      
(in-caten-toplevel)

(progn
  @caten.jit () {
  (defun Gemm ((Pointer Z Type (M K)) (Pointer X Type (M N)) (Pointer Y Type (N K)))
    (for i = (Range M 1) do
         (for j = (Range K 1) do
              (with-locals ((acc 0.0))
                (for kk = (Range N 1) do
                     (setf acc (+ acc (* (aref X (+ (* N i) kk)) (aref Y (+ (* K kk) j))))))
                (setf (aref Z (+ (* K i) j)) acc)))))})

(defun !matmul-jit (a b)
  (let ((out (st "A[i j] B[j k] -> A[i k]" (a b))))
    (Gemm out a b)))

(caten (!matmul-jit (make-tensor `(128 128)) (make-tensor `(128 128))))

(defun benchmark (&key (upfrom 2) (below 1024) (step 2) (results))
  (loop with count = upfrom
        while (<= count below) do
          (let ((searched (caten (!matmul-jit (make-tensor `(,count ,count)) (make-tensor `(,count ,count))))))
            (push (cons count (caten/runtime/profile:with-real-time (forward searched))) results))
          (setf count (* step count)))
  results)

(print (benchmark))

;; SearchSpace
;; - [ ] Tile
;; - [ ] ParallelND
;; - [ ] ...
;; [TODO] Compute GFlops, compare the result w/ OpenBLAS
