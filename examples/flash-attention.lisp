;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; An FlashAttention Compilation Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-flash-attention
  (:use :cl :caten/api :caten/lang))

(in-package :caten-flash-attention)

;; Allow @caten.jit reader macro
(in-caten-toplevel)
;; [TODO]
;; - [ ] !randを修正する ...
;; - [ ] FlashAttentionを動作させる
;; - [ ] Benchmark Place
(progn ;; Tips: EmacsでC-c C-cできるようにprognで@caten.jit () { }を囲っておく
  @caten.jit () {
  (defun flash-attention ((Pointer Q Type (Batch Head N D)) (Pointer K Type (Batch Head N D)) (Pointer V Type (Batch Head N D))
                          (Pointer O Type (Batch Head N D))
                          (Pointer L Type (Batch Head N)) (Pointer M Type (Batch Head N)))
    (let ((scale (/ 1.0 (sqrt (scast D :float32))))
          (outer (* batch n head)))
      (for b = (Range BATCH 1) do
           (for h = (Range Head 1) do
                (for i = (Range N 1) do
                     (let ((q-base-idx (* D (+ i (* N (+ (* b head) h)))))
                           (k-base-idx (* D (* N (+ (* b head) h))))
                           (v-base-idx (* D (* N (+ (* b head) h))))
                           (o-base-idx (* D (+ i (* N (+ (* b head) h))))))
                       (with-locals ((row-m (aref M (+ i (* N (+ (* b head) h)))))
                                     (row-l (aref L (+ i (* N (+ (* b head) h))))))
                         (for j = (Range N 1) do
                              (with-locals ((dot 0.0))
                                (for dth = (Range D 1) do
                                     (setf dot (+= dot (* (aref Q (+ q-base-idx dth)) (aref K (+ k-base-idx dth))))))
                                (let ((S (* dot scale))
                                      (new-max (max row-m S))
                                      (exp-prev (exp (- row-m new-max)))
                                      (exp-cur (exp (- S new-max)))
                                      (l-new (+ (* exp-prev row-l) exp-cur)))
                                  (for dth1 = (Range D 1) do
                                       (setf (aref O (+ o-base-idx dth1))
                                             (/ (+ (* exp-cur (aref V (+ v-base-idx dth1))) (* exp-prev row-l (aref O (+ o-base-idx dth1)))) l-new)))
                                  (setf row-m new-max
                                        row-l l-new))))
                         (setf
                          (aref M (+ i (* n (+ (* b head) h)))) row-m
                          (aref L (+ i (* n (+ (* b head) h)))) row-l))))))))})

(defun test-flash-attention (&key (batch 10) (head 8) (n 128) (d 512))
  (multiple-value-bind (q k v o l m)
      (flash-attention
       (caten/api:make-tensor (list batch head n d))
       (caten/api:make-tensor (list batch head n d))
       (caten/api:make-tensor (list batch head n d))
       (caten/api:make-tensor (list batch head n d))
       (caten/api:make-tensor (list batch head n))
       (caten/api:make-tensor (list batch head n)))
    o))
