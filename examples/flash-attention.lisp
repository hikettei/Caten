;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; An FlashAttention Compilation Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-flash-attention
  (:use :cl :caten/api :caten/lang))

(in-package :caten-flash-attention)

(in-caten-toplevel)
;; TODO
;; - 1. Compare the outputs
;; - 2. まずLispで動かす
(progn
  @caten.jit () {
  (defun flash_attention ((Pointer Q Type (Batch Head N D)) (Pointer K Type (Batch Head N D)) (Pointer V Type (Batch Head N D))
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
                       (with-locals ((row_m (aref M (+ i (* N (+ (* b head) h)))))
                                     (row_l (aref L (+ i (* N (+ (* b head) h))))))
                         (for j = (Range N 1) do
                              (with-locals ((dot 0.0))
                                (for dth = (Range D 1) do
                                     (setf dot (+= dot (* (aref Q (+ q-base-idx dth)) (aref K (+ k-base-idx (* D j) dth))))))
                                (let ((S (* dot scale))
                                      (new-max (max row_m S))
                                      (exp-prev (exp (- row_m new-max)))
                                      (exp-cur (exp (- S new-max)))
                                      (l-new (+ (* exp-prev row_l) exp-cur)))
                                  (for dth1 = (Range D 1) do
                                       (setf (aref O (+ o-base-idx dth1))
                                             (/ (+ (* exp-cur (aref V (+ v-base-idx (* j D) dth1))) (* exp-prev row_l (aref O (+ o-base-idx dth1)))) l-new)))
                                  (setf row_m new-max
                                        row_l l-new))))
                         (setf
                          (aref M (+ i (* n (+ (* b head) h)))) row_m
                          (aref L (+ i (* n (+ (* b head) h)))) row_l))))))))})

(defstruct Config (batch 30) (head 4) (n 3) (d 16))
(defmethod make-inputs-from-config ((config Config))
  (with-slots ((batch batch) (head head) (n n) (d d)) config
    (ctx:with-contextvar (:BEAM 0)
      (values
       (proceed (!rand `(,batch ,head ,n ,d))) ;; Q
       (proceed (!rand `(,batch ,head ,n ,d))) ;; K
       (proceed (!rand `(,batch ,head ,n ,d))) ;; V
       (make-tensor `(,batch ,head ,n ,d))
       (make-tensor (list batch head n))
       (make-tensor (list batch head n))))))

(defun test-flash-attention (config)
  (multiple-value-bind (q k v o l m) (make-inputs-from-config config)
    (multiple-value-bind (q k v o l m) (flash_attention q k v o l m)
      (caten o))))

;; (test-flash-attention)
