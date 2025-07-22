;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;; Caten/Lang Example
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :caten-lang-example
  (:use :cl :caten/api :caten/lang))

(in-package :caten-lang-example)
;; [TODO] Caten/Langはまだ開発中の機能です。Productionには以下を追加したい。
;; - [ ] きちんとした構文エラー検知，Specs, Docsの作成
;; - [ ] LanguageはHackable
;; - [ ] Arefの自動計算
;; - [ ] C言語，Pythonなど外部の言語からのLowering
;; - [ ] n-profileなど設定機能の追加 (BEAM)
;; - [ ] Provide a decent test.
(in-caten-toplevel)

(progn
  @caten.jit () {
  (defun sumreduce ((Pointer X Type (A B)))
    (with-locals ((acc 0.0))
      (for idx = (Range (* A B) 1) do
           (setf acc (+= acc (aref X idx))))
      (setf (aref X 0) acc)))})

(progn
  @caten.jit () {
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
