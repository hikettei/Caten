(defpackage :caten/codegen/polyhedral
  (:use :cl :caten/air :caten/aasm)
  (:export
   ))
(in-package :caten/codegen/polyhedral)

;; 実際の実行時間を計測する機構の作成が最優先に思えます

(defun extract-scop-from-blueprint (blueprint)
  (declare (type FastGraph blueprint))
  ;; あ ~ Indexingをどうするかの解釈...
  ;; -> 普通に1Dのままで良さそうに見える？
  ;; Reductionのaccess repをどう解釈するか，という話もある
  )
