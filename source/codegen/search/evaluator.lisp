(defpackage :caten/codegen/search/evaluator
  (:documentation "Provides an Evaluator which sorts multiple PolyhedralScheduleItem")
  (:use :cl :caten/codegen/search/polyhedral)
  (:export
   #:Evaluator
   ))

(in-package :caten/codegen/search/evaluator)

(defclass Evaluator ()
  nil)

(defmethod evaluate-polyhedral ((psi Polyhedral-Schedule-Item) (evaluator Evaluator) blueprint)
  ;; Modify psi-evaluation
  )
;; [TODO]
;; - 前回とのDiffを計測して，差分が0ならSKIP
;; - RandomForest, Compile+Runをサポート
;;   - SymbolicTileをサポートする
;; - 複数のカーネルを生成するときは，カーネルごとに分割してCacheできるように
;; - regression tree!
