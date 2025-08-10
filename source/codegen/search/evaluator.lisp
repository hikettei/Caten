(defpackage :caten/codegen/search/evaluator
  (:documentation "Provides an Evaluator which sorts multiple PolyhedralScheduleItem")
  (:use :cl :caten/codegen/search/polyhedral)
  (:export
   #:Evaluator
   ))

(in-package :caten/codegen/search/evaluator)

(defclass Evaluator ()
  nil)
;; [TODO] というかこれは丸ごとCommonにする。
(defclass Device-Measurer (Evaluator)
  ((runtime :initarg :runtime)
   (cache :initform (make-hash-table :test 'equal))))

(defgeneric evaluate-polyhedral (psi evaluator blueprint))

(defmethod evaluate-polyhedral ((psi Polyhedral-Schedule-Item) (evaluator Evaluator) blueprint)
  ;; Modify psi-evaluation
  ;; これは，Splitしたカーネルごとに文字列 vs 実行時間のHashTableを作るようにする
  )
;; [TODO]
;; - 前回とのDiffを計測して，差分が0ならSKIP
;; - RandomForest, Compile+Runをサポート
;;   - SymbolicTileをサポートする
;; - 複数のカーネルを生成するときは，カーネルごとに分割してCacheできるように
;; - regression tree!
