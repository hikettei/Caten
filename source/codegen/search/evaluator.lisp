(defpackage :caten/codegen/search/evaluator
  (:documentation "Provides an Evaluator which sorts multiple PolyhedralScheduleItem")
  (:use :cl :caten/codegen/search/polyhedral)
  (:export
   #:Evaluator
   #:DeviceMeasurer

   #:evaluate-polyhedral
   ))

(in-package :caten/codegen/search/evaluator)

(defclass Evaluator ()
  nil)

(defclass Random-Forest () nil)

(defclass Proximity () nil)

;; [TODO] というかこれは丸ごとCommonにしてDISKへ保存する。
(defclass DeviceMeasurer (Evaluator)
  ((runtime :initarg :runtime)
   (cache :initform (make-hash-table :test 'equal))))

(defgeneric evaluate-polyhedral (psi evaluator blueprint))

(defmethod evaluate-polyhedral ((psi Polyhedral-Schedule-Item) (evaluator Evaluator) blueprint)
  "
MeasurerWorkflow
```
   [Input: Polyhedral, Blueprint]
              |
   ForEach [Kernel] in codegen(polyhedral, blueprint):
              |
   <IF: EntryIsInCache> ---> [Return previous result]
              |
          [Profile]
              |
         [StoreInCache]
```
"
  ;; TODO
  ;; - [ ] Finish ASTGen
  ;; - [ ] BEAM Cache Systemを構築する
  ;; - [ ] Replayerは削除する
  ;; - [ ] 入力データについてどうしよう。ZeroDivisionが起こるかもしれない
  ;; - [ ] Where, 条件分岐を含む実装についてはもっと難しい。PayneHanekなど
  )

(defmethod evaluate-polyhedral ((psi Polyhedral-Schedule-Item) (evaluator Proximity) blueprint)
  "Compute Proximity Evaluation"
  )
;; [TODO]
;; - 前回とのDiffを計測して，差分が0ならSKIP
;; - RandomForest, Compile+Runをサポート
;;   - SymbolicTileをサポートする
;; - 複数のカーネルを生成するときは，カーネルごとに分割してCacheできるように
;; - regression tree!
