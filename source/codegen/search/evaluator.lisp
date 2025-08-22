(defpackage :caten/codegen/search/evaluator
  (:documentation "Provides an Evaluator which sorts multiple PolyhedralScheduleItem")
  (:use :cl :caten/codegen/search/polyhedral :caten/codegen/search/ast)
  (:export
   #:Evaluator
   #:DeviceMeasurer
   #:evaluate-polyhedral))

(in-package :caten/codegen/search/evaluator)

(defclass Evaluator () nil)

(defclass Random-Forest () nil)

(defclass Proximity () nil)

;; [TODO] というかこれは丸ごとCommonにしてDISKへ保存する。
;; GlobalParamにDeviceMeasurerを配置する
(defclass DeviceMeasurer (Evaluator)
  ((runtime :initarg :runtime)
   (blueprint :initarg :blueprint)
   (ctx :initform nil :accessor dm-ctx)
   (version :initform nil :initarg :version)
   (cache :initform (make-hash-table :test 'equal) :accessor dm-cache)))

(defgeneric evaluate-polyhedral (psi evaluator blueprint))

(defmethod evaluate-polyhedral ((psi Polyhedral-Schedule-Item) (evaluator DeviceMeasurer) blueprint)
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
  (multiple-value-bind (kernels allocs) (apply-schedule (psi-theta psi) blueprint :ctx (dm-ctx evaluator))
    ;; TODO
    ;; - [x] Finish ASTGen
    ;; - [x] BEAM Cache Systemを構築する
    ;; - [ ] Replayerは削除する
    ;; - [ ] 入力データについてどうしよう。ZeroDivisionが起こるかもしれない
    ;; - [ ] Where, 条件分岐を含む実装についてはもっと難しい。PayneHanekなど
    (loop for kernel in kernels
          for kernel-id = (caten/codegen/renderer:make-kernel-description kernel :version (slot-value evaluator 'version) :getraw nil)
          for cache = (gethash kernel-id (dm-cache evaluator))
          if cache sum cache
            ;; [TODO] True Evaluation!, version --> n_evaluation, etc
            else sum (setf (gethash kernel-id (dm-cache evaluator)) (random 1.0)))))

(defmethod evaluate-polyhedral ((psi Polyhedral-Schedule-Item) (evaluator Proximity) blueprint)
  "Compute Proximity Evaluation"
  
  )
