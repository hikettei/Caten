(defpackage :caten/codegen/search/evaluator
  (:use :cl)
  (:export

   ))

(in-package :caten/codegen/search/evaluator)

;; [TODO]
;; - 前回とのDiffを計測して，差分が0ならSKIP
;; - RandomForest, Compile+Runをサポート
;;   - SymbolicTileをサポートする
;; - 複数のカーネルを生成するときは，カーネルごとに分割してCacheできるように
