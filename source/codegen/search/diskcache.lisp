(defpackage :caten/codegen/diskcache
  (:use :cl :caten/air)
  (:export

   ))

(in-package :caten/codegen/diskcache)
;; [Workload]
;; - [ ] 1. $Affine ==> FusionKeyのMappingを実装
;; - [ ] 2. ILP Basedで計算量が大きいFusionを実施するか，Valueを (Item Relocate Basedで実装)
;; - [ ] 3. DB Backend or HashTable Backend
;; - [ ] dashboard.lispで累積したカーネルの情報を簡易的に可視化する
;; - [ ] dashboard.lispに同型のグラフの情報を蓄積させる ==> ２回目以降Cache
;; dashboard.lispで累積したグラフを可視化させる
;; 検索エントリは$Affine
;; DB
;; - Entry: 入力ドメインのBytes, データ型, 
;; - ScheduleNode YAML
;; - UnionMap YAML
;; -

(defun db-connection ())
(defun diskcache-get ())
(defun diskcache-set ())

(defun create-affine-entry (node)
  (declare (type Node node))
  (assert (eql :Affine (node-type node)))
  
  )

(defun lookup-entry-by-id (id)
  (declare (type string id))
  
  )
