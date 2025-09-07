(in-package :caten/api)

;; Tensor V2 Design Concepts:
;; - [ ] 計算グラフの表記すらIRへ統合する
;; - [ ] Support Autograd
;;  - [ ] Introduce (defnode :gradient option)
;;  - [ ] VIEW Autodiffを実装可能かどうか
;; - [ ] SINK
;; - [ ] caten/langで記述したコードをVIEWに書き戻せるかどうかを考える。
;;   - [ ] Reschedule(Separate sccs)
;;   - [ ] Polyhedral Modelの自動微分
