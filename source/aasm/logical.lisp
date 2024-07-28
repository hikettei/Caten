(in-package :caten/aasm)

;; [TODO] %where ... fuse the index access when doing lowerer
(defun %where (condition x y &key (id (gensym "WID")))
  "id = where(condition, x{true-then}, y{false-then})"
  (declare (type node condition x y))
  (emit (make-node :TernaryOps :WHERE (list id) (list (node->id condition) (node->id x) (node->id y)))))

;; [TODO] simplify-logical-ops
;; e.g.: (not (not x)) -> x
;; WIP: arefに対する論理式を合成できるようにする + whereを合成
;; a[f(i)]が並列化できるか確かめるように書きたい
;;(defsimplifier
;;    (simplify-logical :speed 3)
;;    ((:Not ((:Not (x)))) -> ((node graph) (id->value graph x))))
