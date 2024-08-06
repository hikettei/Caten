(cl:in-package :cl-user)
(defpackage :caten/common.tqdm
  (:documentation "Simple replacement for tqdm (TODO: Copy and paste cl-tqdm)")
  (:use :cl))
(in-package :caten/common.tqdm)

;; TODO
;; 1. BatchNorm/LayerNorm/ReLU/Conv2D/Embeddingらへんをまず実装する，その上でJITのテストを作成
;; 2. cl-tqdm
;; 3.
