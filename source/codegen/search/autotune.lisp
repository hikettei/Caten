(defpackage :caten/codegen/search/autotune
  (:use :cl :caten/air)
  (:export
   #:online-autotune-kernel
   ))

(in-package :caten/codegen/search/autotune)

(defun setup-autotune ()
  (values
   (ctx:getenv :BEAM)
   (+ (ctx:getenv :BEAM_THRESHOLD) 100.0)))

;; - GraphScheduleの時点で，なるべく超デカくグラフを持っておく(あくまでAutoTuneの計算量が膨大になるのを防ぐための)
;; - OnlineAutotuneKernelはめちゃめちゃでかい関数をFissionできる
;; - そのカーネル単位内で最小の実行時間を満たすカーネルをend2endで適用する = end2endでもOptimalとなる
(defun online-autotune-kernel (runtime node)
  (when (getattr node :optimized-p) (return-from online-autotune-kernel node))
  (multiple-value-bind (beam-width threshold) (setup-autotune)
    (caten/isl::with-isl-context
      
      )))
