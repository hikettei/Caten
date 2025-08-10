(defpackage :caten/codegen/search/autotune
  (:use :cl :caten/air :caten/codegen/search/polyhedral :caten/codegen/byoc :caten/codegen/search/evaluator)
  (:export
   #:online-autotune-kernel
   ))

(in-package :caten/codegen/search/autotune)
;; ~~ ScheduleGenerationTree ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Schedule-Generation-Tree ()
  ((depth :initform 0 :accessor sgt-depth :initarg :depth)
   (items :initform nil :accessor sgt-items :initarg :items))
  (:documentation
"Schedule-Generation-Tree collects Polyhedral IR candidates that belong to the
same search generation (beam frontier). Conceptually, for a parent schedule θ₀,
its children {θ₁,θ₂,…} formed by one-step transformations constitute one level
(generation) of this tree. The class stores the current frontier to be scored,
pruned (Top-k), and expanded to produce the next generation."))

(defun sgt-add-evaluations (sgt evaluator blueprint)
  "Attach evaluation scores to the current generation."
  (declare (type Schedule-Generation-Tree sgt))
  (mapc
   #'(lambda (x) (setf (psi-evaluation x) (evaluate-polyhedral x evaluator blueprint)))
   (sgt-items sgt)))

(defun sgt-prune-topk (sgt topk)
  "Prune the current frontier by keeping the Top-k candidates under the active"
  (declare (type Schedule-Generation-Tree sgt) (type fixnum topk))
  (let ((best-items (sort (sgt-items sgt) #'< :key #'psi-evaluation)))
    (setf (sgt-items sgt) (subseq best-items 0 (min topk (length best-items))))))

(defun sgt-make-nextgen (sgt)
  "Expand the pruned frontier to form the next generation."
  (declare (type Schedule-Generation-Tree sgt))
  (make-instance 'Schedule-Generation-Tree :items (copy-list (sgt-items sgt)) :depth (1+ (sgt-depth sgt))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO]
;; - TILE Parameter Space?
;; - 
(defun setup-autotune (runtime)
  (values
   (ctx:getenv :BEAM)
   (+ (ctx:getenv :BEAM_THRESHOLD) 100.0)
   (make-instance 'DeviceMeasurer :runtime runtime)
   ;; evaluator2
   ))

;; - GraphScheduleの時点で，なるべく超デカくグラフを持っておく(あくまでAutoTuneの計算量が膨大になるのを防ぐための)
;; - OnlineAutotuneKernelはめちゃめちゃでかい関数をFissionできる
;; - そのカーネル単位内で最小の実行時間を満たすカーネルをend2endで適用する = end2endでもOptimalとなる
;; - Coalesceってどうやって実装するんだろう。
;;  - TODO: Two more search command
;;   - Coalesce | At which stage?
;;   - Skewing  | これはTemplateGen
(defun online-autotune-kernel (runtime node)
  "
BEAM Search Workflow:
                [Input Blueprint]
                        |
    [Template Generation with Early Pruning] Mapping Reschedule, Interchange, TileGPU/PARALLEL at this stage
                        |
                   [BEAM Search] Optimizing TILE/VECTORIZE/SPLITREDUCE
"
  (when (getattr node :optimized-p) (return-from online-autotune-kernel node))
  (multiple-value-bind (beam-width threshold cost1 cost2) (setup-autotune runtime)
    (caten/isl::with-isl-context
      ;; - BEAM Search With Early Pruning
      ;; - 最初にInterchange, Parallel, Rescheduleから50個くらいの空間を生成
      ;; - 古典的なPolyhedral Compilerとしてできないか，top@5ができればいい
      (let* ((blueprint (kernel-blueprint (getattr node :kernel-info)))
             (root (make-polyhedral-schedule-item blueprint))
             (gen0 (make-instance 'Schedule-Generation-Tree :items (list root))))
        (sgt-add-evaluations gen0 cost1 blueprint)
        (sgt-prune-topk gen0 3)
        (print (sgt-make-nextgen gen0))

        ))))

;; 次にやること
;; タイルアクセスを解析して、インターチェンジが有効な次元がどれか列挙する方法はないか考える
;; はじめにテンプレート生成(実行なし)
;; 次にタイルなど細かい最適化
;; - 最初にInterchange, Parallel, Rescheduleから50個くらいの空間を生成
;; - 古典的なPolyhedral Compilerとしてできないか，top@5ができればいい
