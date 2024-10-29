(defpackage :caten/polyhedral/tiling
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl :caten/polyhedral/ir)
  (:export #:solve)
  (:documentation "Provides an auto-tuner for tiling dims and params
- References
  - [AKG]()
  - https://speakerdeck.com/ideininc/pldi-21lun-wen-du-mihui-akg-automatic-kernel-generation-for-neural-processing-units-using-polyhedral-transformations?slide=11
"))

(in-package :caten/polyhedral/tiling)

;; https://github.com/mindspore-ai/akg/blob/master/src/poly/tiling/tiling_solver.cc#L1238
;; https://github.com/mindspore-ai/akg/blob/master/src/poly/tiling/tiling_analyzer.cc#L1532

;; Goal: https://github.com/ggerganov/llama.cpp/blob/master/ggml/src/ggml-metal.metal
(defun tile-bands (schedule)
  (declare (type schedule schedule))
  (let ((tile-schedule schedule)
        (node (schedule-get-root schedule))
        (next-nodes)
        (tileable-bands))
    ;; Enumerate all tilable bands
    (loop named tiling-search
          for n-children = (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node))
          while (> n-children 0) do
            ;; Reached to the maximum tile band size => Stop
            (loop named search-for-children
                  for nth upfrom 0 below n-children
                  for band = (schedule-node-get-child node nth)
                  for type = (schedule-node-get-type band)
                  if (eql type :schedule-node-band)
                    do (push band tileable-bands)
                       (push (schedule-node-get-child band 0) next-nodes)
                       (return-from search-for-children)
                  else
                    do (push band next-nodes))
            (when (= (length next-nodes) 0)
              (return-from tiling-search))
            (setf node (pop next-nodes)))
    ;; Only tile the reduction node
    (print tileable-bands)
    tile-schedule))
;; TODO: Create Shard Memory (softmax) loop fussion -> SINK
;; TODO: 
(defun solve (ir)
  "An entry point for the tiling optimizer"
  (declare (type Polyhedral-IR ir))
  (let ((schedule (poly-schedule ir)))
    (tile-bands schedule))
  ;; -> Proceed to unrolling
  ;; -> coincident=parallelize -> Global Loop
  nil)
