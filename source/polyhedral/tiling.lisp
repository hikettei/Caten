(defpackage :caten/polyhedral/tiling
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/isl :caten/polyhedral/ir)
  (:export #:tile-bands)
  (:documentation "Provides an auto-tuner for tiling dims and params
- References
  - https://speakerdeck.com/ideininc/pldi-21lun-wen-du-mihui-akg-automatic-kernel-generation-for-neural-processing-units-using-polyhedral-transformations?slide=11
"))

(in-package :caten/polyhedral/tiling)

(defun tiling-sizes (band &key (size-default 32) (dims))
  (declare (type list dims) (type fixnum size-default))
  (let* ((band-space (schedule-node-band-get-space band))
         (dim (space-dim band-space 3)))
    (multi-val-from-val-list
     band-space
     (apply
      #'make-value-list
      (loop for i upfrom 0 below dim
            collect
            (or (nth i dims) size-default))))))

(defun shift-band-zero (band)
  "Refernece: https://github.com/hikettei/cl-polyhedral/blob/main/source/tiling.lisp#L52C1-L79C37"
  (let* ((domain (schedule-node-get-domain band))
         (partial-schedule (schedule-node-band-get-partial-schedule band))
         (mupa (multi-union-pw-aff-intersect-domain partial-schedule domain))
         (n (multi-union-pw-aff-size mupa))
         (multi-val (multi-union-pw-aff-min-multi-val mupa)))
    (loop for i upfrom 0 below n
          for v = (multi-val-get-val multi-val i)
          do (when (value-negative-infinity-p v)
               (setf multi-val (multi-val-set-val multi-val i (value 1)))))
    (let* ((shift (multi-union-pw-aff-multi-val-on-domain domain multi-val))
           (shift-neg (multi-union-pw-aff-neg shift))
           (partial-schedule (multi-union-pw-aff-add partial-schedule shift-neg)))
      (values partial-schedule shift))))

(defun tile-partial-schedule (partial-schedule tile-size &key (scale-tile-loops nil))
  (let ((n (multi-union-pw-aff-size partial-schedule)))
    (loop for i upfrom 0 below n
          for upa1 = (multi-union-pw-aff-get-union-pw-aff partial-schedule i)
          for v = (multi-val-get-val tile-size i)
          for upa2 = (union-pw-aff-scale-down-val upa1 v)
          for upa3 = (union-pw-aff-floor upa2)
          for upa = (if scale-tile-loops (union-pw-aff-scale-val upa3 v) upa3)
          do (setf partial-schedule (multi-union-pw-aff-set-union-pw-aff partial-schedule i upa)))
    partial-schedule))

(defun schedule-tile-band (band &key (size-default 32) (dims))
  (multiple-value-bind (partial-schedule shift)
      (shift-band-zero band)
    (let* ((tiling-sizes (tiling-sizes band :size-default size-default :dims dims))
           (partial-schedule (tile-partial-schedule partial-schedule tiling-sizes))
           (tiled-sched (multi-union-pw-aff-add partial-schedule shift)))
      (schedule-node-get-schedule
       (schedule-node-insert-partial-schedule band tiled-sched)))))

(defun get-tileable-bands (schedule)
  (declare (type schedule schedule))
  (let ((node (schedule-get-root schedule))
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
    tileable-bands))

(defun tile-bands (ir)
  "`tile-bands` helps you execute the computation tile by tile over the two axes"
  (declare (type Polyhedral-IR ir))
  (let* ((schedule (poly-schedule ir))
         (bands (get-tileable-bands schedule)))
    (dotimes (i (length bands))
      (setf (poly-schedule ir) (schedule-tile-band (nth i (get-tileable-bands (poly-schedule ir))))))))
