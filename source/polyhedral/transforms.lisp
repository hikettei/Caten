(defpackage :caten/polyhedral/transforms
  (:use :cl :caten/polyhedral/ir :caten/polyhedral/config))

(in-package :caten/polyhedral/transforms)

(defun get-zeros-on-union-set (delta-uset)
  (declare (type isl::union-set delta-uset))
  (let* ((delta-set (isl:set-from-union-set delta-uset))
         (ma (isl:multi-aff-zero (isl:set-get-space delta-set))))
    (isl:union-set-from-set (isl:set-from-multi-aff ma))))

(defun check-legality-parallel (node dep)
  "
```
(check-legality-parallel node dep)
```
Returns T if the band node is legal to be parallelized with respect to the dep.
Reference: https://github.com/hikettei/tadashi/blob/main/src/legality.c#L91-L122"
  (declare (type isl::schedule-node node) (type isl::union-map dep))
  (when (isl:union-map-is-empty dep) (return-from check-legality-parallel t))
  (let* ((map (isl:schedule-node-band-get-partial-schedule-union-map node))
         (domain (isl:union-map-apply-range (isl:union-map-apply-domain dep map) map))
         (delta (isl:union-map-deltas domain))
         (_ (when (isl:union-set-is-empty delta) (return-from check-legality-parallel t)))
         (zeros (get-zeros-on-union-set delta))
         (cmp (isl:union-set-lex-lt-union-set delta zeros))
         (retval (isl:union-set-is-empty cmp))
         (cmp (isl:union-set-lex-gt-union-set delta zeros)))
    (declare (ignore _))
    (and retval (isl:union-set-is-empty cmp))))

;; not working?
(defun insert-parallel (node)
  (isl::%isl-schedule-node-insert-mark (isl::schedule-node-handle node) (isl::identifier-handle (isl::make-id-from-str "parallel"))))
;; [TODO] Integrate Packing/Unroll/Tiling/Parallel into commands.lisp
;; [TODO] Using WMMA
;; [TODO] Vectorized CLANG
(defun polyir-set-parallel (poly level)
  ""
  (declare (type Polyhedral-IR poly))
  (let ((deps (poly-dependencies poly)))
    (map-schedule-nodes
     #'(lambda (type band)
         (when (eql type :schedule-node-band)
           (print "+++++++++++++")
           (print band)
           (when (check-legality-parallel band deps)
             (print "COINCIDENT=1"))))
     poly)))

(defun get-mark-insertable-bands (schedule)
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
;; (defun insert-mark (band))
(defun packing (config ir)
  (declare (type polyhedral-ir ir))
  ;; [TODO]
  )
