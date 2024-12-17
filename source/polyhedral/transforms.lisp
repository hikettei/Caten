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

(defun insert-parallel (band)
  (isl:schedule-node-insert-mark band (isl::make-id-from-str "parallel")))
;; [TODO] Integrate Packing/Unroll/Tiling/Parallel into commands.lisp
;; [TODO] Using WMMA
;; [TODO] Vectorized CLANG
(defun get-coincident-points (poly)
  (map-schedule-nodes
   #'(lambda (type band)
       (when (eql type :schedule-node-band)
         (when (check-legality-parallel band (poly-dependencies poly)) band)))
   poly))

(defun polyir-set-coincident (poly)
  (declare (type Polyhedral-IR poly))
  ;; TODO(hikettei) there should be more clever way to do this:
  (let ((insertable-points (get-coincident-points poly)))
    (dotimes (i (length insertable-points))
      (setf (poly-schedule poly) (isl:schedule-node-get-schedule (insert-parallel (nth i (print (get-coincident-points poly)))))))))
;; (defun polyir-set-parallel (poly)
;; Interchange the largest loop to the outermost
;; If symbolic, insert IF
;; (defun insert-mark (band))
(defun packing (config ir)
  (declare (type polyhedral-ir ir))
  ;; [TODO]
  )
