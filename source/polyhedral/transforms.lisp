(defpackage :caten/polyhedral/transforms
  (:use :cl :caten/polyhedral/ir :caten/polyhedral/config)
  (:export
   #:get-zeros-on-union-set
   #:check-legality-parallel
   #:check-legality
   #:polyir-set-coincident
   #:polyir-loop-interchange))

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

(defun check-legality (schedule dep)
  "
```
(check-legality schedule dep)
```
Returns T if the current schedule does not break any dependences in dep."
  (declare (type isl::schedule schedule) (type isl::union-map dep))
  (when (isl:union-map-is-empty dep) (return-from check-legality t))
  (let* ((map (isl:schedule-get-map schedule))
         (domain (isl:union-map-apply-domain dep map))
         (domain (isl:union-map-apply-range domain map))
         (delta (isl:union-map-deltas domain))
         (zeros (get-zeros-on-union-set delta))
         (le (isl:union-set-lex-le-union-set delta zeros))
         (retval (isl:union-set-is-empty le)))
    retval))

(defun insert-parallel (band)
  (isl:schedule-node-insert-mark band (isl::make-id-from-str "parallel")))

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
      (setf (poly-schedule poly) (isl:schedule-node-get-schedule (insert-parallel (nth i (get-coincident-points poly))))))
    (length insertable-points)))

(defun %loop-interchange (schedule-node)
  (declare (type isl::schedule-node schedule-node))
  (let* ((mupa (isl:schedule-node-band-get-partial-schedule schedule-node))
         (node (isl:schedule-node-delete schedule-node))
         (node (isl:schedule-node-first-child node))
         (node (isl:schedule-node-insert-partial-schedule node mupa)))
    node))

(defun polyir-loop-interchange (poly nth)
  (declare (type polyhedral-ir poly) (type fixnum nth))
  (let ((bands (map-schedule-nodes #'(lambda (type band) (when (eql type :schedule-node-band) band)) poly)))
    (unless (<= nth (length bands)) (return-from polyir-loop-interchange nil))
    (let ((new-sched (isl:schedule-node-get-schedule (%loop-interchange (nth nth bands)))))
      (if (check-legality new-sched (poly-dependencies poly))
          (progn (setf (poly-schedule poly) new-sched) t)
          nil))))
;; (defun polyir-set-parallel (poly)
;; [TODO] Integrate Packing/Unroll/Tiling/Parallel into commands.lisp
;; [TODO] Using WMMA
;; [TODO] Vectorized CLANG
;; Interchange the largest loop to the outermost
;; If symbolic, insert IF
;; (defun insert-mark (band))
(defun packing (config ir)
  (declare (type polyhedral-ir ir))
  ;; [TODO]
  )
