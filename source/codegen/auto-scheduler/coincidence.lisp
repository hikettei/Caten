(defpackage :caten/codegen/coincidence
  (:use :cl :caten/codegen/polyhedral)
  (:export
   #:check-legality-parallel
   #:check-legality
   #:apply-parallel))

(in-package :caten/codegen/coincidence)

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
   #'(lambda (type band mark)
       (when (and (eql type :schedule-node-band) (not (equalp (princ-to-string mark) "PARALLEL")))
         (when (check-legality-parallel band (poly-dependencies poly)) band)))
   poly))

(defun apply-parallel (poly level &aux (c 0))
  (declare (type Polyhedral-IR poly))
  (loop for bands = (get-coincident-points poly)
        while bands do
          (when (> c level) (return-from apply-parallel))
          (incf c)
          (setf (poly-schedule poly) (isl:schedule-node-get-schedule (insert-parallel (car (get-coincident-points poly)))))))

(defun %loop-interchange (schedule-node)
  (declare (type isl::schedule-node schedule-node))
  (let* ((mupa (isl:schedule-node-band-get-partial-schedule schedule-node))
         (node (isl:schedule-node-delete schedule-node))
         (n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))
         (_ (when (= 0 n-child) (return-from %loop-interchange nil)))
         (node (isl:schedule-node-first-child node))
         (__   (when (find (isl:schedule-node-get-type node) `(:schedule-node-filter)) (return-from %loop-interchange nil)))
         (node (isl:schedule-node-insert-partial-schedule node mupa)))
    (declare (ignore _ __))
    node))

(defun polyir-loop-interchange (poly nth)
  (declare (type polyhedral-ir poly) (type fixnum nth))
  (let ((bands (map-schedule-nodes #'(lambda (type band mark) (when (eql type :schedule-node-band) band)) poly)))
    (unless (<= nth (length bands)) (return-from polyir-loop-interchange nil))
    (let* ((new-sched (%loop-interchange (nth nth bands)))
           (new-sched (when new-sched (isl:schedule-node-get-schedule new-sched))))
      (if (and new-sched (check-legality new-sched (poly-dependencies poly)))
          (progn (setf (poly-schedule poly) new-sched) t)
          nil))))
