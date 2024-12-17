(defpackage :caten/polyhedral/parallelism
  (:use :cl :caten/polyhedral/ir :caten/polyhedral/config)
  (:export
   #:get-zeros-on-union-set
   #:check-legality-parallel))

(in-package :caten/polyhedral/parallelism)

(defun get-zeros-on-union-set (delta-uset)
  (declare (type isl::union-set delta-uset))
  (let* ((delta-set (isl:set-from-union-set delta-uset))
         (ma (isl:multi-aff-zero (isl:set-get-space delta-set))))
    (isl:union-set-from-set (isl:set-from-multi-aff ma))))

(defun check-legality-parallel (node dep)
  "Reference: https://github.com/hikettei/tadashi/blob/main/src/legality.c#L91-L122"
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

(defun polyir-set-parallel (poly level)
  (declare (type Polyhedral-IR poly))
  (let ((schedule (poly-schedule poly))
        (deps (poly-dependencies poly)))
    (map-schedule-nodes
     #'(lambda (type band)
         (when (eql type :schedule-node-band)
           (print "+++++++++++++")
           (print band)
           (print (check-legality-parallel band deps))))
     poly)
    (print "++++FINISH++++++")
    ))
