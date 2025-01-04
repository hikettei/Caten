(defpackage :caten/codegen/transform
  (:documentation "Provides various polyhedral transformation for the code transformation")
  (:shadow #:set #:space)
  (:shadowing-import-from :cl :map)
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral :cl-ppcre :caten/isl)
  (:export
    #:get-zeros-on-union-set #:check-legality-parallel #:check-legality))

(in-package :caten/codegen/transform)
;; ~~ Legality Computations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun get-zeros-on-union-set (delta-uset)
  (declare (type isl::union-set delta-uset))
  (let* ((delta-set (set-from-union-set delta-uset))
         (ma (multi-aff-zero (set-get-space delta-set))))
    (union-set-from-set (set-from-multi-aff ma))))

(defun check-legality-parallel (node dep)
  "
```
(check-legality-parallel node dep)
```
Returns T if the band node is legal to be parallelized with respect to the dep.
Reference: https://github.com/hikettei/tadashi/blob/main/src/legality.c#L91-L122"
  (declare (type isl::schedule-node node) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from check-legality-parallel t))
  (let* ((map (schedule-node-band-get-partial-schedule-union-map node))
         (domain (union-map-apply-range (union-map-apply-domain dep map) map))
         (delta (union-map-deltas domain))
         (_ (when (union-set-is-empty delta) (return-from check-legality-parallel t)))
         (zeros (get-zeros-on-union-set delta))
         (cmp (union-set-lex-lt-union-set delta zeros))
         (retval (union-set-is-empty cmp))
         (cmp (union-set-lex-gt-union-set delta zeros)))
    (declare (ignore _))
    (and retval (union-set-is-empty cmp))))

(defun check-legality (schedule dep)
  "
```
(check-legality schedule dep)
```
Returns T if the current schedule does not break any dependences in dep."
  (declare (type isl::schedule schedule) (type isl::union-map dep))
  (when (union-map-is-empty dep) (return-from check-legality t))
  (let* ((map (schedule-get-map schedule))
         (domain (union-map-apply-domain dep map))
         (domain (union-map-apply-range domain map))
         (delta (union-map-deltas domain))
         (zeros (get-zeros-on-union-set delta))
         (le (union-set-lex-le-union-set delta zeros))
         (retval (union-set-is-empty le)))
    retval))
;; ~~ Legality Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-interchange (poly band1 idx)
  "Returns a new schedule of band1 with band1 and idx'th band node interchanged. Returns nil if the operation breaks the dependencies."
  (declare (type polyhedral-ir poly) (type isl::schedule-node band1) (type fixnum idx))
  (assert (eql (isl:schedule-node-get-type band1) :schedule-node-band))
  (let* ((mupa (isl:schedule-node-band-get-partial-schedule band1))
         (node (isl:schedule-node-delete band1))
         (n-child (isl::%isl-schedule-node-n-children (isl::schedule-node-handle node)))
         (_ (when (= 0 n-child) (return-from apply-interchange nil))) ;; Nothing to interchange
         (node (schedule-node-get-band-from-relative-idx node idx))
         (__ (assert node () "IDX=~a does not exists in the schedule:~%~A" idx band1))
         (node (isl:schedule-node-insert-partial-schedule node mupa)))
    (declare (ignore _ __))
    (when (check-legality (isl:schedule-node-get-schedule node) (poly-dependencies poly))
      node)))
;; ~~ Tile Based Transformations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Directive ()
  ((visible :initarg :visible :accessor visible-p :initform t)))

(defmethod directive->id ((directive Directive))

  )

(defmethod id->directive ((string string))
  
  )
