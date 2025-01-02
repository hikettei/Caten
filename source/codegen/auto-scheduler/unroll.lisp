(defpackage :caten/codegen/unroll
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral :cl-ppcre)
  (:import-from
   :caten/codegen/tiling
   #:tiling-sizes)
  (:export #:apply-packed-funcall #:apply-unroll #:mark-unroll-p #:make-unroll #:parse-unroll-directive #:mark-unroll-body-p #:mark-unroll-parent-p #:get-packable-band))

(in-package :caten/codegen/unroll)

(defun mark-unroll-p (mark)
  (declare (type string mark))
  (or
   (scan "TILE_BAND" (string-upcase mark))
   (scan "UNROLL" (string-upcase mark))))

(defun mark-unroll-body-p (mark) (scan "UNROLL_BODY" (string-upcase mark)))
(defun mark-unroll-parent-p (mark) (scan "UNROLL_PARENT" (string-upcase mark)))

(defun make-unroll (name n-unroll)
  (declare (type integer n-unroll))
  (format nil "~a[~D]" name n-unroll))

(defun parse-unroll-directive (directive)
  (declare (type string directive))
  (assert (mark-unroll-p directive) () "The directive ~a is not an unroll" directive)
  (let ((start (position #\[ directive))
        (end   (position #\] directive)))
    (assert (and start end) () "The directive ~a is not an unroll" directive)
    (parse-integer (subseq directive (1+ start) end))))

(defun schedule-node-band-apply-unroll (schedule-node parent-directive inner-directive n-unroll)
  (declare (type isl::schedule-node schedule-node))
  (let* ((tiled-schedule (isl::schedule-node-insert-mark schedule-node (isl::make-id-from-str parent-directive)))
         (tiled-schedule (isl:schedule-node-get-child tiled-schedule 0))
         (tiled-schedule (isl:schedule-node-band-tile tiled-schedule (tiling-sizes schedule-node :size-default n-unroll)))
         (tiled-schedule (isl::schedule-node-insert-mark (isl::schedule-node-get-child tiled-schedule 0) (isl::make-id-from-str inner-directive))))
    (isl:schedule-node-get-schedule tiled-schedule)))

(defun get-packable-band (node-id si directive-p)
  (declare (type function directive-p) (type string node-id))
  (map-schedule-nodes
   #'(lambda (type node mark)
       (when (and
              (eql type :schedule-node-band)
              (or (null mark) (not (funcall directive-p (princ-to-string mark))))
              (equalp node-id (partial-schedule-node-id (isl:schedule-node-band-get-partial-schedule node))))
         (return-from get-packable-band node)))
   si)
  nil)
;; Generic Implementation which can be applied for UNROLL, VECTORIZE
(defun apply-packed-funcall (schedule-node n-unroll node-id parent-directive inner-directive directive-p)
  "Groups the iteration into several packed-funcall. Packed-Funcall can be also transformed into Unrolling, or Vectorizing.
For example, the following code:
```
for (int i=0; i<a; i++) {
  T0(c0);
}
```
is mutated into:
```
// Mark(Parent_Directive)
for (int i=0; i<a; i+=UNROLL_BY) {
  // Mark(Inner_Directive)
  for (int j=0; min(UNROLL_BY, i % UNROLL_BY); j++) {
    T0(c0+j);
  }
}
```
`caten/codegen/directives` should have an ability to transform the marked loops into the expected form, for example, loop unrolling is mutated as:
```
for (int i=0; i<(a-UNROLL_BY); i+=UNROLL_BY) {
        [packed_funcall]
                 { T0(c0+0)
  T0'(c0, 0~4) = { T0(c0+1)
                 { T0(c0+2)
                 { T0(c0+3)
}
for (int i=a - (mod a UNROLL_BY); i<a; i+=1) {
 T0(c0)
}
```
"
  (declare (type node schedule-node) (type integer n-unroll) (type string inner-directive) (type function directive-p) (type string node-id))
  (assert (and (funcall directive-p parent-directive) (funcall directive-p inner-directive)) () "directive-p should detect both parent and inner directive")
  (let* ((si (getattr schedule-node :polyhedral))
         (band (get-packable-band node-id si directive-p)))
    (setf (poly-schedule si) (schedule-node-band-apply-unroll band parent-directive inner-directive n-unroll))))

(defun apply-unroll (schedule-node unroll-by node-id)
  (declare (type node schedule-node) (type integer unroll-by))
  (apply-packed-funcall schedule-node unroll-by node-id (make-unroll "UNROLL_PARENT" unroll-by) (make-unroll "UNROLL_BODY" unroll-by) #'mark-unroll-p))
