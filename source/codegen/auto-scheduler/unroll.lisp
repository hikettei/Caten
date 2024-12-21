(defpackage :caten/codegen/unroll
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral)
  (:import-from
   :caten/codegen/tiling
   #:tiling-sizes)
  (:export #:apply-packed-funcall))

(in-package :caten/codegen/unroll)

(defun schedule-node-band-apply-unroll (schedule-node)
  (declare (type isl::schedule-node schedule-node))
  (print "UNROLL")
  (let* ((tiled-schedule (isl:schedule-node-band-tile schedule-node (tiling-sizes schedule-node :size-default 16)))
         (tiled-schedule (isl:schedule-node-get-child tiled-schedule 0))
         (tiled-schedule (isl:schedule-node-band-set-ast-build-options tiled-schedule (isl:union-set-from-str "{ unroll[4] }"))))
    (isl:schedule-node-get-schedule schedule-node)))

(defun schedule-apply-schedule-option (si idx)
  (declare (type Polyhedral-IR si))
  (let ((bands (map-schedule-nodes #'(lambda (type node) (when (eql type :schedule-node-band) node)) si)))
    (when bands
      (setf (poly-schedule si) (schedule-node-band-apply-unroll (first bands))))))

(defun apply-packed-funcall (schedule-node gid unroll-by)
  "Groups the iteration into several packed-funcall.
Packed-Funcall can be also transformed into Unrolling, or Vectorizing.
For example, the following code:
```
for (int i=0; i<a; i++) {
  T0(c0);
}
```
is mutated into:
```
for (int i=0; i<(a-UNROLL_BY); i+=UNROLL_BY) {
        [packed_funcall]
                 { T0(c0+0)
  T0'(c0, 0~4) = { T0(c0+1)
                 { T0(c0+2)
                 { T0(c0+3)
}
for (int i=a - (mod a UNROLL_BY); i<a; i+=1) {
 T0(c0) // Loop Reminder (TODO: Optimize Index Computation)
}
```
"
  (declare (type node schedule-node) (type integer unroll-by) (type symbol gid))
  (schedule-apply-schedule-option (getattr schedule-node :polyhedral) nil)
  
  )
