(defpackage :caten/codegen/unroll
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral)
  (:export #:apply-packed-funcall))

(in-package :caten/codegen/unroll)

(defun schedule-apply-schedule-option (si idx)
  (declare (type Polyhedral-IR si))
  (let ((bands (map-schedule-nodes #'(lambda (type node) (when (eql type :schedule-node-band) node)) si)))
    (print bands)
    ))

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
