(defpackage :caten/codegen/unroll
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral)
  (:import-from
   :caten/codegen/tiling
   #:tiling-sizes)
  (:import-from :caten/codegen/scop #:scop)
;;  (:improt-form :caten/codegen/polyhedral-ast #:polyhedral->bp)
  (:export #:apply-packed-funcall #:apply-unroll))

(in-package :caten/codegen/unroll)

(defun schedule-node-band-apply-unroll (schedule-node directive &key (n-unroll 4))
  (declare (type isl::schedule-node schedule-node))
  (let* ((tiled-schedule (isl:schedule-node-band-tile schedule-node (tiling-sizes schedule-node :size-default n-unroll)))
         (tiled-schedule
           (isl:schedule-node-insert-mark
            (isl:schedule-node-get-child tiled-schedule 0)
            (isl::make-id-from-str directive))))
    (isl:schedule-node-get-schedule tiled-schedule)))

(defun get-packable-bands (si directive)
  (map-schedule-nodes
   #'(lambda (type node mark)
       (when (and (eql type :schedule-node-band)
                  (or (null mark) (not (equalp directive (princ-to-string mark)))))
         node))
   si))

(defun schedule-apply-schedule-option (si n-unroll directive)
  (declare (type Polyhedral-IR si))
  (let ((bands (get-packable-bands si directive)))
    (dotimes (nth (length bands))
      (setf (poly-schedule si) (schedule-node-band-apply-unroll (nth nth (get-packable-bands si directive)) directive :n-unroll n-unroll)))))
;; Generic Implementation which can be applied for UNROLL, VECTORIZE
(defun apply-packed-funcall (schedule-node unroll-by directive)
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
The unrolled loop is marked as directive, so the final transformation processes can be placed in polyhedral-ast.lisp
"
  (declare (type node schedule-node) (type integer unroll-by) (type string directive))
  (schedule-apply-schedule-option (getattr schedule-node :polyhedral) unroll-by directive))

(defun apply-unroll (schedule-node unroll-by)
  (declare (type node schedule-node) (type integer unroll-by))
  (scop 
  )
