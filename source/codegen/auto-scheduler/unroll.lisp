(defpackage :caten/codegen/unroll
  (:use :cl :caten/aasm :caten/air :caten/codegen/polyhedral :cl-ppcre)
  (:import-from
   :caten/codegen/tiling
   #:tiling-sizes)
  (:import-from :caten/codegen/scop #:with-polyhedral-space)
;;  (:improt-form :caten/codegen/polyhedral-ast #:polyhedral->bp)
  (:export #:apply-packed-funcall #:apply-unroll #:mark-unroll-p #:make-unroll #:parse-unroll-directive))

(in-package :caten/codegen/unroll)

(defun mark-unroll-p (mark)
  (declare (type string mark))
  (scan "UNROLL" (string-upcase mark)))

(defun make-unroll (name n-unroll)
  (declare (type integer n-unroll))
  (format nil "~a[~D]" name n-unroll))

(defun parse-unroll-directive (directive)
  (declare (type string directive))
  (assert (mark-unroll-p directive) () "The directive ~a is not an unroll" directive)
  (parse-integer (subseq directive 7 (1- (length directive)))))

(defun schedule-node-band-apply-unroll (schedule-node parent-directive inner-directive n-unroll)
  (declare (type isl::schedule-node schedule-node))
  (let* ((tiled-schedule (isl::schedule-node-insert-mark schedule-node (isl::make-id-from-str parent-directive)))
         (tiled-schedule (isl:schedule-node-get-child tiled-schedule 0))
         (tiled-schedule (isl:schedule-node-band-tile tiled-schedule (tiling-sizes schedule-node :size-default n-unroll)))
         (tiled-schedule (isl::schedule-node-insert-mark (isl::schedule-node-get-child tiled-schedule 0) (isl::make-id-from-str inner-directive))))
    (isl:schedule-node-get-schedule tiled-schedule)))

(defun get-packable-bands (si directive-p)
  (declare (type function directive-p))
  (map-schedule-nodes
   #'(lambda (type node mark)
       (when (and (eql type :schedule-node-band) (or (null mark) (not (funcall directive-p (princ-to-string mark)))))
         node))
   si))

(defun schedule-apply-schedule-option (si n-unroll parent-directive inner-directive directive-p)
  (declare (type Polyhedral-IR si))
  (loop for bands = (get-packable-bands si directive-p)
        while bands do
          (setf (poly-schedule si) (schedule-node-band-apply-unroll (car bands) parent-directive inner-directive n-unroll))))
;; Generic Implementation which can be applied for UNROLL, VECTORIZE
(defun apply-packed-funcall (schedule-node unroll-by parent-directive inner-directive directive-p)
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
  (declare (type node schedule-node) (type integer unroll-by) (type string inner-directive) (type function directive-p))
  (assert (and (funcall directive-p parent-directive) (funcall directive-p inner-directive)) () "directive-p should detect both parent and inner directive")
  (schedule-apply-schedule-option (getattr schedule-node :polyhedral) unroll-by parent-directive inner-directive directive-p))

(defun apply-unroll (schedule-node unroll-by)
  (declare (type node schedule-node) (type integer unroll-by))
  (apply-packed-funcall schedule-node unroll-by (make-unroll "UNROLL_PARENT" unroll-by) (make-unroll "UNROLL_BODY" unroll-by) #'mark-unroll-p))
