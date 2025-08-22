(in-package :caten/aasm)

(defclass ScheduleItem () nil)
(defclass ScheduleTime (AType) ((type :initarg :type :reader st-type)))

(defun verify-schedule-item (self)
  #'(lambda (id->type node)
      (let ((ptypes (loop for i in (node-reads node) collect (gethash i id->type))))
        (assert (every #'(lambda (x) (find (st-type x) `(:Affine :Nonaffine))) ptypes)))
      (list (make-instance 'ScheduleTime :type self))))

(defnode (:Schedule :Affine) (ScheduleItem)
         "
```
  [VIEW]
     ↓
 [items x N] where each item is jitable
     ↓
   [out]
```
"
         :slots ((view) (items) (polyhedron))
         :type-relay (verify-schedule-item :Affine))

(defnode (:Schedule :NonAffine) (ScheduleItem)
         ""
         :slots ((items :initform nil))
         :type-relay (verify-schedule-item :Nonaffine))
