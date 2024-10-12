(in-package :caten/workflow)

(defun %column-major-calc-strides (shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element 1)))
    (loop for i from 1 to (- num-dims 1) do
      (setf (nth i strides) `(* ,(nth (- i 1) strides) ,(nth (- i 1) shape))))
    strides))

(defun %row-major-calc-strides (shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element 1)))
    (loop for i downfrom (- num-dims 2) to 0 do
      (setf (nth i strides) `(* ,(nth (+ i 1) strides) ,(nth (+ i 1) shape))))
    strides))

(defun calc-strides-with-form (order shape)
  "Return (values stride_place stride_forms)"
  (let ((strides (ecase order
                   (:row (%row-major-calc-strides shape))
                   (:column (%column-major-calc-strides shape))))
        (placeholders (loop repeat (length shape) collect (gensym))))
    (values
     placeholders
     (loop for stride in strides
           for place in placeholders
           collect `(_%setf ,place ,stride)))))
