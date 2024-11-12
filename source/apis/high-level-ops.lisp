(in-package :caten/apis)
;; ~~~ Dimension Manipulation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodel (SplitNode (sizes &key (dim 0)))
    ((sizes sizes :type (or fixnum list))
     (dim dim :type fixnum)))

(defmethod call ((op SplitNode) &rest inputs)
  (with-attrs ((dim :dim) (sizes :sizes)) op
    (let* ((x (first inputs))
           (dim (normalize-axis (first inputs) dim))
           (_ (assert (integerp (nth dim (shape x))) () "SplitNode: The dimension to split must be an integer."))
           (sizes (if (integerp sizes) ;; Normalize to the list
                      (append
                       (loop for i upfrom 0 below (nth dim (shape x)) by sizes collect sizes)
                       (if (= 0 (mod (nth dim (shape x)) sizes)) ;; Compute Reminder
                           nil
                           (list (mod (nth dim (shape x)) sizes))))
                      sizes)))
      (declare (ignore _))
      (assert (= (length inputs) 1) () "SplitNode only accepts one input.")
      (flet ((~ (last-size size)
               (append
                (loop for i upfrom 0 below dim collect t)
                (list (list last-size (+ last-size size)))
                (loop for i upfrom (1+ dim) below (ndim x) collect t))))
        (assert (= (apply #'+ sizes) (nth dim (shape x))) () "SplitNode: the sum of sizes must be equal to the size of the split dimension.")
        (apply
         #'values
         (loop with total = 0
               for size in sizes
               collect (apply #'!view x (~ total size))
               do (incf total size)))))))

(defun !split (x sizes &key (dim 0))
  "
```
(!split x sizes &key (dim 0))
```

Splits the tensor into multiple tensors along the specified dimension.

If `sizes` is an integer, it splits into equally sized chunks if possible, otherwise the last chunk will be smaller.
If `sizes` is a list, it splits into `(length sizes)` chunks with size in `dim` according to `size`.

Note: The dimension to split must be an integer.
"
  (declare (type tensor x)
           (type (or fixnum list) sizes)
           (type fixnum dim))
  ;; Defmodel doesnot support multiple output creation?
  (forward (SplitNode sizes :dim dim) x))
