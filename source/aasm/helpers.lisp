(in-package :caten/aasm)

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defun %column-major-calc-strides (shape)
  (declare (type list shape))
  (flet ((const (n) (if (node-p n) n (%iconst n))))
    (let* ((num-dims (length shape))
           (strides (make-list num-dims :initial-element (%iconst 1))))
      (loop for i from 1 to (- num-dims 1) do
	(setf (nth i strides) (%mul (const (nth (- i 1) strides)) (const (nth (- i 1) shape)))))
      strides)))

(defun %row-major-calc-strides (shape)
  (declare (type list shape))
  (flet ((const (n) (if (node-p n) n (%iconst n))))
    (let* ((num-dims (length shape))
           (strides (make-list num-dims :initial-element (%iconst 1))))
      (loop for i downfrom (- num-dims 2) to 0 do
	(setf (nth i strides) (%mul (const (nth (+ i 1) strides)) (const (nth (+ i 1) shape)))))
      strides)))

(defun render-list (list)
  (apply #'concatenate 'string
	 (butlast (loop for n in list
			append (list (format nil "~a" n) ", ")))))

(defun render-attrs (node)
  (if (node-attrs node)	      
      (with-output-to-string (out)
	(format out " where")
	(dolist (k (getattrs node))
	  (when k
	    (format out " :~(~a~)=~a" k (getattr node k)))))
      ""))
