(in-package :caten/api)

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defun nth1 (nth list)
  "Just nth but supports -1, -2... accessing"
  (let ((idx (if (>= nth 0)
		 nth
		 (+ (length list) nth))))
    (nth idx list)))

(defun (setf nth1) (value nth list)
  (let ((idx (if (>= nth 0)
		 nth
		 (+ (length list) nth))))
    (setf (nth idx list) value)))

(defun normalize-axis (x n)
  (declare (type tensor x))
  (assert (integerp n) () "axes should be designed as a number, but got ~A" n)
  (if (< n 0) (+ (tensor-nrank x) n) n))

(defun normalize-axes (x axes)
  (if (listp axes) (map 'list #'(lambda (n) (normalize-axis x n)) axes) (list (normalize-axis x axes))))

(defun pad-left (&rest shape)
  (let ((max-dim (reduce #'max (map 'list #'length shape))))
    (mapcar #'(lambda (s) (append (make-list (- max-dim (length s)) :initial-element 1) s)) shape)))
