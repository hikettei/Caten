(in-package :caten/api)

(defun align-left (shapes)
  (let ((max-dim (reduce #'max (map 'list #'length shapes))))
    (values
     max-dim
     (loop for shape in shapes
           collect
           (append (make-list (- max-dim (length shape)) :initial-element 1) shape)))))

(defun broadcast-shape (shapes)
  (multiple-value-bind (max-dim shapes) (align-left shapes)
    (loop for dim upfrom 0 below max-dim
          for sdim-sizes = (map 'list #'(lambda (x) (nth dim x)) shapes)
          collect (or (find-if #'(lambda (x) (not (eql x 1))) sdim-sizes) 1))))

(defun broadcast-elwise (a b)
  (declare (type Tensor a b))
  (let ((broadcasted (broadcast-shape (list (tensor-shape a) (tensor-shape b)))))
    (values (!expand a broadcasted) (!expand b broadcasted))))

(deftype axis-t () `(or number symbol Tensor))
(defstruct (ViewRange
	    (:constructor make-vrange (from to by broadcast size subscript
				       &aux
					 (from (->size from))
					 (to (->size to))
					 (by (->size by))
					 (size (->size size)))))
  (from from :type Tensor) (to to :type Tensor)
  (by by :type Tensor) (broadcast broadcast :type boolean)
  (size size :type Tensor) (subscript subscript))

(defun vrange-size (vrange)
  (declare (type ViewRange vrange))
  (!div (!sub (viewrange-to vrange) (viewrange-from vrange)) (viewrange-by vrange)))

(defun parse-view-subscript (size subscript)
  (declare (type axis-t size))
  (flet ((normalize (x) (if (and (numberp x) (< x 0)) (!add (->size size) (->size x)) x))
	 (1p (x) (if (tensor-p x) (!add x (->size 1)) (!add (->size x) (->size 1)))))
    (ematch subscript
      ((list :~ n) (make-vrange 0 (normalize n) 1 t size subscript));; broadcasting (:~ N)
      ((eql t)  (make-vrange 0 size 1 nil size subscript)) ;; nothing
      ((guard x (typep x 'axis-t)) (make-vrange (normalize x) (1p (normalize x)) 1 nil size subscript)) ;; A[i]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) 1 nil size subscript)) ;; A[from:to]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)) (guard by (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) by nil size subscript))))) ;; A[from:to:by]
