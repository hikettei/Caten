(in-package :caten/apis)

(deftype axis-t () `(or number symbol Tensor))

(defstruct (ViewRange
	    (:constructor make-vrange (from to by broadcast size subscript
				       &aux
					 (from (->iconst from))
					 (to (->iconst to))
					 (by (->iconst by))
					 (size (->iconst size)))))
  (from from :type Tensor) (to to :type Tensor)
  (by by :type Tensor) (broadcast broadcast :type boolean)
  (size size :type Tensor) (subscript subscript))

(defun vrange-size (vrange)
  (declare (type ViewRange vrange))
  (!div (!sub (viewrange-to vrange) (viewrange-from vrange)) (viewrange-by vrange)))

(defun parse-view-subscript (size subscript)
  (declare (type axis-t size))
  (flet ((normalize (x) (if (and (numberp x) (< x 0)) (!add (->iconst size) (iconst x)) x))
	 (1p (x) (if (tensor-p x) (!add x (iconst 1)) (!add (iconst x) (iconst 1)))))
    (ematch subscript
      ((list :~ n) (make-vrange 0 (normalize n) 1 t size subscript));; broadcasting (:~ N)
      ((eql t)  (make-vrange 0 size 1 nil size subscript)) ;; nothing
      ((guard x (typep x 'axis-t)) (make-vrange (normalize x) (1p (normalize x)) 1 nil size subscript)) ;; A[i]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) 1 nil size subscript)) ;; A[from:to]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)) (guard by (typep to 'axis-t)))
       (make-vrange (normalize from) (normalize to) by nil size subscript))))) ;; A[from:to:by]

(defun .compose-views (old new)
  "
Applying a further slicing:
    (Range 2 10 2) ;; old
 +) (Range 0 4  2) ;; new
 ------------------
    (Range 2 6 2)
"
  (declare (type ViewRange old new))
  (with-slots ((frm1 from) (to1 to) (by1 by) (bc1 broadcast) (size size) (s1 subscript)) old
    (with-slots ((frm2 from) (to2 to) (by2 by) (bc2 broadcast) (s2 subscript)) new
      (let* ((offset (!where (!> by1 (iconst 0)) (!minimum frm1 to1) (!maximum frm1 to1)))
	     (from (!+ offset (!* (!signum by1) frm2)))
	     (from (if (listp s1) from (iconst 0))) ;; A[2][3] is equivalent to A[3], do not compose them.
	     (to   (!+ offset (!* (!signum by1) to2)))
	     (step (!* (!signum (!* by1 by2)) (!lcm by1 by2))))
	(make-vrange from to step bc2 (if bc1 (iconst 1) size) s2)))))

(defun merge-views (base subscripts allow-merge)
  "Composes the two mergeable views"
  (declare (type Tensor base)
	   (type boolean allow-merge)
	   (type list subscripts))
  (let* ((parsed-subscripts (map 'list #'parse-view-subscript (tensor-shape base) subscripts)))
    (when (tensor-views base)
      (assert (= (length (tensor-views base)) (length subscripts))
	      ()
	      "Cannot merge views with different ranks ~a and ~a" (tensor-views base) subscripts))
    (assert (= (ndim base) (length subscripts))
	    ()
	    "Nrank and subscript length should be the same. ~a vs ~a" (shape base) subscripts)
    (when (null (tensor-views base)) (return-from merge-views parsed-subscripts))
    (when (null allow-merge)
      (loop for v in (tensor-views base)
	    for s in parsed-subscripts
	    do (setf (viewrange-size s) (viewrange-size v)))
      (return-from merge-views parsed-subscripts))
    (map 'list #'.compose-views (tensor-views base) parsed-subscripts)))
