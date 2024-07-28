(in-package :caten)

(deftype axis-t () `(or number symbol Tensor))

(defstruct (ViewRange
	    (:constructor make-vrange (from to by broadcast
				       &aux
					 (from (->iconst from))
					 (to (->iconst to))
					 (by (->iconst by)))))
  (from from :type Tensor) (to to :type Tensor)
  (by by :type Tensor) (broadcast broadcast :type boolean))

(defun vrange-size (vrange)
  (declare (type ViewRange vrange))
  ;; [TODO]: !abs
  (!div (!sub (viewrange-to vrange) (viewrange-from vrange)) (viewrange-by vrange)))

(defun parse-view-subscript (size subscript)
  (declare (type axis-t size))
  ;; [TODO] -1とかの処理はここじゃないとできない？
  ;; T = shape of corresponding axis
  (flet ((1p (x) (if (tensor-p x) (!add x (iconst 1)) (!add (iconst 1) (iconst x)))))
    (ematch subscript
      ((list :~ n) (make-vrange 0 n 1 t))   ;; broadcasting (:~ N)
      ((eql t)  (make-vrange 0 size 1 nil)) ;; nothing
      ((guard x (typep x 'axis-t)) (make-vrange x (1p x) 1 nil)) ;; A[i]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)))
       (make-vrange from to 1 nil)) ;; A[from:to]
      ((list (guard from (typep from 'axis-t)) (guard to (typep to 'axis-t)) (guard by (typep to 'axis-t)))
       (make-vrange from to by nil))))) ;; A[from:to:by]

(defun .compose-views (old new)
  "
Applying a further slicing:
    (Range 2 10 2) ;; old
 +) (Range 0 4  2) ;; new
 ------------------
    (Range 2 6 2)
"
  (declare (type ViewRange old new))
  (with-slots ((frm1 from) (to1 to) (by1 by)) old
    (with-slots ((frm2 from) (to2 to) (by2 by) (bc2 broadcast)) new
      (make-vrange
       ;; [TODO] あってる？
       (!add frm1 frm2)
       (!sub to1 to2)
       (!mul by1 by2)
       bc2))))

(defun merge-views (base subscripts)
  "Composes the two mergeable views"
  (declare (type Tensor base)
	   (type list subscripts))
  (let* ((parsed-subscripts (map 'list #'parse-view-subscript (tensor-shape base) subscripts)))
    (when (null (tensor-views base)) (return-from merge-views parsed-subscripts))
    (map 'list #'.compose-views (tensor-views base) parsed-subscripts)))

