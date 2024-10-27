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
      ;; Simplifies for the common cases
      (trivia:match s2
        ((guard x (eql x t)) old)
        ((list :~ (guard x (or (symbolp x) (numberp x))))
         (assert bc2)
         (make-vrange frm2 to2 by2 bc2 size s2))
        (_
         ;; Computes from/to/step manually
         ;; [FIXME]
         ;; During JIT execution, Scheduler may generate guard statements due to incorrect inference of dynamic_shape in the projection from Symbol to Symbol.
         ;; e.g. (A B) Tesnor -> View ([c, d], [e, f]) 
         (let* ((offset (!where (!> by1 (iconst 0)) (!minimum frm1 to1) (!maximum frm1 to1)))
	        (from (!+ offset (!* (!signum by1) frm2)))
	        (from (if (listp s1) from (iconst 0))) ;; A[2][3] is equivalent to A[3], do not compose them.
	        (to   (!+ offset (!* (!signum by1) to2)))
	        (step (!* (!signum (!* by1 by2)) (!lcm by1 by2))))
	   (make-vrange from to step bc2 (if bc1 (iconst 1) size) s2)))))))

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

;; ~~ Shape Tracker ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Handle the stride by ShapeTracker
;; AASM Based Construction
;; Tensor Shaped Tensor Creation Support!

;; Workload
;; 1. Modules can inference ShapeTracker?
;; 2. Tensor Shaped Tensor? Stride Computation?
;; 3. defsimplifier (:VIEW VIEW) -> :VIEW
;; 4. Create a cache for lowering modules
(defstruct (Tracker
            (:conc-name tr-)
            (:constructor make-tracker (shape mask order permute broadcast &key (contiguous nil))))
  (shape shape :type list)
  (shape-for-stride (copy-list shape) :type list)
  (order order :type (member :row :column))
  (permute permute :type list)
  (broadcast broadcast :type list)
  (mask mask :type list)
  (contiguous contiguous :type boolean))

(defmethod print-object ((tr Tracker) stream)
  (print-unreadable-object (tr stream :type t)
    (flet ((p (obj)
             (if (tensor-p obj)
                 (format nil "[tensor: ~a]" (tensor-id obj))
                 obj)))
      (format stream ":order={~(~a~)~a} :shape=(~a) :contiguous-p=~a" (tr-order tr) (tr-permute tr)
              (apply
               #'concatenate
               'string
               (butlast
                (loop for s in (tr-shape tr)
                      for m in (tr-mask tr)
                      for b in (tr-broadcast tr)
                      append
                      (list (format nil "~a~a"
                                    (if b (format nil "~~~a" (p b)) (p s))
                                    (if m
                                        (format nil "~a" (map 'list #'p m))
                                        ""))
                            " "))))
              (tr-contiguous tr)))))

(defun canonicalize-int (int) (if (tensor-p int) (or (try-fold-constant int) int) int))
(defun canonicalize-shape (list) (map 'list #'canonicalize-int list))

(defun start-tracking (shape &key (order :row) (mask nil) &aux (shape (canonicalize-shape shape)))
  (declare (type (member :row :column) order)
           (type list shape))
  (assert (every #'(lambda (s) (or (tensor-p s) (symbolp s) (and (integerp s) (> s 0)))) shape) () "Trying to create tracker with negative dimension: ~a" shape)
  ;; Canonicalize mask
  (when (and mask (every #'(lambda (m s) (equal m (list 0 s))) mask shape)) (setf mask nil))
  (let ((mask (or mask (loop repeat (length shape) collect nil)))
        (broadcast (loop repeat (length shape) collect nil)))
    (make-tracker shape mask order (range 0 (length shape)) broadcast :contiguous t)))

(defmethod tr-apply-permute ((tensor Tensor) order) (tr-apply-permute (tensor-tr tensor) order))
(defmethod tr-apply-permute ((tracker Tracker) order)
  (assert (= (length order) (length (tr-shape tracker))) () "Trying to permute tracker with different dimension: ~a" order)
  (assert (equal (sort (copy-list order) #'<) (range 0 (length order))) () "Trying to permute tracker with invalid order: ~a, order are given from shuffling ~a" order (range 0 (length order)))
  (let ((new-tracker (copy-tracker tracker)))
    (setf (tr-shape new-tracker)     (permute-list order (tr-shape tracker))
          (tr-mask new-tracker)      (permute-list order (tr-mask tracker))
          (tr-permute new-tracker)   (permute-list order (tr-permute tracker))
          (tr-broadcast new-tracker) (permute-list order (tr-broadcast tracker)))
    new-tracker))

(defmethod tr-apply-uprank ((tensor Tensor) mask) (tr-apply-uprank (tensor-tr tensor) mask))
(defmethod tr-apply-uprank ((tracker Tracker) mask)
  (assert (= (count-if #'identity mask) (length (tr-shape tracker))) () "Trying to uprank tracker with different dimension: ~a" mask)
  (assert (every #'(lambda (x) (typep x 'boolean))  mask) () "Trying to uprank tracker with invalid mask: ~a" mask)
  ;; T=existing dimension, NIL=new dimension sized 1
  (let* ((new-tracker (copy-tracker tracker))
         (new-shape (loop for m in mask
                          if m collect (pop (tr-shape new-tracker))
                            else collect 1))
         (new-mask (loop for m in mask
                         if m collect (pop (tr-mask new-tracker))
                           else collect nil))
         (new-broadcast (loop for m in mask
                              if m collect (pop (tr-broadcast new-tracker))
                                else collect nil))
         (new-permute (loop for m in mask
                            for nth upfrom 0
                            for offset = (count-if #'null (subseq mask 0 nth))
                            if m collect (+ offset (pop (tr-permute new-tracker)))
                              else collect nth)))
    (assert (equal (sort (copy-list new-permute) #'<) (range 0 (length new-permute))))
    (setf (tr-shape new-tracker) new-shape
          (tr-shape-for-stride new-tracker) (permute-list new-permute new-shape)
          (tr-mask new-tracker) new-mask
          (tr-permute new-tracker) new-permute
          (tr-broadcast new-tracker) new-broadcast)
    new-tracker))

(defgeneric tr-reshapeable-p (obj new-shape))
(defmethod tr-reshapeable-p ((tensor Tensor) new-shape) (tr-reshapeable-p (tensor-tr tensor) new-shape))
(defmethod tr-reshapeable-p ((tracker Tracker) new-shape &aux (new-shape (canonicalize-shape new-shape)))
  (let ((shape-w/o-one (loop for s in new-shape if (not (eql s 1)) collect s)))
    (when (equal shape-w/o-one (tr-shape tracker))
      (return-from tr-reshapeable-p t)))
  ;; Not contiguous -> Not reshapeable (todo: masked reshape)
  (when (null (tr-contiguous tracker)) (return-from tr-reshapeable-p nil))
  ;; Permuted -> Not contiguous
  (when (not (equal (range 0 (length (tr-shape tracker))) (tr-permute tracker)))
    (return-from tr-reshapeable-p nil))
  ;; Broadcasted -> Not contiguous
  (when (some #'identity (tr-broadcast tracker))
    (return-from tr-reshapeable-p nil))
  t)

(defmethod tr-apply-reshape ((tensor Tensor) new-shape) (tr-apply-reshape (tensor-tr tensor) new-shape))
(defmethod tr-apply-reshape ((tracker Tracker) new-shape &aux (new-shape (canonicalize-shape new-shape)))
  (assert (every #'(lambda (s) (or (symbolp s) (tensor-p s) (and (integerp s) (> s 0)))) new-shape) () "Trying to reshape tracker with negative or wrong typed dimension: ~a" new-shape)
  (assert (tr-reshapeable-p tracker new-shape) () "tr-apply-reshape: ~a ~a is not reshapeable! Call !contiguous in advance." tracker new-shape)
  (let ((shape-w/o-one (loop for s in new-shape if (not (eql s 1)) collect s)))
    (when (equal shape-w/o-one (tr-shape tracker))
      (return-from tr-apply-reshape (tr-apply-uprank tracker (map 'list #'(lambda (s) (not (eql s 1))) new-shape))))
    ;; Can reshape (reshape=chainging the stride)
    (let* ((new-tracker (copy-tracker tracker)))
      (setf (tr-shape new-tracker) new-shape
            (tr-shape-for-stride new-tracker) (copy-list new-shape)
            (tr-permute new-tracker) (range 0 (length new-shape))
            (tr-mask new-tracker) (make-list (length new-shape))
            (tr-broadcast new-tracker) (make-list (length new-shape))
            (tr-contiguous new-tracker) t)
      new-tracker)))

(defmethod tr-apply-slice ((tensor Tensor) slice new-shape) (tr-apply-slice (tensor-tr tensor) slice new-shape))
(defmethod tr-apply-slice ((tracker Tracker) slice new-shape &aux (new-shape (canonicalize-shape new-shape)))
  (assert (= (length slice) (length (tr-shape tracker))) () "Trying to slice tracker with different dimension: ~a" slice)
  (assert (every #'viewrange-p slice) () "Trying to slice tracker with invalid slice: ~a" slice)
  (let ((new-tracker (copy-tracker tracker)))
    (setf (tr-shape new-tracker) new-shape
          (tr-mask new-tracker)
          (loop for s in slice
                collect (list (canonicalize-int (viewrange-from s)) (canonicalize-int (viewrange-to s)) (canonicalize-int (viewrange-by s))))
          (tr-contiguous new-tracker) nil)
    new-tracker))

(defmethod tr-apply-broadcast ((tensor Tensor) subscript) (tr-apply-broadcast (tensor-tr tensor) subscript))
(defmethod tr-apply-broadcast ((tracker Tracker) subscript)
  "subscript = (nil 10 nil ...)"
  (assert (= (length subscript) (length (tr-shape tracker))) () "Trying to broadcast tracker with different dimension: ~a" subscript)
  (assert (every #'(lambda (s) (or (null s) (tensor-p s) (symbolp s) (and (integerp s) (> s 0)))) subscript) () "Trying to broadcast tracker with negative or wrong typed dimension: ~a" subscript)
  (let* ((subscript
           (loop for s in subscript
                 if s collect (canonicalize-int s)
                 else collect nil))
         (merged
          (loop for b in (tr-broadcast tracker)
                for s in subscript
                collect (if b b s))))
    (let ((new-tracker (copy-tracker tracker)))
      (setf (tr-broadcast new-tracker) merged)
      new-tracker)))

(defmethod %make-view-from-tracker ((tracker Tracker) write-to base)
  (%view
   base
   (tr-shape tracker)
   (loop for m in (tr-mask tracker)
         if m collect (car m) else collect 0)
   (loop for m in (tr-mask tracker)
         for s in (tr-shape tracker)
         for b in (tr-broadcast tracker)
         if m collect (second m) else collect (if b b s))
   (loop for m in (tr-mask tracker)
         if m collect m else collect 1)
   (loop for b in (tr-broadcast tracker)
         if b collect t else collect nil)
   (permute-list (tr-permute tracker) (%stride (tr-shape-for-stride tracker) (tr-mask tracker)))
   :id write-to
   :permute (tr-permute tracker)))
