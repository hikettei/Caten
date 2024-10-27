(in-package :caten/apis)
;; [TODO] Handle the stride by ShapeTracker
;; AASM Based Construction
;; Tensor Shaped Tensor Creation Support!
;; Well tested?
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
                               (if b (format nil "~~~a" b) s)
                               (if m
                                   (format nil "{~a}" m)
                                   ""))
                          " "))))
            (tr-contiguous tr))))

(defun start-tracking (shape &key (order :row) (mask nil))
  (declare (type (member :row :column) order)
           (type list shape))
  (assert (every #'(lambda (s) (or (symbolp s) (and (integerp s) (> s 0)))) shape) () "Trying to create tracker with negative dimension: ~a" shape)
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

(defmethod tr-apply-reshape ((tensor Tensor) new-shape) (tr-apply-reshape (tensor-tr tensor) new-shape))
(defmethod tr-apply-reshape ((tracker Tracker) new-shape)
  (let ((shape-w/o-one (loop for s in new-shape if (not (eql s 1)) collect s)))
    (when (equal shape-w/o-one (tr-shape tracker))
      (return-from tr-apply-reshape (tr-apply-uprank tracker (map 'list #'(lambda (s) (not (eql s 1))) new-shape))))
    ;; Masked reshape -> not contiguous?
    (when (null (tr-contiguous tracker))
      ;; [TODO] Implement masked reshape
      (return-from tr-apply-reshape nil))
    ;; Permuted -> Not contiguous
    (when (not (equal (range 0 (length (tr-shape tracker))) (tr-permute tracker)))
      (return-from tr-apply-reshape nil))
    ;; Broadcasted -> Not contiguous
    (when (some #'identity (tr-broadcast tracker))
      (return-from tr-apply-reshape nil))
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
(defmethod tr-apply-slice ((tracker Tracker) slice new-shape)
  (assert (= (length slice) (length (tr-shape tracker))) () "Trying to slice tracker with different dimension: ~a" slice)
  (error "TODO")
  )

(defmethod tr-apply-broadcast ((tensor Tensor) subscript) (tr-apply-broadcast (tensor-tr tensor) subscript))
(defmethod tr-apply-broadcast ((tracker Tracker) subscript)
  "subscript = (nil 10 nil ...)"
  (assert (= (length subscript) (length (tr-shape tracker))) () "Trying to broadcast tracker with different dimension: ~a" subscript)
  (let ((merged
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

;; ~~ ShapeTracker DSL ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defpattern sym (to-what) `(and (type symbol) (satisfies (lambda (x) (equalp (symbol-name x) ,to-what)))))
  (defstruct (AxisTracker
	      (:conc-name at-)
	      (:constructor make-at (name nrank shape)))
    (name name :type symbol)
    (nrank nrank :type fixnum)
    (shape shape :type list))
  (defun %at->list (at) (%make-at (at-name at) (at-nrank at) (at-shape at)))
  (defun %make-at (name nrank shape) `(make-at ,name ,nrank ',shape))
  (defun %st->list (st)
    `(make-st
      ,(st-base st)
      (list ,@(map 'list #'%at->list (st-bf st)))
      (list ,@(map 'list #'%at->list (st-aft st)))))	      
  (defstruct (ShapeTracker
	      (:conc-name st-)
	      (:constructor make-st (base bf aft)))
    (base base :type string)
    (bf bf :type list)
    (aft aft :type list))
  (defun read-subscript (subscripts)
    (declare (type string subscripts))
    (read-from-string
     (format nil "( ~a )" (regex-replace-all "\\]" (regex-replace-all "\\[" subscripts" [ ") " ] "))))
  (defun %parse-shape-notation (subscript)
    "return: (values at rest)"
    (declare (type list subscript))
    (ematch subscript
      ((list* name (sym "[") (sym "]") rest)
       (values (make-at (intern (symbol-name name) "KEYWORD") 0 nil) rest))
      ((list* name (sym "[") rest)
       (let ((subscripts)
	     (count 0))
	 (loop named explore
	       for r in rest
	       if (equal "[" (symbol-name r)) do
		 (error "ShapeTracker: Brackets do not match: ~a." subscript)
	       else if (equal "]" (symbol-name r)) do
		 (incf count)
		 (return-from explore)
	       else do
		 (incf count)
		 (push (intern (symbol-name r) "KEYWORD") subscripts))
	 (dotimes (i count) (pop rest))
	 (setf subscripts (reverse subscripts))
	 (let ((count (count :~ subscripts)))
	   (assert (< count 2)
		   ()
		   "ShapeTracker: Infinite rank can be used only once. ~a" subscript)
	   (assert (= (or (position :~ subscripts) 0) 0)
		   ()
		   "ShapeTracker: Infinite rank must be placed at 0th axis.~%e.g.: A[~~ m n k]~%butgot: ~a" subscript)
	 (values (make-at (intern (symbol-name name) "KEYWORD") (- (length subscripts) count) subscripts) rest))))))
  (defun %parse-st (st)
    (declare (type string st))
    (let ((terms (split "->" (format nil "~a " st))))
      (assert (= (length terms) 2)
	      ()
	      "Failed to compile ShapeTracker.~%~%Follow the format below:
    (From-Shape) -> (After-Shape) where X = (S-Expression) ...~%~% ~a ~%~a"
	      st
	      (if (<= (length terms) 1) "    Explicit `->`" "    `->` can only be used once."))
      (flet ((parse-at (lst)
	       (loop while lst
		     collect
		     (multiple-value-bind (at new)
			 (%parse-shape-notation lst)
		       (setf lst new)
		       at))))
	(multiple-value-bind (before after)
	    (values (parse-at (read-subscript (first terms))) (parse-at (read-subscript (second terms))))
	  (make-st st before after)))))
  
  (defun %broadcast-auto (tensors st)
    (declare (type list tensors) (type ShapeTracker st))
    (symbol-macrolet ((->failed (return-from %broadcast-auto tensors)))
      (let* ((kranks (loop for s in (st-bf st)
			   for tns in tensors
			   if (eql :~ (car (at-shape s)))
			     collect (cons (1- (length (at-shape s))) (length (tensor-shape tns)))
			   else
			     collect nil)))
	(when (every #'null kranks)->failed)
	(when (some #'(lambda (x) (< (cdr x) (car x))) kranks)->failed)
	(let* ((aligned-tensors
		 (loop with tallest fixnum = (apply #'max (map 'list #'cdr kranks))
		       for tns in tensors
		       for krank in kranks
		       if (or (null krank) (= (length (tensor-shape tns)) tallest))
			 collect tns
		       else collect (!uprank tns (- tallest (length (tensor-shape tns))))))
	       (shape-goal
		 (loop with tallest = (apply #'max (map 'list #'cdr kranks))
		       for n upfrom 0 below tallest
		       collect
		       (let* ((shapes
				(map 'list
				     #'(lambda (r tns)
					 (and (>= (- n (- tallest (cdr r))) 0)
					      (nth (- n (- tallest (cdr r)))
						   (tensor-shape tns))))
				     kranks tensors))
			      (shapes (loop for s in shapes if s collect s)))
			 (cond
			   ((= (length shapes) 1) (car shapes))
			   ((every #'(lambda (x) (eql x 1)) shapes) 1)
			   ((some #'(lambda (x) (eql x 1)) shapes)
			    (or
			     (find 1 shapes :test #'(lambda (x y) (and (numberp y) (not (= x y)))))
			     (find 1 shapes :test (compose #'not #'eql))))
			   (T
                            ;; fixme: is it (car shapes) ?
                            (or
                             (nth n shapes)
                             (car shapes))))))))
	  (flet ((->subscript (tns ignore-last-k)
		   (loop with rank = (length (tensor-shape tns))
			 for s in (tensor-shape tns)
			 for g in shape-goal
			 for i upfrom 0
			 if (and (>= (- rank i) ignore-last-k) (eql s 1) (not (eql g 1)))
			   collect (progn (assert g) `(:~ ,g))
			 else collect t)))
	    (loop for tns in aligned-tensors
		  for k in kranks
		  for nth upfrom 0
		  if k
		    collect
		    (let ((ss (->subscript tns (car k))))
		      (if (every #'(lambda (x) (eql t x)) ss)
			  tns
			  (if (= nth 0)
			      (!contiguous (apply #'!view tns ss))
			      (apply #'!view tns ss))))
		  else
		    collect tns))))))
			 
  (defun %solve-st (st lazy-solve allow-broadcast &rest tensors &aux (tensors (flatten tensors)))
    "lazy-solve = (symbol . value)"
    (declare (type ShapeTracker st)
	     (type list tensors)
	     (type boolean allow-broadcast))
    (let ((tensors
	    (if allow-broadcast
		(%broadcast-auto tensors st)
		tensors))
	  (infinite-part)
	  (inf-size)
	  (solved (make-hash-table)))
      (loop for (k . f) in lazy-solve do
	(assert (keywordp k) () "ShapeTracker: key is a keyword")
	(setf (gethash k solved) f))
      (assert (= (length tensors) (length (st-bf st)))
	      ()
	      "ShapeTracker: ~a~%The number of input tensors do not match with the declared shape tracker.~%~a" (st-base st) tensors)
      (loop for tensor in tensors
	    for decl in (st-bf st)
	    for nth upfrom 0
	    if (= (length (at-shape decl)) 0)
	      ;; -> scalar
	      do (assert (= (length (tensor-shape tensor)) 0)
			 ()
			 "ShapeTracker: ~a~% ~ath tensor should be a scalar, but got ~a." (st-base st) nth tensor)	       
	    else do
	      (let ((offset 0))
		;; Tensor w/ infinite rank
		(when (position :~ (at-shape decl))
		  (assert (>= (length (tensor-shape tensor)) (at-nrank decl))
			  ()
			  "ShapeTracker: ~a~% Insufficient rank" (st-base st))
		  (let ((n-inf (- (length (tensor-shape tensor)) (at-nrank decl))))
		    (incf offset n-inf)
		    (when (null inf-size)
		      (setf inf-size n-inf
			    infinite-part (make-list n-inf :initial-element nil)))
		    (assert (= inf-size n-inf) () "ShapeTracker: ~a~%The length of ~~ is inconsistent." (st-base st))
		    (dotimes (i n-inf)
		      (if (null (nth i infinite-part))
			  (setf (nth i infinite-part) (nth i (tensor-shape tensor)))
			  (when (and (numberp (nth i (tensor-shape tensor))) (numberp (nth i infinite-part)))
			    (assert (= (nth i infinite-part) (nth i (tensor-shape tensor)))
				    ()
				    "ShapeTracker: ~a~%The shape is inconsistent: ~~ = ~a."
				    (st-base st)
				    infinite-part))))))		
		;; Tensor w/o infinite rank
		(loop for place in (nthcdr (if inf-size 1 0) (at-shape decl))
		      for shape in (nthcdr offset (tensor-shape tensor))
		      if (gethash place solved)
			do (when (and (numberp shape) (numberp (gethash place solved)))
			     (assert (= shape (gethash place solved)) () "ShapeTracker: ~a. Invaild Shape Error.~% ~a should be ~a butgot ~a" (st-base st) place (gethash place solved) shape))
		      else do
			(setf (gethash place solved) shape))))
      (assert (every #'identity infinite-part)
	      ()
	      "ShapeTracker: ~~ still contains nil. -> ~a" infinite-part)
      (setf (gethash :~ solved) infinite-part)
      (labels ((find-base-tensor (name)
		 (let ((pos (position name (st-bf st) :key #'at-name)))
		   (assert pos () "ShapeTracker: ~a~% ~a was not appeared in bf." (st-base st) name)
		   (nth pos tensors)))
	       (make-new-tensor (at)
		 (let ((base (find-base-tensor (at-name at)))
		       (shp  (loop for s in (at-shape at)
				   append
				   (let ((o (gethash s solved)))
				     (unless (eql s :~)
				       (assert o () "ShapeTracker: ~a~%~a is not determined." (st-base st) s))
				     (if (listp o) o (list o))))))
		   ;; TODO: Extend views
		   (if allow-broadcast
		       base
		       (make-tensor shp :dtype (tensor-dtype base) :order (tensor-order base) :id (gensym "STC") :views (tensor-views base)
				    :initial-element (gethash :initial-element solved))))))
	(apply #'values (map 'list #'make-new-tensor (st-aft st))))))
  (defun parse-where (where)
    "Verifies the where form"
    (assert (every #'consp where) () "~a: Each element of where must be cons." where)
    (assert (every (compose #'keywordp #'car) where) () "Invaild Syntax: WHERE = (KEYWORD . VALUE)~%in the ~a" where)
    `(list ,@(loop for (key . value) in where  collect `(cons ,key ,value)))))

(defmacro st (st-notation (&rest input-tensors) &rest where)
  "## [macro] st
Based on the notation of ShapeTracker, automatically generate the Tensor after forward.

### Examples

- ElementWise      : A[~] -> A[~]
- Gemm             : A[~ m n] B[~ n k] -> A[~ m k]
- Scalar           : A[] B[~] -> B[~]
- Multiple outputs : A[] B[] -> A[] B[]

### Infinite-Rank Notation

`~` represents an inifinite-rank.
Positioned only in the first rank, and the rank height becomes infinite.
Within this range, if there is a mismatch in shape, broadcasting is automatically applied.
TODO: Add LazyAssertion which applies shape check even for symbols

### Where

- (KEYWORD . VALUE) will specify a special argument to st. It can specify that `keyword` is solved to `value`.
- `VALUE` can be one of number or list (concatenated).

### Special Keywods

- where (:initial-element . any-number) loads the constant `any-number` when creating a tensor.
"
  (declare (type string st-notation))
  (let ((st (%st->list (%parse-st st-notation))))
    `(%solve-st ,st ,(parse-where where) nil ,@input-tensors)))

(defmacro bc (st-notation (&rest input-tensors) &rest where)
  "## [macro] bc
Perform the same operation as `st`, but also doing broadcasting.
It calls !reshape and !view inside, therefore, it must not be used inside the forward method."
  (declare (type string st-notation))
  (let ((st (%st->list (%parse-st st-notation))))
    `(%solve-st ,st ,(parse-where where) t ,@input-tensors)))

(defun broadcast-elwise (a b) (multiple-value-list (bc "A[~] B[~] -> A[~] B[~]" (a b))))
