(in-package :caten)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
		 (loop with tallest = (apply #'max (map 'list #'cdr kranks))
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
			   ((every #'(lambda (x) (eql x 1)) shapes) t)
			   ((some #'(lambda (x) (eql x 1)) shapes)
			    (or
			     (find 1 shapes :test #'(lambda (x y) (and (numberp y) (not (= x y)))))
			     (find 1 shapes :test (compose #'not #'eql))))
			   (T t))))))
	  (flet ((->subscript (tns ignore-last-k)
		   (loop with rank = (length (tensor-shape tns))
			 for s in (tensor-shape tns)
			 for g in shape-goal
			 for i upfrom 0
			 if (and (>= (- rank i) ignore-last-k) (= s 1))
			   collect `(:~ ,g)
			 else collect t)))
	    (loop for tns in aligned-tensors
		  for k in kranks
		  if k
		    collect
		    (let ((ss (->subscript tns (car k))))
		      (if (every #'(lambda (x) (eql t x)) ss)
			  tns
			  (apply #'!view tns ss)))
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
		       (make-tensor shp :dtype (tensor-dtype base) :dtype (tensor-order base) :id (gensym "STC"))))))
	(apply #'values (map 'list #'make-new-tensor (st-aft st)))))))

(defmacro st (st-notation (&rest input-tensors) &rest where)
  "## [macro] st
Based on the notation of ShapeTracker, automatically generate the Tensor after forward.

### Examples

- ElementWise      : A[~] -> A[~]
- Gemm             : A[~ m n] B[~ n k] -> A[~ m k]
- Scalar           : A[] B[~] -> B[~]
- Multiple outputs : A[] B[] -> A[] B[]

### Notation

`~` represents an inifinite-rank.
Positioned only in the first rank, and the rank height becomes infinite.
Within this range, if there is a mismatch in shape, broadcasting is automatically applied.

TODO: Add LazyAssertion which applies shape check even for symbols
"
  (declare (type string st-notation))
  (let ((st (%st->list (%parse-st st-notation))))
    `(%solve-st ,st ',where nil ,@input-tensors)))

(defmacro bc (st-notation (&rest input-tensors) &rest where)
  "## [macro] bc
Perform the same operation as `st`, but also doing broadcasting.
It calls !reshape and !view inside, therefore, it must not be used inside the forward method."
  (declare (type string st-notation))
  (let ((st (%st->list (%parse-st st-notation))))
    `(%solve-st ,st ',where t ,@input-tensors)))

(defun broadcast-elwise (a b) (multiple-value-list (bc "A[~] B[~] -> A[~] B[~]" (a b))))
