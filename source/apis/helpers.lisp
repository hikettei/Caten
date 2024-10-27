(in-package :caten/apis)

(defun %tpsort-tensors (session &rest tensors)
  "Destructive to session-seen"
  (declare (type Compiler-Session session)
	   (type list tensors)
	   (optimize (speed 3)))
  #+sbcl(setf sb-ext:*inline-expansion-limit* 30)
  (let ((seen (session-seen session))
	(top-sort nil))
    (declare (type list seen top-sort))
    (labels ((top-sort-helper (v)
	       (unless (find (tensor-id v) seen :key #'tensor-id :test #'eql)
		 (progn
		   (push v seen)
		   (dolist (prev (tensor-variables v))
		     (top-sort-helper prev))
		   (push v top-sort)))))
      #+sbcl(declare (inline top-sort-helper))
      (dolist (tensor tensors) (top-sort-helper tensor))
      (setf (session-seen session) seen)
      (reverse top-sort))))

(defun ->const (x f)
  (if (tensor-p x)
      x
      (funcall f x)))
(defun ->iconst (x) (->const x #'iconst))
(defun ->uconst (x) (->const x #'uconst))
(defun ->fconst (x) (->const x #'fconst))

(defsimplifier
    (%obtain-fold-constant-result)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value value)
     ->
     (:_TmpScalarConst (value) :dtype dtype)))

(defun try-fold-constant (tensor)
  (declare (type Tensor tensor))
  (let ((graph (fold-constant (%tensor->aasm tensor))))
    (when (= (length (graph-nodes graph)) 2)
      (%obtain-fold-constant-result graph :no-verify t)
      (when (and (= (length (graph-nodes graph)) 1)
		 (eql :_TmpScalarConst (node-type (car (graph-nodes graph)))))
	(car (node-reads (car (graph-nodes graph))))))))

(defun sfold (x)
  (if (tensor-p x)
      (let ((val (try-fold-constant x))) (or (and val (iconst val)) x))
      x))

(defun zeros-like (tensor)
  "Creates a tensor whose shape is the equivalent to the tensor, but view is reset."
  (declare (type tensor tensor))
  (make-tensor (tensor-shape tensor) :dtype (tensor-dtype tensor) :order (tensor-order tensor) :initial-element 0.0))

(defun clone-like (tensor)
  "Creates a new tensor whose shape/view/stride is completely equivalent to the original one"
  (declare (type tensor tensor))
  (st "A[~] -> A[~]" (tensor) (:initial-element . 0)))

(defun symb (&rest symbols) (intern (with-output-to-string (o) (dolist (s symbols) (princ s o)))))

(defmacro with-no-grad (&body body)
  "
```
(with-no-grad &body body)
```

Under the scope of `with-no-grad`, the gradient computation is disabled. This parameter should be set during inference, and the compiler will generate a more efficient code."
  `(let ((*no-grad* t)) ,@body))

(defun getattr-from-list (attrs id)
  (declare (type list attrs) (type keyword id))
  (ematch attrs ((property id value) value)))

(defmacro with-attrs (slots module &body body)
  "## [macro] with-attrs
Reads and binds attributes from module.
```
(with-attrs ((axis :axis) (keepdims :keepdims)) module
   ...)
```"
  `(let* (,@(loop for slot in slots
		  for bind = (first slot)
		  for name = (second slot)
 		  collect
		  `(,bind (getattr-from-list (module-attrs ,module) ,name))))
     ,@body))

(defun normalize-axis (x n)
  (declare (type tensor x))
  (assert (integerp n) () "axes should be designed as a number. butgot ~A" n)
  (if (< n 0) (+ (ndim x) n) n))

(defun parse-reduce-axes (x lst)
  (declare (type tensor x))
  (ematch lst
    ((eql t)
     (values (loop for i in (shape x) collect 1) (loop for i in (shape x) collect `(:~ ,i))))
    ((guard axis (numberp axis))
     (let ((axis (normalize-axis x axis))
	   (shape-after (shape x))
	   (view-after  (loop for i in (shape x) collect t)))
       (setf (nth axis shape-after) 1
	     (nth axis view-after) `(:~ ,(nth axis (shape x))))
       (values shape-after view-after)))
    ((list* axes)
     (let ((axes (map 'list #'(lambda (a) (normalize-axis x a)) axes))
	   (shape-after (shape x))
	   (view-after  (loop for i in (shape x) collect t)))
       (dolist (axis axes)
	 (setf (nth axis shape-after) 1
	       (nth axis view-after) `(:~ ,(nth axis (shape x)))))
       (values shape-after view-after)))))

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))

(defun render-attrs (node)
  (let ((attr (dump-into-list (node-attr node) :allow-unbound nil)))
    (if attr
	(with-output-to-string (out)
	  (dolist (k (getattrs node))
	    (when k
	      (format out ", ~(~a~)=~a" k (getattr node k)))))
	"")))

(defun avm-gather-args (avm)
  (declare (type avm avm))
  (remove-duplicates
   (loop for node in (graph-nodes (avm-graph avm))
	 if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
	   collect (getattr node :value))))

(defun print-avm (avm &key (stream t) (n-indent 4) (args nil) &aux (graph (avm-graph avm)))
  "Utils for debugging the graph on the repl."
  (declare (type avm avm))
  (macrolet ((indent (n) `(with-output-to-string (out) (dotimes (i ,n) (princ " " out)))))
    (format
     stream
     "~a"
     (with-output-to-string (out)
       (format out "~%~adefun ~(~a~)(~(~a~))~%" (indent (- n-indent 4)) (avm-name avm) (render-list (append args (reverse (avm-gather-args avm)))))
       (loop for node in (graph-nodes graph)
	     if (eql (node-type node) :Allocate) do
	       (let ((nrank (getattr node :nrank)))
		 (format out "~a~(~a~) = ~(~a~)(shape=(~a), stride=(~a)~a);~%"
			 (indent n-indent)
			 (render-list (node-writes node))
			 (node-type node)
			 (render-list (subseq (node-reads node) 0 nrank))
			 (render-list (subseq (node-reads node) nrank))
			 (render-attrs node)))
	     else if (eql (Node-type node) :View) do
	       (let ((nrank (getattr node :nrank)))
		 (flet ((subseq1p (x y z) (subseq x (1+ y) (1+ z))))
		   (format out "~a~(~a~) = ~(~a~)(~(~a~), shape=(~a), views=(~a), stride=(~a)~a);~%"
			   (indent n-indent)
			   (render-list (node-writes node))
			   (node-type node)
			   (car (node-reads node))
			   (render-list (subseq1p (node-reads node) 0 nrank))
			   (let ((upfrom (subseq1p (node-reads node) nrank (* 2 nrank)))
				 (below (subseq1p (node-reads node) (* 2 nrank) (* 3 nrank)))
				 (by (subseq1p (node-reads node) (* 3 nrank) (* 4 nrank)))
				 (bc (getattr node :broadcast)))
			     (render-list
			      (map 'list #'(lambda (x y z l) (format nil "(~a)" (render-list (list x y z l)))) upfrom below by bc)))
			   (render-list (subseq1p (node-reads node) (* 4 nrank) (* 5 nrank)))
			   (if (getattr node :permute)
			       (format nil ", permute=~a" (getattr node :permute))
			       ""))))
	     else
	       do (format out "~a~(~a~)~a~(~a~)(~(~a~)~a);~%" (indent n-indent) (render-list (node-writes node)) (if (node-writes node) " = " "") (node-type node) (render-list (node-reads node)) (render-attrs node)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun collect-initargs-names (args)
    (loop for a in args
	  ;; (f a &aux ...)
	  if (and (symbolp a) (not (eql (aref (symbol-name a) 0) #\&)))
	    collect a
	  else if (listp a) collect (car a))))

(defun pad-left (&rest shape)
  (let ((max-dim (apply #'max (map 'list #'length shape))))
    (mapcar #'(lambda (s) (append (make-list (- max-dim (length s)) :initial-element 1) s)) shape)))

(defun column-major-calc-strides (shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element 1)))
    (loop for i from 1 to (- num-dims 1) do
      (setf (nth i strides) (* (nth (- i 1) strides) (nth (- i 1) shape))))
    strides))

(defun row-major-calc-strides (shape)
  (declare (type list shape))
  (let* ((num-dims (length shape))
         (strides (make-list num-dims :initial-element 1)))
    (loop for i downfrom (- num-dims 2) to 0 do
      (setf (nth i strides) (* (nth (+ i 1) strides) (nth (+ i 1) shape))))
    strides))

(defun static-compute-strides (order shape)
  (ecase order
    (:row (row-major-calc-strides shape))
    (:column (column-major-calc-strides shape))))

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

(defmethod permute-list ((op Permute) list)
  (loop for nth in (permute-order op)
	collect (nth nth list)))
