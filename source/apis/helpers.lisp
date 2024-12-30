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
  (when (tensor-cache-canonicalize tensor)
    (return-from try-fold-constant (tensor-cache-canonicalize tensor)))
  (let ((graph (fold-constant (->fast-graph (%tensor->aasm tensor)))))
    (when (= (length (graph-nodes graph)) 2)
      (%obtain-fold-constant-result graph)
      (when (and (= (length (graph-nodes graph)) 1)
		 (eql :_TmpScalarConst (node-type (car (graph-nodes graph)))))
	(let ((val (car (node-reads (car (graph-nodes graph))))))
          (setf (tensor-cache-canonicalize tensor) val)
          val)))))

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

(defun normalize-axes (x axes)
  (if (listp axes) (map 'list #'(lambda (n) (normalize-axis x n)) axes) (list (normalize-axis x axes))))

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

(defmethod permute-list ((op list) list) (loop for nth in op collect (nth nth list)))

(defun sym-eql (a b)
  (if (and (tensor-p a) (tensor-p b))
      (or
       (eql a b)
       (let* ((g1 (with-no-grad (tensor-lowered-graph a)))
              (g2 (with-no-grad (tensor-lowered-graph b)))
              (g1 (caten/codegen/expr:make-expr :graph g1 :out (car (last (graph-nodes g1)))))
              (g2 (caten/codegen/expr:make-expr :graph g2 :out (car (last (graph-nodes g2))))))
         ;; Note(hikettei) this could be ridiculously slow if the shape is determined by the tensor!
         ;; Especially in the ViT Graph
         (caten/codegen/expr:expr-scalar-equivalent-p g1 g2)))
      (equal a b)))

(defun sym-equal (a b)
  (declare (type list a b))
  (and (= (length a) (length b)) (every #'sym-eql a b)))
