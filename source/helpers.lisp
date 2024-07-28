(in-package :caten)
(defpattern sym (to-what) `(and (type symbol) (satisfies (lambda (x) (equalp (symbol-name x) ,to-what)))))

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
	(let ((val (car (node-reads (car (graph-nodes graph))))))
	  val)))))

(defun zeros-like (tensor)
  "Creates a tensor whose shape is the equivalent to the tensor, but view is reset."
  (declare (type tensor tensor))
  (make-tensor (tensor-shape tensor) :dtype (tensor-dtype tensor) :order (tensor-order tensor) :initial-element 0.0))

(defun clone-like (tensor)
  "Creates a new tensor whose shape/view/stride is completely equivalent to the original one"
  (declare (type tensor tensor))
  (let ((out (copy-tensor tensor)))
    (setf (tensor-id out) (gensym "TID")
	  (tensor-op out) nil
	  (tensor-variables out) nil
	  (tensor-grad out) nil)
    out))

(defun symb (&rest symbols) (intern (with-output-to-string (o) (dolist (s symbols) (princ s o)))))

(defmacro with-no-grad (&body body)
  "## [macro] with-no-grad
Set *no-grad*=t."
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
