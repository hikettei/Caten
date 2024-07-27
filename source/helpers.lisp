(in-package :caten)
(defpattern sym (to-what) `(and (type symbol) (satisfies (lambda (x) (equalp (symbol-name x) ,to-what)))))

;; In SBCL, compilation takes longer than 2s.
(defun %tpsort-tensors (seen &rest tensors)
  (declare (type list tensors)
	   (optimize (speed 3)))
  #+sbcl(setf sb-ext:*inline-expansion-limit* 30)
  (let ((top-sort nil))
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
      (values (reverse top-sort) seen))))

(defun ->iconst (x)
  (if (tensor-p x)
      x
      (iconst x)))

(defsimplifier
    (%obtain-fold-constant-result)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value value)
     ->
     (:_TmpScalarConst (value) :dtype dtype)))
    
(defun try-fold-constant (tensor)
  (declare (type Tensor tensor))
  (let ((graph (fold-constant (%tensor->aasm tensor))))
    (when (= (length (graph-nodes graph)) 2)
      (%obtain-fold-constant-result graph)
      (when (and (= (length (graph-nodes graph)) 1)
		 (eql :_TmpScalarConst (node-type (car (graph-nodes graph)))))
	(let ((val (car (node-reads (car (graph-nodes graph))))))
	  (and (numberp val) val))))))

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
