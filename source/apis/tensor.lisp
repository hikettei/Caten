(in-package :caten/apis)

(defstruct (Tensor
	    (:constructor %internal-make-tensor (op shape
						 &key
						   (dtype *default-float*) (order *default-order*) (id (gensym "TID"))
						   (variables nil) (views nil) (requires-grad nil))))
  (shape shape :type list)
  (buffer nil :type (or null Buffer))
  (dtype dtype :type dtype-t)
  (order order :type (member :row :column))
  (id id :type symbol)
  (op op :type (or null Func)) ;; Type Func or Module
  (views nil :type list)
  (requires-grad requires-grad :type boolean)
  (grad (when requires-grad (make-tensor shape :dtype dtype :order order :requires-grad nil :id (gensym "GRAD"))) :type (or null Tensor))
  (grad-id (when requires-grad (gensym "TGRAD")) :type symbol)
  (variables variables :type list))

(defun grad (tensor) (tensor-grad tensor))
(defun shape (tensor) (copy-list (tensor-shape tensor)))
(defun ndim (tensor) (length (shape tensor)))
(defun dtype-of (tensor) (tensor-dtype tensor))
(defun order (tensor) (tensor-order tensor))

(defmethod print-object ((tensor Tensor) stream)
  (format stream "{Tensor[~(~a~)] :shape ~a :id ~a
~a
  :op ~a
  :requires-grad ~a
  :variables ~a}"
	  (tensor-dtype tensor)
	  (loop for s in (tensor-shape tensor) collect (if (tensor-p s) (tensor-id s) s))
	  (tensor-id tensor)
	  (if (tensor-buffer tensor)
	      (pprint-buffer (tensor-buffer tensor) :indent 2)
	      "  :buffer nil")
	  (tensor-op tensor)
	  (tensor-requires-grad tensor)
	  (map 'list #'tensor-id (tensor-variables tensor))))

(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (id (gensym "TID")) (requires-grad nil) (initial-element nil) (views nil))
  "## [function] make-tensor
Create a new lazy tensor.
Shape := (Integer > 1) | Symbol | Tensor"
  (declare (type list shape)
	   (type dtype-t dtype)
	   (type (member :column :row) order)
	   (type symbol id)
	   (type (or null number symbol) initial-element))
  (dolist (s shape)
    (assert (or (and (integerp s) (>= s 1)) (tensor-p s) (symbolp s))
	    ()
	    "make-tensor: Cannot initialize a tensor.~%~%Shape should be specified as an integer (>1), tensor, or symbol.~%  Butgot: ~a~%  Shape=~a" s shape))
  (let ((buff (%internal-make-tensor nil shape :dtype dtype :order order :id id :requires-grad requires-grad :views views)))
    ;; Weird thing: The Top of the graph should not have variables.
    ;; AD recognises (null (func-variables op)) as an Allocation.
    ;; So do not modify the (tensor-variables tensor), as well as (func-variables Allocation)
    (setf (tensor-op buff) (make-instance 'Allocate :buffer buff :initial-element initial-element))
    buff))

(defun make-scalar (value &key (dtype *default-float*) (order *default-order*) (id (gensym "SID")) (requires-grad nil))
  (make-tensor nil :dtype dtype :order order :id id :requires-grad requires-grad :initial-element value))

(macrolet ((def (name dtype)
	     `(defun ,name (value &key (dtype ,dtype) (order *default-order*) (id (gensym "SID")) (requires-grad nil))
		(if (tensor-p value)
		    value
		    (make-tensor nil :dtype dtype :order order :id id :requires-grad requires-grad :initial-element value)))))
  (def fconst *default-float*)
  (def uconst *default-uint*)
  (def iconst *default-int*))

(defun make-view-internal (base subscripts &key (allow-merge t) (dtype (tensor-dtype base)) (order (tensor-order base)) (id (gensym "VID")) (stride nil))
  (declare (type Tensor base)
	   (type list subscripts)
	   (type dtype-t dtype)
	   (type (member :row :column) order))    
  (handler-bind
      ((error
	 #'(lambda (c) (error 'caten-forward-error :op 'make-view-internal :inputs (list base) :c c))))
    (flet ((is-broadcast (x) (and (listp x) (eql (car x) :~))))
      
      (let* ((views (merge-views base subscripts allow-merge))
	     (buff (%internal-make-tensor nil (map 'list #'vrange-size views) :dtype dtype :order order :id id :views views))
	     (broadcast-mode-p (some #'is-broadcast subscripts)))
	(when broadcast-mode-p
	  (assert (every #'(lambda (x) (or (is-broadcast x) (eql x t))) subscripts)
		  ()
		  "Do not slice other axes when using broadcast. ~a" subscripts))
	(setf (tensor-variables buff)
	      (append
	       (list base)
	       (map 'list (compose #'sfold #'vrange-size) views)
	       (map 'list (compose #'sfold #'viewrange-from) views)
	       (map 'list (compose #'sfold #'viewrange-to) views)
	       (map 'list (compose #'sfold #'viewrange-by) views)
	       (map 'list (compose #'sfold #'viewrange-size) views)
	       (loop for s in stride collect (if (node-p s) s (iconst s))))
	      (tensor-op buff) (make-instance 'View :views views :broadcast-mode broadcast-mode-p :subscripts subscripts :nrank (length views)))
	(setf (func-variables (tensor-op buff)) (tensor-variables buff))
	(assert (every #'tensor-p (tensor-variables buff)) ())
	;; Fold Constants in Shape (detached from the graph, no side effects)
	(setf (tensor-shape buff) (map 'list #'(lambda (x) (if (tensor-p x) (or (try-fold-constant x) x) x)) (tensor-shape buff)))
	buff))))

