(in-package :caten)
;; Deviceをどの時点で決定する？Tensorに持たせたくはない
;; compile時に渡す方針で不便しないはず？
;; Lazyの途中でPrint Debugできるようにしたい!!!
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
  (op op) ;; Type Func or Module
  (views nil :type list)
  (requires-grad requires-grad :type boolean)
  (grad (when requires-grad (make-tensor shape :dtype dtype :order order :requires-grad nil)) :type (or null Tensor))
  (variables variables :type list))

;; (defafunc Allocation :fw :Allocate :bw accumlate-gradients
;;(defmethod print-object ((tensor Tensor) stream))
(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (id (gensym "TID")) (requires-grad nil))
  "Create a new lazy tensor.
Shape := (Integer > 1) | Symbol | Tensor"
  (declare (type list shape)
	   (type dtype-t dtype)
	   (type (member :column :row) order)
	   (type symbol id))
  (dolist (s shape)
    (assert (or (and (integerp s) (>= s 1)) (tensor-p s) (symbolp s))
	    ()
	    "make-tensor: Cannot initialize a tensor.~%~%Shape should be specified as an integer (>1), tensor, or symbol.~%  Butgot: ~a~%  Shape=~a" s shape))
  (let ((buff (%internal-make-tensor nil shape :dtype dtype :order order :id id :requires-grad requires-grad)))
    (setf (tensor-op buff) (make-instance 'Allocate :buffer buff))
    buff))

(defun %tensor-lower (tensor)
  (declare (type Tensor tensor))
  ;; at this level, only graph/function can be coexist.
  ;; but (lower) lowers graph into function,
  ;; all functions are lowered into aasm.
  ;; e.g.: Sigmoid should be defined as a graph, neg should be defined as a function
  )

(defun %tensor-backward (tensor)
  (declare (type Tensor tensor))

  )
