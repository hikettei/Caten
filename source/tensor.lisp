(in-package :caten)
;; Deviceをどの時点で決定する？Tensorに持たせたくはない
;; compile時に渡す方針で不便しないはず？
;; Lazyの途中でPrint Debugできるようにしたい!!!
(defstruct (Tensor
	    (:constructor %internal-make-tensor (shape
						 &key
						   (dtype *default-float*) (order *default-order*) (id (gensym "TID"))
						   (variables nil))))
  (shape shape :type list)
  (buffer nil :type (or null Buffer))
  (dtype dtype :type dtype-t)
  (order order :type (member :row :column))
  (id id :type symbol)
  (variables variables :type list))

;;(defmethod print-object ((tensor Tensor) stream))
(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (id (gensym "TID")))
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
  (%internal-make-tensor shape :dtype dtype :order order :id id))

(defun forward ())

(defun lower ()
  ;; at this level, only graph/function can be coexist.
  ;; but (lower) lowers graph into function,
  ;; all functions are lowered into aasm.
  )

