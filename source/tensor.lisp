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

(defun %lower-iseq (iseq)
  (declare (type list iseq))
  ;; at this level, only graph/function can be coexist <- x
  ;; but (lower) lowers graph into function,
  ;; all functions are lowered into aasm.
  ;; e.g.: Sigmoid should be defined as a graph, neg should be defined as a function
  ;; TODO: Assert that Module is eliminated at this level
  (let ((tensor->id-table (make-hash-table :test #'eql))
	(nodes))
    (flet ((t->id (x) (gethash (tensor-id x) tensor->id-table))
	   (setid (place node) (setf (gethash place tensor->id-table) node)))
      (dolist (tensor iseq)
	;; Assertion: The top of graph starts with no inputs (i.e.: they are always allocation)
	(assert (every #'identity (map 'list #'t->id (func-variables (tensor-op tensor))))
		()
		"Every tensor ~a should be appeared in the graph first. (the top of nodes is not allocation?)"
		(func-variables (tensor-op tensor)))
	(let ((low-graph (apply #'lower (tensor-op tensor) (map 'list #'t->id (func-variables (tensor-op tensor))))))
	  (assert (graph-p low-graph) () "%tensor->asm: lower(~a, ...) should return a graph, butgot ~a" (tensor-op tensor) low-graph)
	  (assert (every #'node-p (graph-nodes low-graph)) () "%tensor->asm: received invaild nodes. all elements should be a node. ~a" low-graph)
	  (assert (>= (length (graph-nodes low-graph)) 1) () "Assertion Failed with (>= (length (graph-nodes low-graph)) 1)")
	  (let ((final (car (last (graph-nodes low-graph)))))
	    (setid (tensor-id tensor) final))
	  (setf nodes (append nodes (graph-nodes low-graph))))))
    (let ((graph (apply #'make-graph nodes)))
      (verify-graph graph)
      graph)))

(defun %tensor->aasm (&rest tensors) (%lower-iseq (apply #'%tpsort-tensors tensors)))
(defun %tensor-backward (tensor)
  (declare (type Tensor tensor))

  )


;; 1. 一旦Module, Backwardだけ実装する
;; 2. %loadを実装 +
;; !reshape/!viewを実装
;; 3. st-levelでBroadcastingを実装
;; 4. BufferってAVMのStructじゃない？AASMへ移動すべき? しなくてもいいか...
;; 5. log1p fusionとか実装する

(defun proceed (&rest tensors)
  "Realizes the tensor"
  (declare (type list tensors))
  ;; 1. Lower
  (let* ((lowered-graph (apply #'%tensor->aasm tensors))
	 ;; 2. Simplify Modules

	 ;; 3. Lower Modules

	 ;; 4. Construct backwards (WIP)
	 )
  ;; 1. Lower the modules -> function

  ;; ModuleのBackward: TmpModuleBWみたいなClassを作る，ClassのSlotにLoweredされたTensorを格納

  ;; 2. Sort functions

  ;; 3. Get pullback

  ;; 4. Get forward lowered asm

  ))
