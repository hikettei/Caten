(in-package :caten)

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
  (variables variables :type list))

(defmethod print-object ((tensor Tensor) stream)
  (format stream "{Tensor[~(~a~)] :shape ~a :id ~a
  :buffer ~a
  :op ~a
  :requires-grad ~a
  :variables ~a}"
	  (tensor-dtype tensor)
	  (loop for s in (tensor-shape tensor) collect (if (tensor-p s) (tensor-id s) s))
	  (tensor-id tensor)
	  (tensor-buffer tensor)
	  (tensor-op tensor)
	  (tensor-requires-grad tensor)
	  (map 'list #'tensor-id (tensor-variables tensor))))

(defun make-tensor (shape &key (dtype *default-float*) (order *default-order*) (id (gensym "TID")) (requires-grad nil) (initial-element nil))
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
  (let ((buff (%internal-make-tensor nil shape :dtype dtype :order order :id id :requires-grad requires-grad)))
    (setf (tensor-op buff) (make-instance 'Allocate :buffer buff :initial-element initial-element))
    buff))

(macrolet ((def (name dtype)
	     `(defun ,name (value &key (dtype ,dtype) (order *default-order*) (id (gensym "SID")) (requires-grad nil))
		(if (tensor-p value)
		    value
		    (make-tensor nil :dtype dtype :order order :id id :requires-grad requires-grad :initial-element value)))))
  (def fconst *default-float*)
  (def uconst *default-uint*)
  (def iconst *default-int*))

(defun make-view-internal (base subscripts &key (dtype (tensor-dtype base)) (order (tensor-order base)) (id (gensym "VID")) (stride nil))
  (declare (type Tensor base)
	   (type list subscripts)
	   (type dtype-t dtype)
	   (type (member :row :column) order))
  (handler-bind
      ((error
	 #'(lambda (c) (error 'caten-forward-error :op 'make-view-internal :inputs (list base) :c c))))
    (let* ((views (merge-views base subscripts))
	   (buff (%internal-make-tensor nil (map 'list #'vrange-size views) :dtype dtype :order order :id id :views views)))
      (setf (tensor-variables buff)
	    (append
	     (list base)
	     (map 'list #'vrange-size views)
	     (loop for v in views collect (viewrange-from v))
	     (loop for v in views collect (viewrange-to v))
	     (loop for v in views collect (viewrange-by v))
	     (loop for s in (tensor-shape base) collect (if (node-p s) s (iconst s)))
	     (loop for s in stride collect (if (node-p s) s (iconst s))))
	    (tensor-op buff) (make-instance 'View :views views :nrank (length views)))
      (setf (func-variables (tensor-op buff)) (tensor-variables buff))
      (assert (every #'tensor-p (tensor-variables buff)) ())
      ;; Fold Constants in Shape (detached from the graph, no side effects)
      (setf (tensor-shape buff) (map 'list #'(lambda (x) (or (and (not (tensor-p x)) x) (try-fold-constant x) x)) (tensor-shape buff)))
      buff)))

;; ~~ compilations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %lower-iseq (iseq &key prev-table (no-verify nil))
  (declare (type list iseq))
  ;; at this level, only graph/function can be coexist <- x
  ;; but (lower) lowers graph into function,
  ;; all functions are lowered into aasm.
  ;; e.g.: Sigmoid should be defined as a graph, neg should be defined as a function
  ;; TODO: Assert that Module is eliminated at this level
  (let ((tensor->id-table (or prev-table (make-hash-table :test #'eql)))
	(nodes))
    (flet ((t->id (x) (gethash (tensor-id x) tensor->id-table))
	   (setid (place node) (setf (gethash place tensor->id-table) node)))
      (dolist (tensor iseq)
	;; Assertion: The top of graph starts with no inputs (i.e.: they are always allocation)
	(assert (every #'identity (map 'list #'t->id (func-variables (tensor-op tensor))))
		()
		"Every tensor ~a should be appeared in the graph first. (make sure that the top of nodes is allocation)"
		(func-variables (tensor-op tensor)))
	(let ((low-graph (apply #'lower (tensor-op tensor) (map 'list #'t->id (func-variables (tensor-op tensor))))))
	  (assert (graph-p low-graph) () "%tensor->asm: lower(~a, ...) should return a graph, butgot ~a" (tensor-op tensor) low-graph)
	  (assert (every #'node-p (graph-nodes low-graph)) () "%tensor->asm: received invaild nodes. all elements should be a node. ~a" low-graph)
	  (assert (>= (length (graph-nodes low-graph)) 1) () "Assertion Failed with (>= (length (graph-nodes low-graph)) 1)")
	  (let ((final (car (last (graph-nodes low-graph)))))
	    (setid (tensor-id tensor) final))
	  (setf nodes (append nodes (graph-nodes low-graph))))))
    (let ((graph (apply #'make-graph nodes)))
      (unless no-verify (verify-graph graph))
      (values graph tensor->id-table))))

(defun %tensor->aasm (&rest tensors) (%lower-iseq (apply #'%tpsort-tensors nil tensors)))
(defun %graph->pullback (prev-table seen prev-grad iseq fw-graph)
  (declare (type list iseq seen)
	   (type hash-table prev-table)
	   (type tensor prev-grad))
  (let ((iseq-bw)
	(grads (make-hash-table :test #'eql)))
    (setf (gethash (tensor-id (car (last iseq))) grads) prev-grad)
    (multiple-value-bind (iseq-bw-tmp seen-new) (%tpsort-tensors seen prev-grad)
      (setf iseq-bw iseq-bw-tmp
	    seen seen-new))
    (labels ((helper (tensor)
	       ;; TODO: Error handling at here
	       (let* ((prev-grad (gethash (tensor-id tensor) grads))
		      (vgrads (when prev-grad (multiple-value-list (backward (tensor-op tensor) prev-grad)))))
		 ;; TODO: Module Backward
		 (when prev-grad
		   (if (null (func-variables (tensor-op tensor)))
		       (when (car vgrads)
			 (assert (= (length vgrads) 1) () "Assertion Failed: The top of node backward should return a single tensor")
			 (multiple-value-bind (iseq-bw-tmp seen-new) (%tpsort-tensors seen (car vgrads))
			   (setf seen seen-new
				 iseq-bw (append iseq-bw iseq-bw-tmp))))
		       (if (and (= (length vgrads) 1) (eql (car vgrads) :module/skip-bw))
			   nil;;(error "TODO: Module Backward")
			   (loop for v in (func-variables (tensor-op tensor))
				 for g in vgrads
				 if g do
				   (multiple-value-bind (iseq-bw-tmp seen-new) (%tpsort-tensors seen g)			    
				     (setf seen seen-new
					   (gethash (tensor-id v) grads) g
					   iseq-bw (append iseq-bw iseq-bw-tmp))))))))))
      (loop for tensor in (reverse iseq) do (helper tensor)))
    (multiple-value-bind (bw-graph bw-table) (%lower-iseq iseq-bw :prev-table prev-table :no-verify t)
      (maphash #'(lambda (k v) (setf (gethash k prev-table) v)) bw-table)
      ;; Simplifier
      (dolist (f *external-simplifiers*) (funcall f fw-graph))
      (let ((merged-graph
	      (apply
	       #'make-graph
	       (append
		(graph-nodes (%lower-modules fw-graph seen prev-table))
		(list (make-node :Special/VM :Pause/Backward nil (list (node->id (car (last (graph-nodes fw-graph)))))))
		(graph-nodes bw-graph)))))
	(dolist (f *external-simplifiers*) (funcall f merged-graph))
	(let ((merged-graph (%lower-modules merged-graph seen prev-table)))
	  (verify-graph merged-graph)
	  (values merged-graph seen prev-table))))))

(defun %module->iseqfw (module seen table)
  (declare (type node module) (type list seen) (type hash-table table))
  (assert (eql :Module (node-class module)) ())
  (assert (getattr module :metadata) () "~a has lost its metadata." module)
  (let* ((op (getattr module :metadata))
	 (lowered
	   (multiple-value-list
	    (apply #'impl op (func-variables op)))))
    (multiple-value-bind (iseq new-table)
	(%lower-iseq (apply #'%tpsort-tensors seen lowered) :prev-table table :no-verify t)
      (maphash #'(lambda (k v) (setf (gethash k table) v)) new-table)
      (graph-nodes iseq))))

(defun %module->iseqbw (module seen prev-table grad-table)
  (declare (type node module)
	   (type list seen)
	   (type hash-table prev-table grad-table))
  
  )

(defun %lower-modules (base-graph seen involved-tensors)
  (declare (type graph base-graph)
	   (type list seen)
	   (type hash-table involved-tensors))
  (let ((new-graph
	  (map 'list
	       #'(lambda (x)
		   (if (eql :module (node-class x))
		       (%module->iseqfw x seen involved-tensors)
		       x))
	       (graph-nodes base-graph))))
    (setf (graph-nodes base-graph) (flatten new-graph))
    (verify-graph base-graph)
    base-graph))

(defparameter *external-simplifiers* `(fold-constant))
(defparameter *no-grad* nil)
(defun proceed (&rest tensors)
  "Realizes the tensor"
  (declare (type list tensors))
  ;; lowered-graph = :func :graph is mixed
  (multiple-value-bind (iseq seen) (apply #'%tpsort-tensors nil tensors)
    (multiple-value-bind (lowered-graph involved-tensors) (%lower-iseq iseq)
      ;; Construct backwards      
      (let* ((prev-grad (make-tensor (tensor-shape (car tensors))
				     :dtype (tensor-dtype (car tensors)) :order (tensor-order (car tensors))
				     :id 'prev-grad :initial-element 1)))
	(let ((fw/bw-graph
		(if *no-grad*
		    (values lowered-graph seen involved-tensors)
		    (%graph->pullback involved-tensors seen prev-grad iseq lowered-graph))))
	  (dolist (f *external-simplifiers*)
	    (funcall f fw/bw-graph))
	  fw/bw-graph)))))

;; 1. 一旦Module, Backwardだけ実装する
;; 2. %loadを実装 + ok          (OK)
;; !where, logicals, castを実装 (OK)
;; absを実装                     (OK)
;; -> 1. broadcast, (fconst 1)を許容する (OK
;; absのconstant foldingを実装 (OK)
;; !reshape/!viewを実装 (OK)
;; Scalar Constant Folding ok (OK)
;; - implement backward
;; - implement frontend proceed
;; - tests
;; ある程度できたらModule/Backward/Functionのテストを実装
;; 3. st-levelでBroadcastingを実装 (OK)
;; 5. log1p fusionとか実装する
;; weightの初期状態をどうやって表現する？
;; testing:
;;   - make-tensor w/ initial-element
;;   - backward test
;;   - broadcast testing
;;   - scalar / matrix testing
