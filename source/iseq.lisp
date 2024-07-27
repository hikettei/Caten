(in-package :caten)

;;
;; 1. Lowerer from Tensor to AASM
;; 2. AD Engine
;;

;; ~~ AD Helper ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Fake-Module-backward (Func)
  ((module :initarg :module :type Module)))

(defmethod lower ((op Fake-Module-Backward) &rest inputs)
  ;; Lower(Fake-Module-Backward) := moduleのBackwardのiseqを入手する
  (print inputs)
  (make-graph))

;; ~~ compilations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO: !! Refactor (defstruct compiler-session
(defstruct (Compiler-Session
	    (:constructor make-compile-session ()))
  (id->tensor (make-hash-table))
  (id->grad   (make-hash-table))
  (seen nil :type list))

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
			   (let ((dummy-grads (%module->iseqbw (tensor-op tensor) seen prev-table grads prev-grad)))
			     (loop for v in (func-variables (tensor-op tensor))
				   for g in dummy-grads do
				     (setf (gethash (tensor-id v) grads) g
					   iseq-bw (append iseq-bw (list g)))))					 
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

(defun %module->iseqbw (module seen prev-table grad-table prev-grad)
  "Module.backward(dout) -> Module.args[0].grad, Module.args[1].grad, ..."
  (declare (type Module module)
	   (type tensor prev-grad)
	   (type list seen)
	   (type hash-table prev-table grad-table))
  ;; 一旦仮のBW Graph Tensorを作成する (variableの数だけ)
  ;; Shape Inferenceは走らせられる
  ;; Viewの情報がわからないが，Variableと同じ状態と仮定して良さそう？
  (loop for v in (func-variables module)
	collect
	(let* ((g (clone-like v))
	       (fake-bw (make-instance 'Fake-Module-Backward :module module)))
	  (setf (tensor-op g) fake-bw
		(tensor-variables g) (list prev-grad)
		(func-variables fake-bw) (list prev-grad))
	  g)))

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
	  ;; さっきのhash-tableはVM実行時/AJIT生成時に使うから取っておく
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


