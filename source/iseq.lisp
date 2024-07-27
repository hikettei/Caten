(in-package :caten)

;;
;; 1. Lowerer from Tensor to AASM
;; 2. AD Engine
;;

;; ~~ Compiler Session ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Compiler-Session
	    (:conc-name session-)
	    (:constructor make-compiler-session (&key (name :main))))
  (name name :type keyword)
  (seen nil :type list)
  (write->node (make-hash-table :test #'eql) :type hash-table)
  (grad->tensor (make-hash-table :test #'eql) :type hash-table)
  (fw-out-ids nil :type list)
  (bw-out-ids nil :type list))

(defun session/update-outputs (session graph)
  "This should occur just after make-graph was happened."
  (declare (type compiler-session session)
	   (type graph graph))
  (setf (graph-outputs graph)
	(loop for id in `(,@(session-fw-out-ids session) ,@(session-bw-out-ids session))
	      append
	      (let ((res (session/read session id t)))
		(and (not (eql res t)) (node-writes res))))))

(defun session/assign (session tid node)
  (declare (type Compiler-Session session)
	   (type symbol tid)
	   (type node node))
  (let ((table (session-write->node session)))
    (when (gethash tid table) (warn "Session/assign: overwriting ~a with ~a" tid node))
    (setf (gethash tid table) node)))

(defun session/read (session tid &optional default)
  (declare (type Compiler-Session session)
	   (type symbol tid))
  (let ((table (session-write->node session)))
    (or (gethash tid table)
	default
	(error "Session/read: The tensor ~a should be appeared in the graph first. (make sure that the top of node is an allocation.)" tid))))

(defun session/setgrad (session tid grad)
  (declare (type Compiler-Session session)
	   (type symbol tid)
	   (type tensor grad))
  (let ((table (session-grad->tensor session)))
    (setf (gethash tid table) grad)))

(defun session/readgrad (session tid)
  (declare (type Compiler-Session session)
	   (type symbol tid))
  (gethash tid (session-grad->tensor session)))
;; ~~ compilations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %lower-iseq (session iseq &key (no-verify nil))
  "Lowers iseq (a list of topologically sorted tensors) into caten/air graph."
  (declare (type compiler-session session)
	   (type list iseq))
  (let ((nodes))
    (flet ((t->id (x) (session/read session (tensor-id x))))
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
	    (session/assign session (tensor-id tensor) final))
	  (setf nodes (append nodes (graph-nodes low-graph))))))
    (let ((graph (apply #'make-graph nodes)))
      (unless no-verify (session/update-outputs session graph))
      (unless no-verify (verify-graph graph))
      graph)))

(defun %make-graph-backward (session iseq &key (iseq-bw))
  (declare (type compiler-session session)
	   (type list iseq))
  (labels ((%bwgraph (nodes)
	     (declare (type list nodes))
	     (assert (every #'tensor-p nodes) ())
	     (setf iseq-bw (append iseq-bw nodes)))
	   (backward-helper (tensor &aux (prev-grad (session/readgrad session (tensor-id tensor))))
	     (declare (type Tensor tensor))
	     (when (null prev-grad) (return-from backward-helper))
	     ;; TODO: Error handling at here
	     (let ((next-grads (multiple-value-list (backward (tensor-op tensor) prev-grad))))
	       (cond
		 ((null (func-variables (tensor-op tensor)))
		  ;; The op is an allocation, the top of node.		   
		  (assert (= (length next-grads) 1)
			  ()
			  "%make-graph-backward: If Node ~a has no variables, then backward should return only one Tensor."
			  (tensor-op tensor))
		  (assert (typep (tensor-op tensor) 'Allocate) () "Expected to be an allocation? (it is safe to remove this assertion ig)")
		  (when (car next-grads) (%bwgraph (%tpsort-tensors session (car next-grads)))))
		 ((and (= (length next-grads) 1) (eql (car next-grads) :module/skip-bw))
		  ;; Module whose backward = nil (i.e.: autodiff from impl)
		  (assert (subtypep (type-of (tensor-op tensor)) 'Module) () "Only modules are allowed to return :module/skip-bw option in backward.
~a is not a module." (tensor-op tensor))
		  ;; Module.backward(prev-grad) -> Module.args_0.grad, Module.args_1.grad, ...
		  (%bwgraph (%module->iseqbw session (tensor-op tensor) prev-grad)))
		 (T
		  (loop for next-var in (func-variables (tensor-op tensor))
			for next-grad in next-grads
			if next-grad do
			  (session/setgrad session (tensor-id next-var) next-grad)
			  (%bwgraph (%tpsort-tensors session next-grad))))))))
    (mapc #'backward-helper (reverse iseq))
    iseq-bw))

(defun %make-graph-from-iseq (session iseq prev-grad &key (no-grad nil) (external-simplifiers nil) (toplevels))
  "Constructs a forward/backward graph based on iseq"
  (declare (type Compiler-Session session)
	   (type list iseq)
	   (type tensor prev-grad))
  (setf (session-fw-out-ids session) (append (session-fw-out-ids session) (map 'list #'tensor-id toplevels))
	(session-bw-out-ids session)
	(append
	 (session-bw-out-ids session)
	 (loop for tensor in iseq
	       if (tensor-requires-grad tensor)
		 collect (tensor-grad-id tensor))))
  (let* ((forward-graph
	   (prog1
	       (%lower-iseq session iseq)
	     (session/setgrad session (tensor-id (car (last iseq))) prev-grad)))
	 (iseq-bw (when (null no-grad) (%tpsort-tensors session prev-grad))))
    ;; First, simplify forward graph module level
    (dolist (f external-simplifiers) (funcall f forward-graph))
    ;; lower them into func level.
    ;; And simplify lowered graph
    (%lower-modules session forward-graph)
    (dolist (f external-simplifiers) (funcall f forward-graph))
    ;; Construct backward
    (when (null no-grad)
      (setf iseq-bw (%make-graph-backward session iseq :iseq-bw iseq-bw)))
    (let ((backward-graph (when (null no-grad) (%lower-iseq session iseq-bw :no-verify t))))
      ;; backward-graph depends on forward-graph, they should not simplified/verified until merged
      (assert (graph-nodes forward-graph) ())
      (let ((merged-graph
	      (apply
	       #'make-graph
	       (append
		(graph-nodes forward-graph)
		(list (make-node :Special/VM :Pause/Backward nil (list (node->id (car (last (graph-nodes forward-graph)))))))
		(graph-nodes backward-graph)))))
	(session/update-outputs session merged-graph)
	;; Graph Level whole optimization
	(dolist (f external-simplifiers) (funcall f merged-graph))
	;; Lower
	(%lower-modules session merged-graph)
	;; Func level whole optimization
	(dolist (f external-simplifiers) (funcall f merged-graph))
	;; verify and complete
	(verify-graph merged-graph)
	merged-graph))))
;; ~~ module lowering utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %module->iseqfw (session module)
  (declare (type compiler-session session) (type node module))
  (assert (eql :Module (node-class module)) ())
  (assert (getattr module :metadata) () "~a has lost its metadata. (-> check simplifier)" module)
  (let* ((op (getattr module :metadata))
	 (lowered (multiple-value-list (apply #'impl op (func-variables op)))))
    (setf (module-impl-iseq op) (apply #'%tpsort-tensors session lowered))
    (let ((nodes (graph-nodes (%lower-iseq session (module-impl-iseq op) :no-verify t))))
      (assert (= (length (module-lower-outputs op)) (length (module-outputs op))) ())
      (loop with tgt = (map
			'list
			#'(lambda (x)
			    (car (node-writes (session/read session x))))
			(map 'list #'tensor-id (module-lower-outputs op)))
	    with src = (map 'list #'tensor-id (module-outputs op))
	    for n in nodes
	    collect
	    (progn
	      (setf (node-writes n) (map 'list #'(lambda (x &aux (p (position x tgt :test #'eql))) (if p (nth p src) x)) (node-writes n)))
	      n)))))

(defun %module->iseqbw (session module prev-grad)
  "Module.backward(dout) -> Module.args[0].grad, Module.args[1].grad, ..."
  (declare (type compiler-session session) (type Module module) (type tensor prev-grad))
  (assert (module-impl-iseq module) () "First, lower this: ~a" module)
  ;; [TODO] Support multiple outputs of module
  ;; determine whichth output is it
  (dolist (out (module-lower-outputs module)) (session/setgrad session (tensor-id out) prev-grad))
  (%make-graph-backward session (module-impl-iseq module)))

(defun %lower-modules (session graph)
  "Lowers all modules existing in the graph until they are disappeared."
  (declare (type graph graph)
	   (type compiler-session session))
  (let ((new-graph
	  (map 'list
	       #'(lambda (x)
		   (if (eql :module (node-class x))
		       (%module->iseqfw session x)
		       x))
	       (graph-nodes graph))))
    (setf (graph-nodes graph) (flatten new-graph))
    (verify-graph graph)
    graph))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *external-simplifiers* `(fold-constant))
(defparameter *no-grad* nil)
(defun %compile-toplevel (tensors &key (no-grad *no-grad*) (external-simplifiers *external-simplifiers*))
  (declare (type list tensors))
  (let* ((session (make-compiler-session :name :main))
	 (iseq (apply #'%tpsort-tensors session tensors))
	 (prev-grad
	   (make-tensor (tensor-shape (car tensors))
			:dtype (tensor-dtype (car tensors)) :order (tensor-order (car tensors))
			:id 'prev-grad :initial-element 1))
	 (graph (%make-graph-from-iseq
		 session iseq prev-grad
		 :no-grad no-grad :external-simplifiers external-simplifiers
		 :toplevels tensors)))
    (when (null no-grad)
      (loop for bwid in (session-bw-out-ids session)
	    for id = (car (node-writes (session/read session bwid)))
	    do (assert (some #'(lambda (x) (find id (node-writes x))) (graph-nodes graph))
		       ()
		       "%compile-toplevel: The tensor ~a where :requires-grad=t could not be differentiated because backward was broken." id)))
    (flet ((std->lid (x) (car (node-writes (session/read session x)))))
      (make-avm graph (session-name session)
		(map 'list #'std->lid (session-fw-out-ids session))
		(map 'list #'std->lid (session-bw-out-ids session))))))

(defun caten (tensors) ;; TODO: :disasseble option etc
  "Compiles the tensor"
  (when (tensor-p tensors)
    (setf tensors (list tensors)))
  (%compile-toplevel tensors))

(defmethod forward ((avm caten/avm:AVM) &rest params)
  (vm/set-params avm params)
  (vm/forward avm))

(defmethod backward ((avm caten/avm:AVM) prev-dout)
  (declare (ignore prev-dout))
  (vm/backward avm))

(defun proceed (&rest tensors)
  "Realizes the tensor"
  (declare (type list tensors))
  (vm/forward (%compile-toplevel tensors :no-grad *no-grad* :external-simplifiers *external-simplifiers*)))

;; 1. 一旦Module, Backwardだけ実装する
;; 2. %loadを実装 + ok          (OK)
;; !where, logicals, castを実装 (OK)
;; absを実装                     (OK)
;; -> 1. broadcast, (fconst 1)を許容する (OK)
;; absのconstant foldingを実装 (OK)
;; !reshape/!viewを実装 (OK)
;; Scalar Constant Folding ok (OK)
;; backwardのrequire-gradのprune
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
(defun %tensor->aasm (&rest tensors)
  (let ((sess (make-compiler-session :name :tensor->aasm)))
    (%lower-iseq sess (apply #'%tpsort-tensors sess tensors))))

