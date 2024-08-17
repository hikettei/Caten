(in-package :caten/apis)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; iseq.lisp transforms the graph of Tensor into caten/aasm graph by the following steps:
;; 1. (Sort)     Topologically sorting the tensor graph, getting iseq (a list of tensors)
;; 2. (Func)     Translate from Tensor into caten/air:node, by using the `lower` method.
;; 3. (Optimize) Simplifying the graph with a mixture of :Func and :Module, enabling an ir-level optimization.
;; 4. (Module)   Translate from Tensor into Tensor, by using the `impl` method, proceeding to (2.) until :Module was purged.
;; 5. (Optimize) Simplifying the graph in a level of :Func.
;; 6. (Autodiff) Constructing the backward graph from (1.), lowering them in the same way as (2.) ~ (5.)
;; ~~ Compiler-Session (Utils) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Compiler-Session
	    (:conc-name session-)
	    (:constructor make-compiler-session (&key (name :main))))
  (name name :type keyword) ;; name of this module
  (seen nil :type list)     ;; a list of seen tensor ids
  (write->node (make-hash-table :test #'eql) :type hash-table)  ;; tensor_id vs lowered_node table
  (grad->tensor (make-hash-table :test #'eql) :type hash-table) ;; grad_id vs grad_tensor table. 
  (tid->tensor (make-hash-table :test #'eql) :type hash-table)  ;; a pair of toplevel_node and lowered_node
  (grad->grads (make-hash-table :test #'eql) :type hash-table)  ;; multi-grad accumlation table.
  (fw-out-ids nil :type list)  ;; list of tensor ids at the end of the graph.
  (bw-out-ids nil :type list)) ;; a list of grads (as well as fw-out-ids)

(defun session/set-tid (session tid tensor)
  (declare (type Compiler-Session session) (type symbol tid) (type tensor tensor))
  (setf (gethash tid (session-tid->tensor session)) tensor))

(defun session/update-outputs (session graph)
  "This should occur just after make-graph was happened."
  (declare (type compiler-session session)
	   (type graph graph))
  (setf (graph-outputs graph)
	(loop for id in `(,@(session-fw-out-ids session))
	      append
	      (let ((res (session/read session id t)))
		(and (not (eql res t)) (node-writes res))))
	(graph-outputs graph) (append (graph-outputs graph) (session-bw-out-ids session))))

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

(defun session/set-multi-grad (session grad-id tid alloc)
  (declare (type Compiler-Session session)
	   (type symbol grad-id tid))
  ;; cons (top-id, rest-grads)
  (if (gethash grad-id (session-grad->grads session))
      (let ((form (gethash grad-id (session-grad->grads session))))
	(setf (gethash grad-id (session-grad->grads session))
	      (if alloc
		  (list tid (second form))
		  (list (first form) (append (list tid) (second form))))))
      (setf (gethash grad-id (session-grad->grads session)) (if alloc (list tid nil) (list nil (list tid))))))

(defun session/sync-multi-grads (session graph)
  (declare (type Compiler-session session)
	   (type graph graph))
  (maphash
   #'(lambda (grad-id leaves)
       (multiple-value-bind (final-grad-id rest-grads) (apply #'values leaves)
	 ;; [TODO] Since we intentionally detached the gradient accumlation
	 ;; we got a lot of additional change here to optimize/inline zero_grad/accum_grad
	 ;; e.g.: rewriting ADD -> MOVE
	 (if (= (length rest-grads) 0)
	     (loop for node in (graph-nodes graph)
		   if (and (= (length (node-writes node)) 1) (eql final-grad-id (car (node-writes node))))
		     do (setf (node-writes node) (list grad-id))
			(session/assign session grad-id node))
	     (let* ((subgrads (map 'list #'(lambda (x) (session/read session x)) rest-grads))
		    (subgrad-id (gensym "SUBGRAD"))
		    (total (if (>= (length subgrads) 2)
			       (let ((node
				       (make-node :BinaryOps :ADD (list subgrad-id) (map 'list #'node->id subgrads) :reduction nil)))
				 (push node (graph-nodes graph))
				 node)
			       (car subgrads))))
	       (let ((final-node
		       ;; TODO: Fuse them
		       (make-node :BinaryOps :MOVE (list grad-id) (list final-grad-id (node->id total)))))
		 (push final-node (graph-nodes graph))
		 (session/assign session grad-id final-node))))))
   (session-grad->grads session)))
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
	     (let ((next-grads
		     (handler-bind ((error #'(lambda (cond) (error 'caten-backward-error :c cond :inputs prev-grad :op (tensor-op tensor)))))
		       (multiple-value-list (backward (tensor-op tensor) prev-grad)))))
	       (cond
		 ((null (func-variables (tensor-op tensor)))
		  ;; The op is an allocation, the top of node.		   
		  (assert (= (length next-grads) 1)
			  ()
			  "%make-graph-backward: If Node ~a has no variables, then backward should return only one Tensor."
			  (tensor-op tensor))
		  (assert (typep (tensor-op tensor) 'Allocate) () "Expected to be an allocation? (it is safe to remove this assertion ig)")
		  (session/set-multi-grad session (tensor-grad-id (alloc-buffer (tensor-op tensor))) (alloc-id (tensor-op tensor)) t)
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
			  (when (tensor-grad-id next-var)
			    (session/set-multi-grad session (tensor-grad-id next-var) (tensor-id next-grad) nil))
			  (%bwgraph (%tpsort-tensors session next-grad))))))))
    (mapc #'backward-helper (reverse iseq))
    iseq-bw))

(defun %make-graph-from-iseq (session iseq prev-grad &key (no-grad nil) (external-simplifiers nil) (toplevels) (maximum-recursion 100))
  "Constructs a forward/backward graph based on iseq"
  (declare (type Compiler-Session session)
	   (type list iseq)
	   (type tensor prev-grad))
  (setf (session-fw-out-ids session)
	(append (session-fw-out-ids session) (map 'list #'tensor-id toplevels))
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
    ;; (Forward Mode) First, simplify the forward graph in :Module/:Func level
    (dolist (f external-simplifiers) (funcall f forward-graph))
    ;; Second, lower an :module into a list of :func
    (%lower-modules session forward-graph)
    ;; Finally, simplifying all the lowered :func. (ir-level optimization is all with regard to forward)
    (dolist (f external-simplifiers) (funcall f forward-graph))
    ;; (Backward Mode) First, create a reverse-mode backward tape from the sorted forward graph.
    ;; the tapes consequent after the allocation of prev-grad.
    (when (null no-grad) (setf iseq-bw (%make-graph-backward session iseq :iseq-bw iseq-bw)))
    ;; the backward graph depends on the forward graph (save-for-backward)
    ;; do not simplify or verify them until merging
    ;; fold :module from backward graph
    (let ((backward-graph (when (null no-grad) (%lower-iseq session iseq-bw :no-verify t))))
      ;; backward-graph depends on forward-graph, they should not simplified/verified until merged
      (let ((merged-graph
	      (apply
	       #'make-graph
	       (append
		(graph-nodes forward-graph)
		(and
		 (graph-nodes forward-graph)
		 (null no-grad)
		 (list (make-node :Special/VM :Pause/Backward nil (list (node->id (car (last (graph-nodes forward-graph))))))))
		(and backward-graph (graph-nodes backward-graph))))))
	;; Rewrite/Optimize f(A) + f(A) grad accumlation
	(when (null no-grad) (session/sync-multi-grads session merged-graph))
	(session/update-outputs session merged-graph)
	;; Function-level whole optimization
	(dolist (f external-simplifiers) (funcall f merged-graph))
	;; Lower the :module if remained.
	(flet ((ok () (null (find :Module (graph-nodes merged-graph) :key #'node-class))))
	  (loop until (ok) for n upfrom 0 do
	    (when (>= n maximum-recursion)
	      (error "%make-graph-from-iseq: maximum-recursion has reached ~a. Make sure that modules have no cycle dependencies." n))
	    (%lower-modules session merged-graph)
	    ;; Func level whole optimization
	    (dolist (f external-simplifiers) (funcall f merged-graph))))
	;; verify and complete
	(verify-graph merged-graph)
	merged-graph))))
;; ~~ module lowering utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %module->iseqfw (session module)
  (declare (type compiler-session session) (type node module))
  (assert (eql :Module (node-class module)) ())
  (assert (getattr module :metadata) () "~a has lost its metadata. (-> check simplifier)" module)
  (%module-obj->iseqfw session (getattr module :metadata)))

(defun %module-obj->iseqfw (session module)
  (declare (type compiler-session session)
	   (type module module))
  (let* ((lowered (multiple-value-list (apply #'impl module (func-variables module)))))
    (setf (module-impl-iseq module) (apply #'%tpsort-tensors session lowered))
    (let ((nodes (graph-nodes (%lower-iseq session (module-impl-iseq module) :no-verify t))))
      (assert (= (length (module-lower-outputs module)) (length (module-outputs module))) ())
      (loop with tgt = (map
			'list
			#'(lambda (x)
			    (car (node-writes (session/read session x))))
			(map 'list #'tensor-id (module-lower-outputs module)))
	    with src = (map 'list #'tensor-id (module-outputs module))
	    for n in nodes
	    collect
	    (progn
	      (setf (node-writes n) (map 'list #'(lambda (x &aux (p (position x tgt :test #'eql))) (if p (nth p src) x)) (node-writes n)))
	      n)))))

(defun %module->iseqbw (session module prev-grad)
  "Module.backward(dout) -> Module.args[0].grad, Module.args[1].grad, ..."
  (declare (type compiler-session session) (type Module module) (type tensor prev-grad))
  ;; [TODO] Support multiple outputs of module
  ;; determine whichth output is it
  (when (null (module-impl-iseq module))
    (%module-obj->iseqfw session module))
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
(defparameter *external-simplifiers* `(optimize-aasm))
(defparameter *no-grad* nil)
(defun %compile-toplevel (tensors &key (no-grad *no-grad*) (external-simplifiers *external-simplifiers*) (name :main))
  (declare (type list tensors))
  (let* ((session (make-compiler-session :name name))
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
      (loop for id in (session-bw-out-ids session)
	    do (assert (some #'(lambda (x) (find id (node-writes x))) (graph-nodes graph))
		       ()
		       "%compile-toplevel: The tensor ~a where :requires-grad=t could not be differentiated because backward was broken." id)))
    (flet ((std->lid (x) (car (node-writes (session/read session x))))
	   (tid->tensor (x) (find x iseq :key #'tensor-id :test #'eql))
	   (tid->tensor-grad (x) (find x iseq :key #'tensor-grad-id :test #'eql)))
      ;; creating a pair of vm_var -> tensor
      (loop for tid in (session-fw-out-ids session)
	    for sid = (std->lid tid)
	    for tensor = (tid->tensor tid)
	    ;; A pair of {ID in AVM} {Actual Tensor}
	    if tensor do (session/set-tid session sid tensor))
      ;; as well as backward
      (when (null no-grad)
	(loop for tid in (session-bw-out-ids session)
	      for sid = (std->lid tid)
	      for tensor = (tid->tensor-grad tid)
	      if tensor do (session/set-tid session sid (tensor-grad tensor))))
      ;; nothing to compute? -> alloc
      (when (null (graph-nodes graph))
	(setf (graph-nodes graph)
	      (with-context-nodes
		  (_ (loop for tid in (session-fw-out-ids session)
			   for sid = (std->lid tid)
			   for tensor = (tid->tensor tid)
			   do (%make-tensor (tensor-shape tensor) :dtype (tensor-dtype tensor) :order (tensor-order tensor) :id sid))))))
      (make-avm graph (session-name session)
		(session-tid->tensor session)
		(map 'list #'std->lid (session-fw-out-ids session))
		(when (null no-grad) (map 'list #'std->lid (session-bw-out-ids session)))))))

(defun caten (tensors
	      &key
		(jit (= 1 (ctx:getenv :JIT)))
		(name (intern (symbol-name (gensym "MAIN")) "KEYWORD"))
		(simplifiers *external-simplifiers*)) ;; TODO disassemble options etc
  "Compiles the (Abstract) tensor"
  (when (tensor-p tensors)
    (setf tensors (list tensors)))
  (let ((avm (%compile-toplevel tensors :name name :external-simplifiers simplifiers)))
    (if jit (caten/ajit:jit avm) avm)))

(defun avm/sync-tensors (avm)
  "Synchronize buffer and tensor (but limited to the end of nodes, and grads)"
  (declare (type caten/avm:avm avm))
  (maphash
   #'(lambda (k v)
       (let ((var (gethash k (avm-variables avm))))
	 (when var
	   (setf (tensor-buffer v) var))))
   (avm-id2tensor avm)))

(defmethod forward ((avm caten/avm:AVM) &rest params)
  (let ((*device* (ctx:getenv :AVM)))
    (vm/set-params avm params)
    (vm/forward avm)
    (avm/sync-tensors avm)
    (apply #'values (map 'list #'(lambda (x) (%apply-proceed (gethash x (avm-id2tensor avm)))) (avm-fw-outputs avm)))))

(defmethod backward ((avm caten/avm:AVM) &optional prev-dout)
  (declare (ignore prev-dout))
  (let ((*device* (ctx:getenv :AVM)))
    (vm/backward avm)
    (avm/sync-tensors avm)
    t))

(defun proceed (&rest tensors)
  "Realizes the tensor"
  (declare (type list tensors))
  (forward (caten tensors)))

(defun %tensor->aasm (&rest tensors)
  (let ((sess (make-compiler-session :name :tensor->aasm)))
    (%lower-iseq sess (apply #'%tpsort-tensors sess tensors))))
