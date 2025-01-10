(in-package :caten/api)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; iseq.lisp: Func Level Graph ===> AASM Graph Lowerer.
;; 1. (Sort)     Topologically sorting the tensor graph, getting iseq (a list of tensors)
;; 2. (Func)     Translate from Tensor into caten/air:node, by using the `lower` method.
;; 3. (Optimize) Simplifying the graph with a mixture of :Func and :Module, enabling an ir-level optimization.
;; 4. (Module)   Translate from Tensor into Tensor, by using the `impl` method, proceeding to (2.) until :Module was purged.
;; 5. (Optimize) Simplifying the graph into a level of :Func.
;; 6. (Autodiff) Constructing the backward graph from (1.), lowering them in the same way as (2.) ~ (5.)

;; ~~ Compiler-Session (Utils) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO(hikettei): It is just sorting tensors with adding a mutation rule to the Module, can't we reimplement it in a simple way?
;; Current impl is too complex :( and there's a lot of reuse.
(defstruct (Compiler-Session
	    (:conc-name session-)
	    (:constructor make-compiler-session (&key (name :main))))
  (name name :type keyword) ;; name of this module
  (seen nil :type list)     ;; a list of seen tensor ids
  (write->node (make-hash-table :test #'eql) :type hash-table)  ;; (forward-time) tensor_id -> lowered_node table.
  (grad->tensor (make-hash-table :test #'eql) :type hash-table) ;; grad_id vs grad_tensor table. 
  (tid->tensor (make-hash-table :test #'eql) :type hash-table)  ;; a pair of toplevel_node and lowered_node
  (grad->grads (make-hash-table :test #'eql) :type hash-table)  ;; multi-grad accumulation table.
  (fw-out-ids nil :type list)  ;; list of tensor ids at the end of the graph.
  (bw-out-ids nil :type list)) ;; a list of grads (as well as fw-out-ids)

(defun session/set-tid (session tid tensor)
  (declare (type Compiler-Session session) (type symbol tid) (type tensor tensor))
  (setf (gethash tid (session-tid->tensor session)) tensor))

(defun session/update-outputs (session graph &key (alias))
  "This should occur just after make-graph has happened."
  (declare (type compiler-session session)
	   (type graph graph)
	   (optimize (speed 3)))
  (setf (graph-outputs graph)
	(loop for id in `(,@(session-fw-out-ids session))
	      if (and alias (gethash id alias))
		collect (gethash id alias)
	      else
		append
		(let ((res (session/read session id t)))
		  (and (not (eql res t)) (node-writes res))))
	(graph-outputs graph) (nconc (graph-outputs graph) (session-bw-out-ids session))))

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
	(error "Session/read: The tensor ~a should have appeared in the graph first. (make sure that the top of node is an allocation.)" tid))))

(defun session/setgrad (session tid grad)
  (declare (type Compiler-Session session)
	   (type symbol tid)
	   (type tensor grad))
  (let ((table (session-grad->tensor session)))
    (if (gethash tid table)
        (let ((prev (gethash tid table)))
          (setf (gethash tid table) (!add prev grad)))
        (setf (gethash tid table) grad))))

(defun session/readgrad (session tid)
  (declare (type Compiler-Session session)
	   (type symbol tid))
  (gethash tid (session-grad->tensor session)))

(defun session/set-multi-grad (session grad-id tid alloc)
  (declare (type Compiler-Session session)
	   (type symbol grad-id tid)
	   (optimize (speed 3)))
  ;; cons (top-id, rest-grads)
  (if (gethash grad-id (session-grad->grads session))
      (let ((form (gethash grad-id (session-grad->grads session))))
	(setf (gethash grad-id (session-grad->grads session))
	      (if alloc
		  (list tid (second form))
		  (list (first form) (nconc (list tid) (second form))))))
      (setf (gethash grad-id (session-grad->grads session)) (if alloc (list tid nil) (list nil (list tid))))))

(defun accumulate-grads (subgrads last-id)
  (graph-nodes (with-context (_ (%add (reduce #'%add (butlast subgrads)) (car (last subgrads)) :id last-id)))))

(defun session/sync-multi-grads (session graph)
  (declare (type Compiler-session session)
	   (type graph graph)
	   (optimize (speed 3)))
  (maphash
   #'(lambda (grad-id leaves)
       (multiple-value-bind (final-grad-id rest-grads) (apply #'values leaves)
	 (declare (type list rest-grads))
	 ;; [TODO] Since we intentionally detached the gradient accumulation
	 ;; we got a lot of additional change here to optimize/inline zero_grad/accum_grad
	 ;; e.g.: rewriting ADD -> MOVE
	 (if (= (length rest-grads) 0)
	     (loop for node in (graph-nodes graph)
		   if (and (= (length (node-writes node)) 1) (eql final-grad-id (the symbol (car (node-writes node)))))
		     do (setf (node-writes node) (list grad-id))
			(session/assign session grad-id node))
	     (if (>= (length rest-grads) 2)
		 (let* ((subgrads (map 'list #'(lambda (x) (session/read session x)) rest-grads))
			(subgrad-id (gensym "SUBGRAD"))
			(totals (accumulate-grads subgrads subgrad-id))
                        (old-alloc-bw (id->value graph final-grad-id))
                        (new-alloc-bw (and old-alloc-bw (make-node :BinaryOps :MOVE (list grad-id) (list (car (node-reads old-alloc-bw)) subgrad-id) :reduction t))))
                   (assert (and old-alloc-bw (eql (node-type old-alloc-bw) :ADD) (getattr old-alloc-bw :reduction :allow-undefined t))
                           ()
                           "The leave of backward graph should be :ADD with reduction! but getting ~a" old-alloc-bw)
                   ;; Alloc-bw-old is assumed to the backward of Allocate, and :ADD(reduce=T)
                   (assert (typep graph 'Graph) () "TODO: Add an implementation for FastGraph!")
                   ;; FIXME(hikettei): this is my stupid but remnode with Graph requires node-id while with FastGraph requires node-writes.
                   (remnode graph (node-id old-alloc-bw))
                   (dolist (total totals)
		     (push total (graph-nodes graph)))
		   (push new-alloc-bw (graph-nodes graph))
		   (session/assign session grad-id new-alloc-bw))
		 (let* ((old-alloc-bw (id->value graph final-grad-id)))
                   (assert (and old-alloc-bw (eql (node-type old-alloc-bw) :ADD) (getattr old-alloc-bw :reduction :allow-undefined t))
                           ()
                           "The leave of backward graph should be :ADD with reduction! but getting ~a" old-alloc-bw)
                   ;; Rewrites the direction of old-alloc-bw to the expected leaf of node.
                   (setf (node-writes old-alloc-bw) (list grad-id))
		   (session/assign session grad-id old-alloc-bw))))))
   (session-grad->grads session)))

;; ~~ compilation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun sync-output-to (tensors nodes)
  (assert (= (length tensors) (length nodes)))
  (loop for tensor in tensors
        for node in nodes
        if (= 0 (tensor-nth-output tensor))
          collect node
        else
          collect (make-node-pointing-to-nth node (tensor-nth-output tensor))))

(defun %lower-iseq (session iseq &key (no-verify nil) (simplifiers *external-simplifiers*))
  "Lowers iseq (a list of topologically sorted tensors) into caten/air graph."
  (declare (type compiler-session session)
	   (type list iseq)
	   (optimize (speed 3)))
  (let ((nodes))
    (flet ((t->id (x) (session/read session (tensor-id x))))
      (dolist (tensor iseq)
	;; Assertion: The top of graph starts with no inputs (i.e.: they are always allocation)
	(assert (every #'identity (map 'list #'t->id (func-variables (tensor-op tensor))))
		()
		"Every tensor ~a should be appeared in the graph first. (make sure that the top of nodes is allocation)"
		(func-variables (tensor-op tensor)))
	(let ((low-graph (apply #'lower (tensor-op tensor) (sync-output-to (func-variables (tensor-op tensor)) (map 'list #'t->id (func-variables (tensor-op tensor)))))))
	  (assert (graph-p low-graph) () "%tensor->asm: lower(~a, ...) should return a graph, but got ~a" (tensor-op tensor) low-graph)
	  (assert (every #'node-p (graph-nodes low-graph)) () "%tensor->asm: received invalid nodes. All elements should be a node. ~a" low-graph)
	  (assert (>= (length (the list (graph-nodes low-graph))) 1) () "Assertion Failed with (>= (length (graph-nodes low-graph)) 1)")
          (when simplifiers
            (setf (graph-outputs low-graph) (nconc (graph-outputs low-graph) (copy-list (node-writes (car (last (graph-nodes low-graph))))))
                  (graph-seen low-graph) (apply #'append (graph-seen low-graph) (map 'list (compose #'node-writes #'t->id) (func-variables (tensor-op tensor))))
                  low-graph (->fast-graph low-graph))
            (dolist (f simplifiers) (funcall f low-graph))
            (setf low-graph (->graph low-graph)))
	  (let ((final (car (last (graph-nodes low-graph)))))
	    (session/assign session (tensor-id tensor) final))
	  (setf nodes (nconc nodes (graph-nodes low-graph))))))
    (let ((graph (apply #'make-graph nodes)))
      (unless no-verify (session/update-outputs session graph))
      (unless no-verify (verify-graph graph))
      graph)))

(defun %make-graph-backward (session iseq &key (iseq-bw))
  (declare (type compiler-session session)
	   (type list iseq)
	   (optimize (speed 3)))
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
		 ((or (null (func-variables (tensor-op tensor))) (typep (tensor-op tensor) 'Allocate)) ;; Dynamic shaped allocation will accept variables!
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
		  (let ((bw (%module->iseqbw session (tensor-op tensor) prev-grad)))
		    (and bw (%bwgraph bw))))
		 (T
                  ;; Consider backward (A -> X Y)
                  ;; (assert (= (length next-grads) (length (remove-duplicates next-grads))) ())
		  (loop for next-var in (func-variables (tensor-op tensor))
			for next-grad in next-grads
			if next-grad do
			  (session/setgrad session (tensor-id next-var) next-grad)
			  (when (tensor-grad-id next-var)
			    (session/set-multi-grad session (tensor-grad-id next-var) (tensor-id next-grad) nil))
			  (%bwgraph (%tpsort-tensors session next-grad))))))))
    (mapc #'backward-helper (reverse iseq))
    iseq-bw))

(defun %make-graph-from-iseq (session iseq prev-grad &key (no-grad nil) (external-simplifiers nil) (rewriters nil) (toplevels) (toplevel-ids) (maximum-recursion 1024))
  "Constructs a forward/backward graph based on iseq"
  (declare (type Compiler-Session session)
	   (type list iseq toplevel-ids)
	   (type tensor prev-grad)
	   (type fixnum maximum-recursion)
	   (optimize (speed 3)))
  ;; Inserts toplevel_ids = pause_backward(toplevels)
  (setf (session-fw-out-ids session)
	(nconc (session-fw-out-ids session) (map 'list #'tensor-id toplevels))
	(session-bw-out-ids session)
	(nconc
	 (session-bw-out-ids session)
	 (loop for tensor in iseq
	       if (and (null no-grad) (tensor-requires-grad tensor))
		 collect (tensor-grad-id tensor))))
  (flet ((lower-all (graph)
	   (flet ((ok () (null (find :Module (the list (graph-nodes (->graph graph))) :key #'node-class))))
	     (loop until (ok) for n fixnum upfrom 0 do
               (when (>= n maximum-recursion)
	         (error "%make-graph-from-iseq: maximum-recursion has reached ~a. Make sure that modules have no cycle dependencies." n))
	       ;; e.g.:
	       ;; n=1 Quantize (Matmul) Dequantize -> QMatmul
	       ;; n=1 (Simplify)
	       ;; n=2 QMatmul -> QAdd + SHR + ...
	       ;; n=2 (Simplify)
	       ;;      ...
               (when (= 1 (the fixnum (ctx:getenv :DOT)))
                 (->dot graph :title (format nil "lowerer T=~a" n)))
	       (%lower-modules session graph)
	       ;; Func level whole optimization
	       (dolist (f rewriters) (funcall f graph))))
           (when (= 1 (the fixnum (ctx:getenv :DOT))) (->dot graph :title "lowerer (final)"))))
    (let* ((forward-graph
	     (prog1
		 (->fast-graph (%lower-iseq session iseq))
	       (session/setgrad session (tensor-id (car (last iseq))) prev-grad)))
	   (iseq-bw (when (null no-grad) (%tpsort-tensors session prev-grad)))
	   (pause-backward-p))
      ;; (Forward Mode) First, Simplify the forward graph in :Module/:Func level
      (dolist (f external-simplifiers) (funcall f forward-graph))
      ;; Second, lower an :module into a list of :func
      (lower-all forward-graph) ;; SLOW
      ;; (Backward Mode) First, create a reverse-mode backward tape from the sorted forward graph.
      ;; the tapes consequent after the allocation of prev-grad.
      (when (null no-grad) (setf iseq-bw (%make-graph-backward session iseq :iseq-bw iseq-bw)))
      ;; the backward graph depends on the forward graph (save-for-backward)
      ;; do not simplify or verify them until merging
      ;; fold :module from backward graph
      (let ((backward-graph (when (null no-grad) (%lower-iseq session iseq-bw :no-verify t))))
	;; backward-graph depends on forward-graph, they should not simplified/verified until merged
	(let* ((forward-graph (->graph forward-graph))
	       (merged-graph
		 (apply
		  #'make-graph
		  (nconc
		   (graph-nodes forward-graph)
		   (and
		    (graph-nodes forward-graph)
		    (null no-grad)
		    (setf pause-backward-p t)
		    (list (make-node :Special/VM :Pause/Backward toplevel-ids (list (node->id (car (last (graph-nodes forward-graph))))))))
		   (and backward-graph (graph-nodes backward-graph))))))
	  ;; Rewrite/Optimize f(A) + f(A) grad accumulation
          (when (null no-grad) (session/sync-multi-grads session merged-graph)) ;; Mutate grads to have an output like TGRAD000...
	  ;; If Pause/Backward was generated, use toplevel-ids instead of toplevels because
	  ;; val_1_1 val_2_1 <- pause/backward(val_1, val_2) was generated.
	  (if pause-backward-p
	      (let ((map (make-hash-table)))
		(loop for ti in (map 'list #'tensor-id toplevels)
		      for newti in toplevel-ids
		      do (setf (gethash ti map) newti))
		(session/update-outputs session merged-graph :alias map))
	      (session/update-outputs session merged-graph))
	  (let ((merged-graph (->fast-graph merged-graph)))
            (lower-all merged-graph)
	    ;; Function-level whole optimization
            (dolist (f external-simplifiers) ;; Slow but O(n)
              (funcall f merged-graph :debug-opt (= 1 (the fixnum (ctx:getenv :PROFILE_SIMPLIFIER)))))
	    ;; verify and complete
            (verify-graph merged-graph)
            ;; Finally minify duplicated stride computation subgraph. (forced)
            (setf merged-graph (minimize-duplicated-symbolic-path merged-graph))
	    (values (->graph-with-tpsort merged-graph) pause-backward-p)))))))

;; ~~ module lowering utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %module->iseqfw (session module-node)
  "Lowers the given module.
e.g.:
```
FastGraph[seen=NIL, outputs=(STC23957099)] {
    <ALLOCATE : TID23956501 <- (shape=(30, 10), stride=(10, 1)) where :nrank=2 :dtype=FLOAT32>
    <Node[MODULE] GRAPH/FEEDFORWARD(NID2395222) : STC23957099 <- (TID23956501) where :in-features=10 :out-features=20 :bias=T>
}
```
-> is transformed into:
```
FastGraph[seen=NIL, outputs=(STC23957099)] {
    <ALLOCATE : TID23956501 <- (shape=(30, 10), stride=(10, 1)) where :nrank=2 :dtype=FLOAT32>
    <Node[MODULE] GRAPH/LINEAR(NID23957737) : STC23957437 <- (TID23956501) where :in-features=10 :out-features=20 :bias=T>
    <Node[MODULE] GRAPH/GELU(NID23957738) : STC23957438 <- (STC23957437) where :approx=TANH :metadata=#<GELU {7119B17C43}>>
    <Node[MODULE] GRAPH/LINEAR(NID23957739) : STC23957099 <- (STC23957438) where :in-features=20 :out-features=10 :bias=T>
}
```
The iseq obtained by lowering the Module must match the output destination specified in the (module-outputs :metadata).
"
  (declare (type compiler-session session) (type node module-node))
  (assert (eql :Module (node-class module-node)) ())
  (assert (getattr module-node :metadata) () "~a has lost its metadata. Check simplifier processes." module-node)
  (let* ((module (getattr module-node :metadata))
	 (lowered (multiple-value-list (apply #'impl module (func-variables module)))))
    (setf (module-impl-iseq module) (apply #'%tpsort-tensors session lowered))
    ;; Lower the blueprint into iseq. (seen is global during compiling, no duplications here)
    (let ((lowered-graph (%lower-iseq session (module-impl-iseq module) :no-verify t)))
      (assert (= (length (the list (module-lower-outputs module))) (length (the list (module-outputs module)))) ())
      (assert (= (length (node-writes module-node)) (length (module-outputs module))) ())
      ;; module-outputs: a list of tensors at "FORWARD"    }
      ;; module-lower-outputs: a list of tensors at "IMPL" } they are both used to confirm the validity of shape inference.
      ;; Rewrite output tensor ids in `lowered-graph` to `(node-writes module-node)`
      (let ((lowered-output-ids
	      (remove-duplicates ;; (A A B B) -> (A B)
	       (flatten
		(map
		 'list
		 #'(lambda (x &aux (node (session/read session (tensor-id x))))
		     (assert node () "The tensor ~a is not found when lowering ~a." module-node)
		     (node-writes node))
		 (module-lower-outputs module)))))
	    (alias-map (make-hash-table)))
	;; Multiple Outputs:
	;; Consider lowering a module A: A -> A B
	;; A is defind as K(X) -> A B
	(assert (= (length lowered-output-ids) (length (module-lower-outputs module))) () "The number of outputs does not match when lowering ~a" module-node)
	(loop for output-id-in-final-graph in (node-writes module-node)
	      for output-id-in-lower-graph in lowered-output-ids
	      do (setf (gethash output-id-in-lower-graph alias-map) output-id-in-final-graph))
	(dolist (node (graph-nodes lowered-graph))
	  (flet ((r (x) (if (symbolp x) (or (gethash x alias-map) x) x)))
	    (setf (node-reads node) (map 'list #'r (node-reads node))
		  (node-writes node) (map 'list #'r (node-writes node)))))
        (dolist (n (graph-nodes lowered-graph))
          (when (typep (node-attr n) 'JITAble)
            (push (cons (node-type module-node) (node-id module-node)) (getattr n :_lowering_history))))
	(graph-nodes lowered-graph)))))

(defun %module->iseqbw (session module prev-grad)
  "Module.backward(dout) -> Module.args[0].grad, Module.args[1].grad, ..."
  (declare (type compiler-session session) (type Module module) (type tensor prev-grad) (optimize (speed 3)))
  ;; [TODO] Support multiple outputs of module
  ;; determine whichth output is it
  (when (module-impl-iseq module)
    (dolist (out (module-lower-outputs module)) (session/setgrad session (tensor-id out) prev-grad))
    (%make-graph-backward session (module-impl-iseq module))))

(defmethod %lower-modules ((session Compiler-Session) (graph Graph))
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

(defmethod %lower-modules ((session Compiler-Session) (graph FastGraph))
  (declare (optimize (speed 3)))
  (maphash
   #'(lambda (write node)
       (declare (ignore write))
       (when (node-p node)
	 (when (eql :module (node-class node))
	   (insert-nodes graph (%module->iseqfw session node)))))
   (%graph-nodes-table graph))
  graph)

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *external-simplifiers* `(optimize-aasm) "A list of external simplifiers. (defined by defsimplifier)")
(defparameter *no-grad* nil)
(defun %compile-toplevel (tensors &key (rewriters nil) (no-grad *no-grad*) (external-simplifiers *external-simplifiers*) (name :main))
  (declare (type list tensors))
  (let* ((external-simplifiers `(compose-views-from-graph ,@external-simplifiers))
         (session (make-compiler-session :name name))
	 (iseq (apply #'%tpsort-tensors session tensors))
	 (prev-grad
	   (make-tensor
	    (tensor-shape (car tensors))
	    :dtype (tensor-dtype (car tensors)) :order (tensor-order (car tensors))
	    :id 'prev_grad :initial-element 1))
	 (toplevel-ids (map 'list #'(lambda (x) (intern (format nil "~a_1" (tensor-id x)))) tensors)))
    (multiple-value-bind (graph pause-backward-p)
	(%make-graph-from-iseq
	 session iseq prev-grad
	 :no-grad no-grad :external-simplifiers external-simplifiers
	 :toplevels tensors :toplevel-ids toplevel-ids :rewriters rewriters)
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
	      for toplevel-id in toplevel-ids
	      for sid = (std->lid tid)
	      for tensor = (tid->tensor tid)
	      ;; A pair of {ID in GraphRuntime} {Actual Tensor}
	      if tensor do (session/set-tid session (if pause-backward-p toplevel-id sid) tensor))
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
	;; If the graph was created from FastGraph, the time-series order should be broken.
	;; call verify-graph to sort them.
        (make-runtime
         graph
         :fw-outputs (if pause-backward-p toplevel-ids (map 'list #'std->lid (session-fw-out-ids session)))
         :bw-outputs (when (null no-grad) (map 'list #'std->lid (session-bw-out-ids session)))
         :params (loop for i in iseq
                       if (tensor-requires-grad i)
                         collect i)
         :id2tensor (session-tid->tensor session))))))

(defun caten (tensors
	      &key
                (backend (ctx:getenv :BACKEND))
                (rewriters nil)
		(simplifiers *external-simplifiers*))
  "
```lisp
(caten tensors &key (backend (ctx:getenv :BACKEND)) (rewriters nil) (simplifiers *external-simplifiers*))
```

An entry point for compiling the given tensors, returning GraphRuntime.

- tensor[Tensor|List] toplevel tensors.
- backend[keyword] a keyword of the backend to use. (assumed to be defined by define-backend)
- rewriters[list] a list of graph rewriters called each time the compiler will lower the module.
- simplifiers[list] a list of external simplifiers used in the graph-level compilation (defined by defsimplifier). Pass the function name.
"
  (when (tensor-p tensors)
    (setf tensors (list tensors)))
  (caten/codegen:jit
   (%compile-toplevel tensors :rewriters rewriters :external-simplifiers simplifiers)
   :backend backend))

(defun runtime-set-params (runtime params)
  (cond
    ((null params))
    ((every #'dotted-pair-p params)
     ;; (warn "The format (format runtime `(id . value) ...) will be deprecated in the future. Use (format runtime key1 value1 key2 value2 ...) instead.")
     (loop for (place . value) in params
           for value-buffer = (if (tensor-p value)
                                  (progn
                                    (assert (buffer-value (tensor-buffer value)) () "make-params-table: The tensor ~a is not realized." value)
                                    (tensor-buffer value))
                                  (progn
                                    (assert (and (symbolp place) (or (numberp value) (buffer-p value))) () "make-params-table: Only buffer, tensor, and numbers are allowed.")
                                    value))
           do (setf (gethash place (runtime-variables runtime)) value-buffer)))
    (T
     (assert (= 0 (mod (length params) 2)) () "The number of parameters should be even, getting ~a" params)
     (loop for nth upfrom 0 below (length params) by 2
           for place = (nth nth params)
           for value = (nth (1+ nth) params)
           for value-buffer = (if (tensor-p value)
                                  (progn
                                    (assert (buffer-value (tensor-buffer value)) () "make-params-table: The tensor ~a is not realized." value)
                                    (tensor-buffer value))
                                  (progn
                                    (assert (and (symbolp place) (or (numberp value) (buffer-p value))) () "make-params-table: Only buffer, tensor, and numbers are allowed.")
                                    value))
           do (setf (gethash place (runtime-variables runtime)) value-buffer)))))

(defun runtime-sync-tensors (runtime)
  (declare (type GraphRuntime runtime))
  (maphash
   #'(lambda (k v)
       (let ((var (gethash k (runtime-variables runtime))))
         (when var (setf (tensor-buffer v) var))))
   (runtime-id2tensor runtime)))
  
(defmethod forward ((runtime GraphRuntime) &rest params)
  (runtime-set-params runtime params)
  (runtime-forward runtime)
  (runtime-sync-tensors runtime)
  (flet ((ap (x &aux (tensor (or (gethash x (runtime-id2tensor runtime)))))
	   (assert (tensor-p tensor) () "Forward: Attempted to reference the output variable ~a, but it is not defined in the runtime id2tensor table: ~a~%~a"
	           x (hash-table-keys (runtime-id2tensor runtime)) runtime)
           ;; Note: (forward model), (forward model) ... each outputs may share the same buffer. so the output needs to be copied.
           (when (and (shape tensor) (buffer-value (tensor-buffer tensor)))
             (setf (tensor-buffer tensor) (copy-buffer (tensor-buffer tensor))
                   (buffer-value (tensor-buffer tensor)) (copy-buffer-value runtime (tensor-buffer tensor))))
	   (%apply-proceed tensor)))
    (apply #'values (map 'list #'ap (runtime-fw-outputs runtime)))))

(defmethod backward ((runtime GraphRuntime) &optional prev-dout)
  (assert (null prev-dout) () "(backward GraphRuntime) does not accept prev-dout")
  (runtime-backward runtime)
  (runtime-sync-tensors runtime)
  t)

(defmethod %run ((runtime GraphRuntime) &rest params)
  (runtime-set-params runtime params)
  (runtime-forward runtime)
  (runtime-sync-tensors runtime)
  (flet ((ap (x) (gethash x (runtime-variables runtime))))
    (apply #'values (map 'list #'ap (runtime-fw-outputs runtime)))))

(defun proceed (&rest tensors)
  "
```
(proceed &rest tensors)
```

Compiles the given tensors, returning an evaluated tensors.
"
  (declare (type list tensors))
  (forward (caten tensors)))

(defun %tensor->aasm (&rest tensors)
  (let* ((sess (make-compiler-session :name :tensor->aasm))
         (graph (%lower-iseq sess (apply #'%tpsort-tensors sess tensors))))
    (setf (graph-outputs graph) (map 'list #'(lambda (x) (car (node-writes (session/read sess (tensor-id x))))) tensors))
    graph))
