(in-package :caten/ajit)
;; Here, applying a jit-specific optimization to the avm.graph.
;; E.g.: we can purge the view nodes since we already have a
;; type information at `type-relay.lisp`.
;; TODO: Refactor around typing relay (esp: memory-write dependencies are messing)
;; ~~ Step1, Before Grouping Process ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %safely-purge-views-from-graph (avm)
  (declare (type avm avm))
  (let ((rewrite-map (make-hash-table)))
    (loop for node in (graph-nodes (avm-graph avm))
	  if (eql (node-type node) :View) do
	    (setf (gethash (car (node-writes node)) rewrite-map) (car (node-reads node))))
    (labels ((r (id)
	       (if (and (gethash id rewrite-map) (not (eql (gethash id rewrite-map) id)))
		   (r (gethash id rewrite-map))
		   id)))
      (setf (graph-nodes (avm-graph avm))
	    (loop for n in (graph-nodes (avm-graph avm))
		  unless (eql (node-type n) :View)
		    collect
		    (progn
		      (setf (node-reads n) (map 'list #'r (node-reads n))
			    (node-writes n) (map 'list #'r (node-writes n)))
		      n)))
      (setf (avm-fw-outputs avm) (map 'list #'r (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'r (avm-bw-outputs avm)))
      (macrolet ((renew (accessor)
		   `(let ((new-table (make-hash-table)))
		      (maphash
		       #'(lambda (k v)
			   (setf (gethash (r k) new-table) v))
		       ,accessor)
		      (setf ,accessor new-table))))
	(renew (avm-id2tensor avm))
	(renew (avm-variables avm))))))

(defun wmma-relay-from (t1 tc nth)
  (make-inferred-type `(,(nth nth (relay-reads tc)) ,@(relay-reads t1)) (relay-writes tc)))
(defun wmma-relay-from-contiguous (t1 t2 t3)
  (let ((a-base (second (relay-reads t3)))
	(a (second (relay-reads t1)))
	(b-base (third (relay-reads t3)))
	(b (second (relay-reads t2)))
	(c (car (relay-reads t3))))
    (flet ((manual-infer (from to)
	     ;; from = broadcasted and contiguous-applied buffer
	     ;; to = before the `from` operation buffer.
	     (let ((from (copy-buffer from)))
	       (assert (= (buffer-nrank from) (buffer-nrank to)) () "(JIT+WMMA Simplifier) Cannot infer the type of buffers from different ranks.~%There's something wrong with the higher-level shape inferencing processes?~%From: ~a~%To: ~a" from to)
	       (assert (eql (buffer-dtype from) (buffer-dtype to)))
	       (setf (buffer-stride from) (buffer-stride to)
		     (buffer-inferred-permute from) (buffer-inferred-permute to)
		     ;; merge views (use broadcasted one)
		     (buffer-views from) (loop for n upfrom 0 below (buffer-nrank from)
					       if (and (nth n (buffer-views from)) (nth n (buffer-views to)))
						 collect (if (fourth (nth n (buffer-views from)))
							     (nth n (buffer-views from))
							     (nth n (buffer-views to)))
					       else
						 collect (or (nth n (buffer-views from))
							     (nth n (buffer-views to)))))
	       from)))
      (make-inferred-type `(,c ,(manual-infer a-base a) ,(manual-infer b-base b)) (relay-writes t3)))))
;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b) :_type_relay t1) c) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 1)))
    ((:Add (c (:Mul (a b) :_type_relay t1)) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 0))))
(defsimplifier
    (contiguous-after-wmma :speed 0)
    ((:WMMA (c (:Move (_ a) :_type_relay t1) (:Move (_ b) :_type_relay t2)) :reduction reduction :_type_relay t3)
     ->
     (:WMMA (c a b) :reduction reduction :_type_relay (wmma-relay-from-contiguous t1 t2 t3))))

;; ~~ Load Pointer Simplification ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod create-scalar-replace-pattern ((graph graph))
  "
Enumerates the following pattern:
```
*val_1 = 1.0;
val_2[0+0+...+0] = *val_1;
out[...] = f(val_2[0+0...+0]);
```
Storing to the dict in a format of:
```
Key:   val_2
Value: 1.0
```
val_2 is broadcasted, but equivalent to using a scalar number, Simplifying the scheduling process.
"
  (flet ((pattern1 (val_2)
	   (declare (type node val_2))
	   (let* ((val_2 (if (eql (node-type val_2) :MOVE) val_2 nil))
		  (val_1 (when val_2 (id->value graph (second (node-reads val_2)))))
		  (val_1 (if (and val_1 (eql (node-type val_1) :LOAD)) val_1 nil)))
	     (when (and val_1 val_2
			;; val_2[0+0+0+...] <- write access pattern is all broadcasted
			(every
			 #'(lambda (x) (eql x 1))
			 (buffer-shape (car (relay-writes (read-type-relay val_2)))))
			(every
			 #'null
			 (buffer-views (car (relay-writes (read-type-relay val_2)))))
			(= 0 (buffer-nrank (car (relay-writes (read-type-relay val_1)))))
			(eql (buffer-dtype (car (relay-writes (read-type-relay val_1))))
			     (buffer-dtype (car (relay-writes (read-type-relay val_2))))))
	       (values (car (node-writes val_2)) val_1))))
	 (pattern2 (val_2)
	   (declare (type node val_2))
	   (let* ((val_2 (if (eql (node-type val_2) :MOVE) val_2 nil))
		  (val_1 (when val_2 (id->value graph (second (node-reads val_2)))))
		  (val_1 (if (and val_1 (not (eql (node-type val_1) :LOAD))) val_1 nil)))
	     (when (and val_1 val_2
			;; val_2[0+0+0+...] <- write access pattern is all broadcasted
			(every
			 #'(lambda (x) (eql x 1))
			 (buffer-shape (car (relay-writes (read-type-relay val_2)))))
			(every
			 #'null
			 (buffer-views (car (relay-writes (read-type-relay val_2)))))
			(= 0 (buffer-nrank (car (relay-writes (read-type-relay val_1)))))
			(eql (buffer-dtype (car (relay-writes (read-type-relay val_1))))
			     (buffer-dtype (car (relay-writes (read-type-relay val_2))))))
	       (values (car (node-writes val_2)) val_1)))))
    (let ((output (make-hash-table)))
      (dolist (node (graph-nodes graph))
	(dolist (pattern (list #'pattern1 #'pattern2))
	  (multiple-value-bind (key value) (funcall pattern node)
	    (when (and key value)
	      (setf (gethash key output) value)))))
      output)))

(defmethod propagate-rebundant-loadp ((graph graph))
  "Rewrites the pattern:
```
*val_1 = 1.0;
val_2[0+0+...+0] = *val_1;
out[...] = f(val_2[0+0...+0]);
```
Into
```
*val_1 = 1.0;
val_2[0+0+...+0] = *val_1;
out[...] = f(*val_1);
```
`"
  (let ((patterns (create-scalar-replace-pattern graph)))
    (setf (graph-nodes graph)
	  (loop for node in (graph-nodes graph)
		for read-new = (map 'list #'(lambda (x) (gethash x patterns)) (node-reads node))
		collect
		(progn
		  (setf (getattr node :_reads_old_for_multiexpr) (node-reads node)
			(node-reads node)
			(loop for r in (node-reads node)
			      for n in read-new
			      for nth upfrom 0
			      if n
				collect
				(progn
				  (setf (nth nth (relay-reads (read-type-relay node))) (car (relay-writes (read-type-relay n))))
				  (if (and (eql (node-type n) :LOAD)
					   (numberp (getattr n :value)))
				      (getattr n :value)
				      (car (node-writes n))))
			      else
				collect r))
		  node)))))

(defun apply-jit-specific-simplifiers (avm)
  "A toplevel for jit-specific optimizers. (WMMA Simplification, Removing Views, Contiguous Node Removals)"
  (declare (type avm avm))
  (%safely-purge-views-from-graph avm)
  (wmma-rewriter (avm-graph avm) :no-verify t)
  (contiguous-after-wmma (avm-graph avm) :no-verify t)
  (propagate-rebundant-loadp (avm-graph avm)))
;; ~~ Step2, Before Applying Polyhedral Compiler (pipeline w/ DOMAIN)  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Under the step2, some nodes have the following attributes:
;; - :_loop_bound_nodes      : a list of nodes used to compute the bound
;; - :_loop_bound_nodes_type : types for _loop_bound_types
(defun get-parent-nodes (node graph &aux (seen nil))
  (declare (type node node) (type graph graph))
  (labels ((explore (node graph)
	     (when (null (find (node-id node) seen))
	       (push (node-id node) seen)
	       (append
		;; :Allocate is placed in the VM-level
		;; nodes used to compute the shape is never used!
		(when (not (eql (node-type node) :Allocate))
		  (loop for r in (append (node-reads node) (getattr node :_loop_bound_nodes))
			if (and (symbolp r) (id->value graph r)) append (explore (id->value graph r) graph)))
		(list node)))))
    (explore node graph)))

(defun create-reduction-alias-f (nodes)
  (let ((table (make-hash-table)))
    (labels ((ref (x)
	       (if (gethash x table)
		   (ref (gethash x table))
		   x)))
      (loop for node in nodes
	    if (getattr node :reduction :allow-undefined t)
	      do (setf (gethash (car (node-writes node)) table) (car (node-reads node)))
	    else
	      do (setf (node-reads node) (map 'list #'ref (node-reads node))))
      #'ref)))
