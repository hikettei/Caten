(in-package :caten/ajit)
;; Here, applying a jit-specific optimization to the avm.graph.
;; E.g.: we can purge the view nodes since we already have a
;; type information at `type-relay.lisp`.
(defun %safely-purge-views-from-graph (avm)
  "(1.) Removes :VIEW from avm graph, (2.) Updates the read/write graph relations"
  (declare (type avm avm))
  (let ((rewrite-map
	  (loop for node in (graph-nodes (avm-graph avm))
		if (eql (node-type node) :View)
		  collect (cons (car (node-reads node)) (car (node-writes node))))))
    (labels ((->find (id) (find id rewrite-map :key #'car :test #'eql))
	     (view-composed? (id &aux (node (id->value (avm-graph avm) id)))
	       (and node (eql (node-type node) :View)))
	     (->aft (id &aux (last id))
	       (if (symbolp id)
		   (labels ((f (x &aux (next (->find x)))
			      (if next
				  (progn
				    (setf last (cdr next))
				    (if (view-composed? (cdr next))
					(f (cdr next))
					last))
				  nil)))
		     (f last)
		     last)
		   id)))
      (setf (graph-nodes (avm-graph avm))
	    (loop for n in (graph-nodes (avm-graph avm))
		  unless (eql (node-type n) :View)
		    collect
		    (progn		      
		      (setf (node-writes n) (map 'list #'(lambda (x) (or (->aft x) x)) (node-writes n)))
		      n))))))

(defun wmma-relay-from (t1 tc nth)
  (make-inferred-type `(,(nth nth (relay-reads tc)) ,@(relay-reads t1)) (relay-writes tc)))
(defun wmma-relay-from1 (t1 t2 t3)
  (make-inferred-type `(,@(relay-writes t3) ,(second (relay-reads t1)) ,(second (relay-reads t2))) (relay-writes t3))
  (setf (nth 1 (relay-reads t3)) (second (relay-reads t1)))
  t3)
;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b) :_type_relay t1) c) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 1)))
    ((:Add (c (:Mul (a b) :_type_relay t1)) :reduction t :_type_relay t2) -> (:WMMA (c a b) :reduction t :_type_relay (wmma-relay-from t1 t2 0))))
(defsimplifier
    (contiguous-after-wmma :speed 0)
    ((:WMMA (c (:Move (_ a) :_type_relay t1) (:Move (_ b) :_type_relay t2)) :reduction reduction :_type_relay t3) -> (:WMMA (c a b) :reduction reduction :_type_relay (wmma-relay-from1 t1 t2 t3))))

(defun apply-jit-specific-simplifiers (avm)
  (declare (type avm avm))
  (%safely-purge-views-from-graph avm)
  (wmma-rewriter (avm-graph avm) :no-verify t)
  (contiguous-after-wmma (avm-graph avm) :no-verify t))

(defun apply-alias-for-rendering-graph (pipeline avm)
  (declare (type hash-table pipeline) (avm avm))
  (let ((alias-map (make-hash-table :test #'eql))
	(scalars (make-hash-table :test #'eql))
	(remove-id-list))
    (labels ((alias (key value)
	       (setf (gethash key alias-map) value))
	     (load-from-map (key) (or (gethash key alias-map) key))
	     (alias-node (node)
	       (alias (car (node-writes node)) (load-from-map (car (node-reads node))))))
      (loop for graph being the hash-values of pipeline do
	(dolist (node (graph-nodes graph))
	  (case (node-type node)
	    (:Allocate nil)
	    (:WHERE
	     (let ((writes (list (load-from-map (second (node-reads node)))))
		   (reads  (map 'list #'load-from-map (node-reads node))))
	       (alias (car (node-writes node)) (second (node-reads node)))
	       (setf (node-writes node) writes
		     (node-reads node) reads)))
	    (otherwise
	     (if (and
		  (eql (node-type node) :Load)
		  (symbolp (getattr node :value))
		  (let ((alloc (id->value (avm-graph avm) (car (node-reads node)))))
		    (and
		     (eql (node-type alloc) :Allocate)
		     (= (getattr alloc :nrank) 0))))
		 (progn
		   ;; [TODO] BugFix is required to (make-tensor `(10) :initial-element 'a)
		   ;; X <- Alloc(...)
		   ;; Y <- LOAD(X, value=a)
		   (setf (gethash (car (node-reads node)) scalars) (getattr node :value))
		   (alias (car (node-writes node)) (getattr node :value))
		   (push (node-id node) remove-id-list))
		 (when (>= (length (node-writes node)) 1)
		   (assert (= (length (node-writes node)) 1) () "Currently, caten/ajit only supports (length node-writes) == 1.")
		   (let ((writes (list (load-from-map (car (node-reads node)))))
			 (reads (map 'list #'load-from-map (node-reads node))))
		     (alias-node node)
		     (setf (node-writes node) writes
			   (node-reads node) reads))))))))
      (maphash
       #'(lambda (_ g)
	   (declare (ignore _))
	   (setf (graph-nodes g) (loop for n in (graph-nodes g) unless (find (node-id n) remove-id-list) collect n)))
       pipeline)
      (setf (avm-fw-outputs avm) (map 'list #'load-from-map (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'load-from-map (avm-bw-outputs avm)))
      (let ((new-id2tensor (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (load-from-map k) new-id2tensor) v))
	 (avm-id2tensor avm))
	(setf (avm-id2tensor avm) new-id2tensor))
      (let ((new-variables (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (load-from-map k) new-variables) v))
	 (avm-variables avm))
	(setf (avm-variables avm) new-variables)))
    scalars))

(defun apply-static-gensym (avm)
  (declare (type avm avm))
  (let ((alias-table (make-hash-table))
	(val-count 0))
    (labels ((val-gensym (id)
	       (if (symbolp id)
		   (or
		    (gethash id alias-table)
		    (prog1
			(setf (gethash id alias-table) (intern (format nil "val_~a" val-count)))
		      (incf val-count)))
		   id)))
      (dolist (node (graph-nodes (avm-graph avm)))
	(setf (node-writes node) (map 'list #'val-gensym (node-writes node))
	      (node-reads node) (map 'list #'val-gensym (node-reads node))))
      (setf (avm-fw-outputs avm) (map 'list #'val-gensym (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'val-gensym (avm-bw-outputs avm)))
      (let ((new-id2tensor (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (val-gensym k) new-id2tensor) v))
	 (avm-id2tensor avm))
	(setf (avm-id2tensor avm) new-id2tensor))
      (let ((new-variables (make-hash-table)))
	(maphash
	 #'(lambda (k v)
	     (setf (gethash (val-gensym k) new-variables) v))
	 (avm-variables avm))
	(setf (avm-variables avm) new-variables)))))
