(in-package :caten/ajit)
;; Here, applying a jit-specific optimization to the avm.graph.
;; E.g.: we can purge the view nodes since we already have a
;; type information at `type-relay.lisp`.
;;
(defun %purge-views-from-schedule (avm)
  "Remove :VIEW from avm.graph"
  (declare (type avm avm))
  (let ((rewrite-map
	  (loop for node in (graph-nodes (avm-graph avm))
		if (eql (node-type node) :View)
		  collect (cons (car (node-reads node)) (car (node-writes node))))))
    (labels ((->find (id) (find id rewrite-map :key #'car :test #'eql))
	     ;; todo: optimize
	     (->aft (id &aux (last id))
	       (labels ((f (x &aux (next (->find x))) (if next (progn (setf last (cdr next)) (f (cdr next))) nil)))
		 (f last))
	       last))
      (setf (graph-nodes (avm-graph avm))
	    (loop for n in (graph-nodes (avm-graph avm))
		  unless (eql (node-type n) :View)
		    collect
		    (progn
		      (setf (node-writes n) (map 'list #'(lambda (x) (or (->aft x) x)) (node-writes n)))
		      n))))))
;; WMMA (c a b) <=> c = c + a * b (:reduction)
(defsimplifier
    (wmma-rewriter :speed 0)
    ((:Add ((:Mul (a b)) c) :reduction t) -> (:WMMA (c a b) :reduction t))
    ((:Add (c (:Mul (a b))) :reduction t) -> (:WMMA (c a b) :reduction t)))

(defsimplifier
    (contiguous-after-wmma :speed 0)
    ((:WMMA (c (:Move (_ a)) (:Move (_ b))) :reduction reduction) -> (:WMMA (c a b) :reduction reduction)))

(defun apply-jit-specific-simplifiers (avm)
  (declare (type avm avm))
  (%purge-views-from-schedule avm)
  (wmma-rewriter (avm-graph avm) :no-verify t)
  (contiguous-after-wmma (avm-graph avm) :no-verify t))

(defun apply-alias-for-rendering-graph (pipeline)
  (declare (type hash-table pipeline))
  (let ((alias-map (make-hash-table :test #'eql)))
    (labels ((alias (key value)
	       (setf (gethash key alias-map) value))
	     (load-from-map (key) (or (gethash key alias-map) key))		 
	     (alias-node (node)
	       (alias (car (node-writes node)) (load-from-map (car (node-reads node))))))
      (loop for graph being the hash-values of pipeline do
	(dolist (node (graph-nodes graph))
	  (case (node-type node)
	    (:Allocate nil)
	    (otherwise
	     (when (>= (length (node-writes node)) 1)
	       (assert (= (length (node-writes node)) 1) () "Currently, caten/ajit only supports (length node-writes) == 1.")

	       (let ((writes (list (load-from-map (car (node-reads node)))))
		     (reads (map 'list #'load-from-map (node-reads node))))
		 (alias-node node)
		 (setf (node-writes node) writes
		       (node-reads node) reads))))))))))

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
      (maphash
       #'(lambda (k v)
	   (setf (gethash (val-gensym k) (avm-id2tensor avm)) v))
       (avm-id2tensor avm))
      (maphash
       #'(lambda (k v)
	   (setf (gethash (val-gensym k) (avm-variables avm)) v))
       (avm-variables avm)))))

