(in-package :caten/ajit)
;; JIT-specific simplifiers
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
