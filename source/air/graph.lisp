(in-package :caten/air)

(defstruct (Graph
	    (:constructor make-graph (&rest nodes)))
  "nodes: t=0 ... t=n-1"
  (nodes nodes :type list))
(defun graph-collect-if (graph f)
  (declare (type graph graph)
	   (type function f))
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (funcall f node) collect node)))
(defun id->users (graph id) (graph-collect-if graph #'(lambda (node) (find id (node-reads node) :test #'eql))))
(defun id->value (graph id)
  (let ((result (graph-collect-if graph #'(lambda (node) (find id (node-writes node) :test #'eql)))))
    (assert (<= (length result) 1) () "write rules must be immutable. ~a" graph)
    (car result)))
(defun id->node (graph id)
  (declare (type graph graph)
	   (optimize (speed 3)))
  (find id (graph-nodes graph) :test #'eql :key #'node-id))
(defun remnode (graph id)
  (declare (type graph graph)
	   (type symbol id)
	   (optimize (speed 3)))
  (setf (graph-nodes graph)
	(loop for node in (graph-nodes graph)
	      unless (eql id (node-id node)) collect node)))

(defun verify-graph (graph)
  "Verify the consistency of the graphs and simplify them by operating following:
- Checks if all variables are immutable
- All read dependencies are appearedin writes.
- Purge all isolated graph
- Sort by the time
- TODO: verify-graph is called multiple times during compilation, needs optimized more."
  (declare (type graph graph)
	   (optimize (speed 3)))
  ;; Variables are immutable
  ;; Slow O(n^2) in the worst case.
  (loop for node in (graph-nodes graph)
	do (id->value graph (node-id node)))
  ;; All variables in reads must be appeared in writes
  (let ((undefined))
    (loop for node in (graph-nodes graph)
	  do (dolist (read (node-reads node))
	       (when (symbolp read)
		 (when (null (id->value graph read))
		   (push read undefined)))))
    (assert (null undefined)
	    ()
	    "verify-graph: these symbols are undefined. ~a~%~a" undefined graph))
  ;; Purge all isolated graph
  (purge-isolated-graph graph)
  ;; Sort the graph by time-series.
  ;;(graph-reorder-by-time graph)
  t)

(defun purge-isolated-graph (graph)
  "assuming the last graph is the final output, prunes the isolated graph"
  (declare (type graph graph) (optimize (speed 3)))
  (when (graph-nodes graph)
    (let ((output (node-writes (car (last (graph-nodes graph))))))
      (setf (graph-nodes graph)
	    (loop for n in (graph-nodes graph)
		  collect
		  (if (every #'(lambda (w) (and (symbolp w) (= 0 (length (the list (id->users graph w)))) (not (find w output)))) (node-writes n))
		      nil n))
	    (graph-nodes graph)
	    (loop for n in (graph-nodes graph) if n collect n)))))

;;(defun graph-reorder-by-time (graph)
;;  (declare (type graph graph)
;;	   (optimize (speed 3)))
;;  (when (null (graph-nodes graph)) (return-from graph-reorder-by-time))
  ;; Starting from the last indexed node. Assume this is the main graph
  ;; and all the other graph are subgraph (to be purged)
;;  (let ((toplevel (node-writes (car (last (graph-nodes graph))))))
    
;;    ))
