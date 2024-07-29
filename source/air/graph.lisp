(in-package :caten/air)

(defstruct (Graph
	    (:constructor make-graph (&rest nodes)))
  "nodes: t=0 ... t=n-1
outputs: a list of ids where sorting is starting from.
If outputs is nil, the writes of last nodes becomes the top"
  (nodes nodes :type list)
  (outputs nil :type list))
;; TODO: inline id->users/id->values after improving the root alogirhtm
(declaim (ftype (function (graph (or symbol number)) (or null node list)) id->users id->value))
(defun id->users (graph id)
  (declare (type graph graph)
	   (optimize (speed 3)))
  (if (not (symbolp id))
      nil
      (loop for node in (graph-nodes graph)
	    if (find id (node-reads node) :test #'eql)
	      collect node)))
(defun id->value (graph id)
  (declare (type graph graph))
  (if (not (symbolp id))
      nil
      (loop for node in (graph-nodes graph)
	    if (find id (node-writes node) :test #'eql)
	      do (return-from id->value node))))
(defun id->node (graph id)
  (declare (type graph graph) (optimize (speed 3)))
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
- TODO: verify-graph is called multiple times during compilation, needs optimized more.
- Nodes whose class are start with special/ cannot be purged even if they are isolated."
  (declare (type graph graph)
	   (optimize (speed 3)))
  ;; [TODO] Remove duplicated writes
  ;; Remove duplicated writes
  
;;  (setf (graph-nodes graph)
;;	(reverse
;;	 (loop with seen = nil
;;	       for node in (reverse (graph-nodes graph))
;;	       if (null (find (the symbol (car (node-writes node))) seen))
;;		 collect (progn (push (car (node-writes node)) seen) node)))) 
  ;; Purge all isolated graph
  (resolve-isolated-nodes graph)
  (purge-isolated-graph graph)  
  t)

(defun special-p (kw) (declare (optimize (speed 3))) (search "SPECIAL/" (format nil "~a" kw)))

(defun resolve-isolated-nodes (graph)
  (declare (type graph graph)
	   (optimize (speed 3)))
  (let ((new-nodes) (seen) (stashed))
    (declare (type list new-nodes seen stashed))
    (flet ((seen-p (reads) (every #'(lambda (x) (or (numberp x) (find x seen :test #'eql))) reads)))
      (loop for node in (graph-nodes graph)
	    for position fixnum upfrom 0
	    for reads = (node-reads node)
	    for writes = (node-writes node)
	    for w-seen-p = (or (some #'(lambda (x) (not (find (the symbol x) seen :test #'eql))) writes) (special-p (node-class node)))
	    if (seen-p reads) do
	      (when w-seen-p
		(dolist (w writes) (push w seen))
		(push node new-nodes))
	    else do
	      (when w-seen-p
		(push (cons reads node) stashed))
	    end
	    do (when w-seen-p
		 (loop with finish-p = nil
		       with changed-p = nil
		       while (not finish-p)
		       do (setf changed-p nil)
			  (loop for (reads-old . node-old) in stashed
				if (seen-p reads-old) do
				  (push node-old new-nodes)
				  (setf changed-p t)
				  (dolist (w (node-writes node-old)) (push w seen))
				  (setf stashed (remove node-old stashed :key #'cdr :test #'equal)))
			  (setf finish-p (not changed-p))))))
    ;;(assert (null stashed) () "verify-graph: these nodes are isolated: ~a" stashed) 
    (setf (graph-nodes graph) (reverse new-nodes))
    graph))
  
(defun purge-isolated-graph (graph)
  "assuming the last graph is the final output, prunes the isolated graph"
  (declare (type graph graph) (optimize (speed 3)))
  (when (graph-nodes graph)
    (let ((output
	    (or (graph-outputs graph) (node-writes (car (last (graph-nodes graph)))))))
      (setf (graph-nodes graph)
	    (loop for n in (graph-nodes graph)
		  collect
		  (if (every #'(lambda (w) (and (symbolp w) (= 0 (length (the list (id->users graph w)))) (not (find w output)))) (node-writes n))
		      (if (special-p (node-class n)) n nil)
		      n))
	    (graph-nodes graph)
	    (loop for n in (graph-nodes graph) if n collect n)))))
