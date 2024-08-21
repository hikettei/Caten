(in-package :caten/ajit)

(defun render-graph/get-timestamps (graph)
  (declare (type graph graph))
  (loop for node in (graph-nodes graph)	if (eql (node-type node) :FUNCALL) collect (getattr node :idx)))

(defgeneric node/in-place-mutation (id node) (:documentation "Return a symbol indicating the position of output (chosen from node-reads)"))
(defmethod node/in-place-mutation :around (id node)
  (if (next-method-p)
      (call-next-method)
      (progn
	(warn "node/in-place-mutation for ~a is not defined, ignoring in-place-mutation opt." id)
	nil)))
(defmethod node/in-place-mutation ((id (eql :EXPR)) node) (car (node-reads node)))
(defmethod node/in-place-mutation ((id (eql :WMMA)) node) (car (node-reads node)))
  
;; ~~ reference-counter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Reference-Counter
	    (:conc-name refcount-))
  (alias (make-hash-table) :type hash-table)
  (refcount (make-hash-table) :type hash-table)
  (refcount-by (make-hash-table) :type hash-table))
(defmethod print-object ((r reference-counter) stream)
  (flet ((print-hash (obj)
	   (with-output-to-string (out)
	     (maphash #'(lambda (k v) (format out "~a -> ~a~%" k v)) obj))))
    (format stream "<Reference-Counter
Alias:
~a
Refcount:
~a
Refcount-by:
~a
>"
	    (print-hash (refcount-alias r))
	    (print-hash (refcount-refcount r))
	    (print-hash (refcount-refcount-by r)))))
(defun node-output-position (node) (error "deprecated") (case (node-type node) (:WHERE (second (node-reads node))) (otherwise (car (node-reads node)))))
(defun id->memwrites (graph id)
  "Counts how many times id was read in the graph, and by who?"
  (declare (type graph graph) (type symbol id))
  (let ((count 0)
	(nodes))
    (loop for node in (graph-nodes graph)
	  for reads = (node-reads node) do
	    (loop for r in reads
		  if (eql r id) do (incf count) (push node nodes)))
    (values count (remove-duplicates nodes :key #'node-id))))
(defun create-reference-counter (poly render-graph &optional poly-bw bw-render-graph)
  (declare (type polyhedral poly) (type graph render-graph))
  (let ((refcount (make-hash-table))
	(refby (make-hash-table))
	(graph
	  (apply
	   #'make-graph
	   (append
	    (loop for idx in (render-graph/get-timestamps render-graph)
		  append (graph-nodes (gethash idx (poly-pipeline poly))))
	    (when poly-bw
	      (loop for idx in (render-graph/get-timestamps bw-render-graph)
		    append (graph-nodes (gethash idx (poly-pipeline poly-bw)))))))))
    (labels ((relevant-graph (pos) (apply #'make-graph (subseq (graph-nodes graph) pos)))
	     (inplace (node pos)
	       ;; (in-place-p . users)
	       (multiple-value-bind (count users) (id->memwrites (relevant-graph pos) (car (node-writes node)))
		 (cons count users))))	     
      (loop for node in (graph-nodes graph)
	    for pos upfrom 0
	    for (refc . refb) = (inplace node pos)
	    do (setf (gethash (car (node-writes node)) refcount) refc
		     (gethash (car (node-writes node)) refby) refb)))
    (make-reference-counter :refcount refcount :refcount-by refby)))

(defun refcount/refalias (count id)
  (let ((val (gethash id (refcount-alias count))))
    (if val
	(if (eql id val)
	    val
	    (refcount/refalias count val))
	id)))

(defun refcount/make-alias (count node in-place save-for-backwards)
  (if (eql (node-type node) :Allocate)
      (setf (gethash (car (node-writes node)) (refcount-alias count)) (car (node-writes node)))
      (if in-place
	  (let ((id (node/in-place-mutation (node-type node) node)))
	    (if (find id save-for-backwards)
		nil
		(when id (setf (gethash (car (node-writes node)) (refcount-alias count)) id))))
	  (setf (gethash (car (node-writes node)) (refcount-alias count)) (car (node-writes node))))))

(defun refcount/update-buffer (count buffer)
  (flet ((new (x) (refcount/refalias count (reveal-buffer x))))
    (setf (buffer-shape buffer) (map 'list #'new (buffer-shape buffer))
	  (buffer-stride buffer) (map 'list #'new (buffer-stride buffer))
	  (buffer-views buffer)
	  (loop for v in (buffer-views buffer)
		if v
		  collect (list (new (nth 0 v)) (new (nth 1 v)) (new (nth 2 v)) (nth 3 v))
		else
		  collect v))))

(defun refcount/update-node (node refcount)
  (declare (type node node))
  (flet ((ref (x) (refcount/refalias refcount x)))
    (when (eql (node-type node) :EXPR)
      (let ((buffers (getattr node :buffers)))
	(assert (every #'(lambda (x) (eql :AREF (expr-op x))) buffers))
	(mapc
	 #'(lambda (aref)
	     (setf (expr-x aref) (ref (expr-x aref))))
	 buffers)))
    ;;(assert (null (getattr node :_reads)))
    ;;(assert (null (getattr node :_writes)))
    (map 'list #'(lambda (x) (when x (refcount/update-buffer refcount x))) `(,@(relay-writes (read-type-relay node)) ,@(relay-reads (read-type-relay node))))
    (setf (getattr node :_loop_bound_nodes) (map 'list #'ref (getattr node :_loop_bound_nodes))
	  (getattr node :_reads)  (node-reads node)
	  (getattr node :_writes) (node-writes node)
	  (node-reads node) (map 'list #'ref (node-reads node))
	  (node-writes node) (map 'list #'ref (node-writes node)))))

(defun apply-memory-planner! (avm polyhedral refcount render-graph save-for-backwards)
  (declare (type avm avm) (type polyhedral polyhedral) (type Reference-counter refcount)
	   (type graph render-graph) (type list save-for-backwards))
  (let* ((pipeline (poly-pipeline polyhedral))
	 (pipeline-ids (render-graph/get-timestamps render-graph))
	 (pipeline-ids-all (hash-table-keys pipeline))
	 (default-allocs (loop for node in (loop for k in pipeline-ids-all append (graph-nodes (gethash k pipeline)))
			       if (eql (node-type node) :Allocate)
				 collect node))
	 (pipeline-ids-by-loop (render-graph/sort-by-time render-graph))
	 (allocated-items
	   (loop for time in `(,@pipeline-ids)
		 append
		 (loop for node in (graph-nodes (gethash time pipeline))
		       if (eql (node-type node) :Allocate)
			 collect (car (node-writes node)))))
	 (allocated-items (append allocated-items
				  (poly-vm-inputs polyhedral)
				  (loop for o in (poly-vm-outputs polyhedral)
					if (poly/io-scalar-p polyhedral o) collect o))))
    (assert (equal (flatten pipeline-ids-by-loop) pipeline-ids))
    (labels ((find-alloc (id)
	       (find id default-allocs :key (compose #'car #'node-writes)))
	     (inplace-p (node time)
	       ;; Return: (in-place-p . intersects-with-current-pipeline?)
	       (dolist (r (node-reads node))
		 (when (and (symbolp r) (gethash r (refcount-refcount refcount)))
		   (decf (gethash r (refcount-refcount refcount)))))
	       (when (eql (node-type node) :Allocate) (return-from inplace-p (cons t t)))
	       ;(when (find (car (node-writes node)) save-for-backwards) (return-from inplace-p (cons nil nil)))
	       (let* ((id (or (node/in-place-mutation (node-type node) node)
			      (return-from inplace-p (cons nil nil))))
		      (refcount-n (gethash id (refcount-refcount refcount)))
		      (refdom     (gethash id (refcount-refcount-by refcount))))
		 (if refcount-n
		     (cons
		      (<= refcount-n 1)
		      (every #'(lambda (node) (find (node-id node) (graph-nodes (gethash time pipeline)) :key #'node-id)) refdom))
		     (cons t t))))
	     (newid (x) (refcount/refalias refcount x)))
      ;; O(nlogn) * the cost of id->users ...
      (loop for loop-ids in pipeline-ids-by-loop do
	(loop
	  for time in loop-ids
	  for graph = (gethash time pipeline) do
	    (loop
	      for node in (graph-nodes graph)
	      for (inplace-p . all-exists-in-the-same-pipeline) = (inplace-p node time) do
		(assert (= 1 (length (node-writes node))) ())
		(refcount/make-alias refcount node inplace-p save-for-backwards)
		;; If write-to area is not going to be used by any other ops, let's make it in-place
		;; otherwise:
		;;  - If write-to-user exists in the same schedule -> create a tmpvar.
		;;  - If write-to-user exists in the another schedule -> they are save-for-backwards, lets keep them copying
		(when (and (null inplace-p) all-exists-in-the-same-pipeline)
		  ;; [TODO]
		  ;; Minimizing the number of allocations by following the rule:
		  ;; 1. (car reads) becomes write, (except for %WHERE)
		  ;;  | -   A <- f(B, C, D)
		  ;;  | ->  B <- f(B, C, D)
		  ;; 2. If (car reads) is duplicated in the graph, allocates tmpvar e.g.:
		  ;;  | -   A1 <- f(A, B)
		  ;;  | -   A2 <- f(A, C)
		  ;;  | -   O1 <- f(A1, A2)
		  ;;   ->
		  ;;  | -   At1 <- f(A, B)
		  ;;  | -   At2 <- f(A, C)
		  ;;  | -   O1 <-  f(At1, At2)
		  ;;  Where At1, At2 is a scalar value, being rendered `float _val_0_0` in clang.
		  )
		(refcount/update-node node refcount))))

      ;; Update allocated-items
      ;; undecl var wo all kaiketu siyou
      (setf allocated-items (map 'list #'newid allocated-items))
      (loop for time in `(,@pipeline-ids)
	    for graph = (gethash time pipeline) do
	      (loop for node in (graph-nodes graph)
		    for type = (read-type-relay node) do
		      (loop for id in `(,@(node-writes node) ,@(node-reads node))
			    for typ in `(,@(relay-writes type) ,@(relay-reads type))
			    if (and (symbolp id) (null (find id allocated-items))) do
			      (push id allocated-items)
			      (setf (graph-nodes (gethash time pipeline))
				    (append
				     (list
				      (or
				       (find-alloc id)
				       (progn
					 (warn "~a is not defined" id)
					 (make-node :Buffer :Allocate
						    (list id) (map 'list #'reveal-buffer `(,@(buffer-shape typ) ,@(buffer-stride typ)))
						    :nrank (buffer-nrank typ)
						    :dtype (buffer-dtype typ)
						    :_type_relay (make-inferred-type nil (list typ))
						    :_labelled
						    (if (find id save-for-backwards)
							"Save_for_backward"
							"Tmp")))))
				     (graph-nodes (gethash time pipeline)))))))
      
      (flet ((replacer (x) (refcount/refalias refcount x)))
	(loop for g in (graph-nodes render-graph)
	      if (eql (node-type g) :FOR) do
		(expr-recursive-replace (getattr g :below) #'replacer)
		(expr-recursive-replace (getattr g :upfrom) #'replacer)
		(expr-recursive-replace (getattr g :by) #'replacer)))
      
      (setf (avm-fw-outputs avm) (map 'list #'newid (avm-fw-outputs avm))
	    (avm-bw-outputs avm) (map 'list #'newid (avm-bw-outputs avm))
	    ;; vm-inputs are fixed (they are dynamic shapes)
	    (poly-vm-outputs polyhedral) (map 'list #'newid (poly-vm-outputs polyhedral)))

      (macrolet ((renew (accessor)
		   `(let ((new-table (make-hash-table)))
		      (maphash
		       #'(lambda (k v)
			   (setf (gethash (newid k) new-table) v))
		       ,accessor)
		      (setf ,accessor new-table))))
	(renew (avm-id2tensor avm))
	(renew (poly-vm-io-types polyhedral))
	(renew (avm-variables avm))))))
