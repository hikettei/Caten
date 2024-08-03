(in-package :caten/ajit)
;; First, Nodes determined to be necessarily Fused (e.g.: non-viewed and same shaped tensors)
;; are combined into a single SubGraph and converted into an ISL AST.
;; Refenreces: https://pliss2019.github.io/albert_cohen_slides.pdf
(defstruct (Scheduled-Items
	    (:conc-name si-)
	    (:constructor make-scheduled-items (top)))
  "Top ... (top-node, top-buffer, top-id-in-nodes)"
  (nodes (list (first top)) :type list)
  (latest (second top) :type Buffer)
  (latest-id (third top) :type symbol)
  (name (intern (symbol-name (gensym "S")) "KEYWORD") :type keyword))

(defun si/append-item (scheduled-items node)
  (declare (type scheduled-items scheduled-items)
	   (type node node))
  (when (null (find (node-id node) (si-nodes scheduled-items) :test #'eql :key #'node-id))
    (push node (si-nodes scheduled-items))))

(defun buffer-intersect-p (a b)
  "Returns T if two buffers a and b are mergeable."
  (declare (type Buffer a b))
  ;; either of buffers are scalar -> merge them
  (symbol-macrolet ((->ok (return-from buffer-intersect-p t))
		    (->ng (return-from buffer-intersect-p nil)))
    ;; Either of args is a scalar -> merge them
    (when (or (= (buffer-nrank a) 0) (= 0 (buffer-nrank b)))->ok)

    ;; Contiguous and same-shaped buffer -> merge them
    (when (and
	   (every #'null (buffer-views a))
	   (every #'null (buffer-views b))
	   (equal (buffer-shape a) (buffer-shape b)))
      ->ok)
    ;; Otherwise, leave it to out polyhedral compiler.
    ->ng))

(defun recursive-find-group (avm type-map scheduled-items)
  "Return -> (list scheduled-items ...)"
  (declare (type avm avm)
	   (type type-reporter type-map)
	   (type scheduled-items scheduled-items))
  (flet ((id->type (x) (map/type-of type-map x))
	 (explore (x) (when x (recursive-find-group avm type-map x))))
    (with-slots ((latest latest) (latest-id latest-id)) scheduled-items
      (when (find latest-id (rp-seen type-map)) (return-from recursive-find-group))
      (let* ((node (id->value (avm-graph avm) latest-id))
	     (children (node-reads node))
	     (mergeable-list
	       (map 'list
		    #'(lambda (x) (or (numberp x) (buffer-intersect-p latest (id->type x)))) children)))
	(setf (rp-seen type-map) (append (rp-seen type-map) (node-writes node)))
	;; Top_ID <- F(Children[0], Children[1], ...)
	;;             mergeable[0] mergeable[1], ...
	(if (every #'identity mergeable-list)
	    (let ((parent-groups))
	      (dolist (c children)
		(when (not (numberp c))
		  (si/append-item scheduled-items (id->value (avm-graph avm) c))))
	      (dolist (c children)
		(when (not (numberp c))
		  (setf (si-latest scheduled-items) (id->type c)
			(si-latest-id scheduled-items) c)
		  (setf parent-groups (append parent-groups (cdr (explore scheduled-items))))))
	      (append (list scheduled-items) parent-groups))
	    (let ((new-groups
		    (map
		     'list
		     #'(lambda (x)
			 (when (not (numberp x))
			   (make-scheduled-items (list (id->value (avm-graph avm) x) (id->type x) x))))
		     children)))
	      (append
	       (list scheduled-items)
	       (loop for n in (map 'list #'explore new-groups) if n collect n))))))))

(defun %for (gid size)
  (declare (type (or number symbol) size))
  (emit (make-node :IR :for (list gid) (list 0 size 1))))
(defun %endfor (gid) (emit (make-node :IR :endfor nil (list gid))))

(defun buffer->loop-size (dim &rest buffers)
  (let* ((shapes (map 'list #'(lambda (x) (nth dim (buffer-shape x))) buffers))
	 (views  (map 'list #'(lambda (x) (nth dim (buffer-views x)))  buffers)))
    (declare (ignore views))
    (let ((out
	    (or
	     (when (every #'numberp shapes) (apply #'max shapes))
	     (when (every #'(lambda (x) (eql x 1)) shapes) 1)
	     (car shapes))))
      (if (buffer-p out)
	  (if (fakearray-p (buffer-value out))
	      (fakearray-initial-element (buffer-value out))
	      (buffer-value out))
	  (if (fakearray-p out)
	      (fakearray-initial-element out)
	      out)))))

(defun reveal-buffer (object)
  (if (buffer-p object)
      (if (fakearray-p (buffer-value object))
	  (fakearray-initial-element (buffer-value object))
	  (buffer-value object))
      (if (fakearray-p object)
	  (fakearray-initial-element object)
	  object)))

(defun schedule->submodule (sched type-map)
  (declare (type scheduled-items sched))
  (flet ((id->buffer (x) (map/type-of type-map x)))
    (let* ((args (map 'list #'id->buffer (schedule-depends-on sched)))
	   (nrank (or (and args (buffer-nrank (car args))) 0))
	   (index-components (map 'list #'gid (range 0 nrank)))
	   (loopsizes (map 'list #'(lambda (x) (apply #'buffer->loop-size x args)) (range 0 nrank))))
      (let ((g
	      (with-context
		(start-loop (loop for i in index-components for s in loopsizes do (%for i s)))
		(_ (dolist (node (si-nodes sched)) (emit node)))
		(end-loop (dolist (i index-components) (%endfor i))))))
	(setf (graph-seen g) (schedule-depends-on sched))
	g))))

(defun schedule-depends-on (sched)
  (declare (type scheduled-items sched))
  (let ((seen) (depends-on))
    (loop for node in (si-nodes sched) do
      (dolist (r (node-reads node))
	(when (null (find r seen))
	  (when (symbolp r)
	    (push r depends-on))
	  (push r seen)))
      (dolist (w (node-writes node))
	(push w seen)))
    (reverse depends-on)))

(defun graph->loop-factors (graph)
  (remove-duplicates
   (loop for node in (graph-nodes graph)
	 if (eql (node-type node) :FOR)
	   collect (car (node-writes node)))))

(defun render-domain (pipeline &key (depends-on nil))
  "Render the domain notation from the scheduled subgraphs
```
Domain [depends-on] -> {
  Sched_0_ID(loop_factors_0) : IConstraint_0;
  Sched_1_ID(loop_factors_1) : IConstraint_1;
                 ...
}
```
Pipeline: A hash-table where keys and values are: {T_ID[Fixnum] -> Scheduled_Subgrpah[Graph]}"
  (declare (type list depends-on)
	   (type hash-table pipeline))
  (with-output-to-string (out)
    ;; renders depends-on
    (format out "[~(~a~)] -> {~%" (render-list depends-on))
    (maphash
     #'(lambda (timestamp subgraph)
	 (let* ((loop-factors (graph->loop-factors subgraph))
		(constraints
		  (loop for node in (graph-nodes subgraph)
			if (eql (node-type node) :FOR)
			  collect
			  (progn
			    (assert (= 1 (nth 2 (node-reads node))) () "Loop steps should be optimized by the polyhedral compiler. Set=1.")
			    (make-iconstraint (car (node-writes node)) (nth 0 (node-reads node)) (nth 1 (node-reads node)))))))
	   (format out "  T~a[~(~a~)]" timestamp (render-list loop-factors))
	   (when constraints
	     (format out " : ")
	     (format out "~a" (apply #'concatenate 'string (butlast (loop for c in constraints append (list (form c) " and "))))))
	   (format out ";~%")))
     pipeline)
    (format out "}")))

(defun render-isl-aref (id type-map)
  "A[2x, y] -> A[2x*stride1 + y*stride2]"
  (declare (type symbol id))
  (let ((buffer (map/type-of type-map id)))
    (apply
     #'concatenate
     'string
     (butlast
      (loop for nth upfrom 0
	    for stride-nth in (buffer-stride buffer)
	    for view in (buffer-views buffer)
	    for stride = (reveal-buffer stride-nth)
	    for upfrom = (reveal-buffer (or (nth 0 view) 0))
	    for by     = (reveal-buffer (or (nth 2 view) 1))
	    for broadcast-p = (nth 3 view)
	    for gid = (gid nth)
	    append
	    (list
	     (progn
	       (assert (typep stride 'integer-t))
	       (assert (typep upfrom 'integer-t))
	       (assert (typep by 'integer-t))
	       (if broadcast-p
		   (format nil "~a" upfrom)
		   (format nil "~a~a~a"
			   (if (= by 1)
			       (if (and (numberp stride) (= stride 1))
				   ""
				   (format nil "~a*" stride))
			       (if (and (numberp stride) (= stride 1))
				   (format nil "~a*" by)
				   (if (and (numberp stride) (numberp by))
				       (format nil "~a*" (* stride by))
				       (format nil "~a*~a*" by stride))))
			   gid
			   (if (= upfrom 0) "" (format nil "+~a" upfrom)))))
	     "+"))))))

(defun vm-instruction-p (node)
  "Add more classes here if you have a certain node that do not desired to be involved."
  ;; :IR = :FOR :ENDFOR
  (null (find (node-class node) `(:IR :Buffer))))

(defun render-access (mode pipeline type-map &key (depends-on nil))
  "Render the read/write accessing relation ship in the following notation:
```
[depends-on] -> {
    Sched_0_ID[read_index] -> Tensor_ID_N[strided_access_idx];
        ...
}
```
"
  (declare (type list depends-on)
	   (type (member :read :write) mode)
	   (type hash-table pipeline))
  (with-output-to-string (out)
    (format out "[~(~a~)] -> {~%" (render-list depends-on))
    (maphash
     #'(lambda (timestamp subgraph)
	 (let* ((occur-from
		  (format nil "T~a[~(~a~)]"
			  timestamp (render-list (graph->loop-factors subgraph)))))
	   (dolist (node (graph-nodes subgraph))
	     (when (vm-instruction-p node)
	       ;; When reduction is T, the first argument becomes the dependency
	       ;; e.g.: Tn[...]: A <- ADD(X, Y, reduction=t) is the equivalent to
	       ;; Tn[...]: A = (X += Y),  i.e.: Tn[...]: A = (X = X + Y)
	       ;; Here, X depends on X.
	       (when (getattr node :reduction)
		 (let ((reduce-to (car (node-reads node))))
		   (when (symbolp reduce-to)
		     (format out "  ~a -> ~a[~(~a~)];~%" occur-from reduce-to (render-isl-aref reduce-to type-map)))))
	       (dolist (r (remove-duplicates (funcall (if (eql mode :read) #'node-reads #'node-writes) node)))
		 ;; When node has a :reduction
		 (when (symbolp r)
		   (format out "  ~a -> ~a[~(~a~)];~%" occur-from r (render-isl-aref r type-map))))))))
     pipeline)
    (format out "}")))

;; TODO: gidにTimestampくっつけないといけない気がする

;; polyhedral compilation to determine the parallelization strategy
;; If we do; compile from avm into ISL, optimizng
;; This is the toplevel of all optimization stuff
(defun create-schedule-grouping (avm &key (verbose nil))
  ""
  (declare (type avm avm)
	   (type boolean verbose))
  (let* ((type-map (run-type-infer avm))
	 (recursive-top-ids (append (avm-fw-outputs avm) (avm-bw-outputs avm)))
	 (dynamic-shapes (avm-gather-args avm)))
    (when verbose
      (format t "== [Initial Graph] ==~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    ;; ~ Optimizations ~~
    (apply-jit-specific-simplifiers avm)
    (when verbose
      (format t "== [Graph after applying jit-specific simplifiers] ==~%")
      (uiop:symbol-call (find-package :caten) :print-avm avm))
    (flet ((id->buffer (id)
	     (assert (symbolp id) () "Graph should not return a number!")
	     (list (id->value (avm-graph avm) id) (map/type-of type-map id) id)))
      (let* ((schedules (map 'list (compose #'make-scheduled-items #'id->buffer) recursive-top-ids))
	     (scheduled (reverse (flatten (map 'list #'(lambda (x) (recursive-find-group avm type-map x)) schedules)))))
	;; verify-graph assets no duplication in branches from recursive-top-ids
	(when verbose
	  (format t "== [Graph after applying an initial scheduler process] ==~%")
	  (print-schedules scheduled))
	
	(let* ((graphs (map 'list #'(lambda (x) (schedule->submodule x type-map)) scheduled))
	       (pipeline (make-hash-table)))
	  ;; Pipeline: T_ID -> Submodule_Graph
	  (loop for nth upfrom 0
		for g in graphs
		do (setf (gethash nth pipeline) g))
	  ;; [TODO]: When graph is fused to single?
	  ;; -> SKip the polyhedral compilation
	  ;; Creates the initial problem:
	  (let* ((domain (render-domain pipeline :depends-on dynamic-shapes))
		 (read-access (render-access :read pipeline type-map :depends-on dynamic-shapes))
		 (write-access (render-access :write pipeline type-map :depends-on dynamic-shapes)))
	    
	    (when verbose
	      (format t "== [Domain] ===========")
	      (format t "~%~a~%" domain)
	      (format t "== [Read Accesses] =======")
	      (format t "~%~a~%" read-access)
	      (format t "== [Write Accesses] ======")
	      (format t "~%~a~%" write-access))
	    
	    (let ((model (optimize-polyhedral domain read-access write-access :verbose verbose)))
	     ;;(print model)
	      )))))))

#+(or)(let ((c (caten (!identity (!matmul (make-tensor `(a b) :id 'x) (make-tensor `(b c) :id 'y))))))
	(time (caten/ajit::create-schedule-grouping c)))
