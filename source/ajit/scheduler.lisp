(in-package :caten/ajit)
;; First, Nodes determined to be necessarily Fused (e.g.: non-viewed and same shaped tensors)
;; are combined into a single SubGraph and converted into an ISL AST.
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
      (when (find latest-id (rp-seen type-map))
	(return-from recursive-find-group))
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

(defun %purge-views-from-schedule (avm)
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

;; polyhedral compilation to determine the parallelization strategy
;; If we do; compile from avm into ISL, optimizng
(defun create-schedule (avm)
  (declare (type avm avm))
  (let* ((type-map (run-type-infer avm))
	 (recursive-top-ids (append (avm-fw-outputs avm) (avm-bw-outputs avm))))
    ;;(uiop:symbol-call (find-package :caten) :print-avm avm)
    ;;(print "++++++")
    ;; ~ Optimizations ~~
    (%purge-views-from-schedule avm)
    ;;(print "++++++")
    (wmma-rewriter (avm-graph avm) :no-verify t)
    (contiguous-after-wmma (avm-graph avm) :no-verify t)
    ;;(uiop:symbol-call (find-package :caten) :print-avm avm)
    ;;(print "++++++")
    (flet ((id->buffer (id)
	     (assert (symbolp id) () "Graph should not return a number!")
	     (list (id->value (avm-graph avm) id) (map/type-of type-map id) id)))
      (let* ((schedules (map 'list (compose #'make-scheduled-items #'id->buffer) recursive-top-ids))
	     (scheduled (reverse (flatten (map 'list #'(lambda (x) (recursive-find-group avm type-map x)) schedules)))))
	;; verify-graph assets no duplication in branches from recursive-top-ids
	(print "++ Scheduled ++")
	(print-schedules scheduled)
	scheduled
	(print "++ Polyhedral ++")
	(let ((graphs (map 'list #'(lambda (x) (schedule->submodule x type-map)) scheduled)))
	  (print graphs))
	;; TODO: Pipelining
	nil))))

#+(or)(let ((c (caten (!identity (!matmul (make-tensor `(a b) :id 'x) (make-tensor `(b c) :id 'y))))))
	(time (caten/ajit::create-schedule c)))
