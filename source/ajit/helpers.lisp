(in-package :caten/ajit)

(deftype integer-t () `(or number symbol))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gid (n) (intern (format nil "_GID~a" n)))
  (defun symb (&rest args) (intern (with-output-to-string (out) (dolist (n args) (princ n out)))))
  (defmacro range (from below &optional (by 1))
    `(loop for i from ,from below ,below by ,by collect i)))

(defun maphash1 (function hash-table)
  "Equivalent to the maphash, but the order in which keys are called is sorted.
Since the order in which functions are executed are not ANSI Portable. For scheduler related algorithm, this function
should be used instead"
  (declare (type function function) (type hash-table hash-table))
  (let ((keys (hash-table-keys hash-table)))
    (assert (every #'numberp keys))
    (loop for key in (sort keys #'<)
	  do (funcall function key (gethash key hash-table)))))

(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))
(defun render-schedule (sched)
  (let* ((graph (apply #'make-graph (si-nodes sched)))
	 (avm (make-avm graph (si-name sched) (make-hash-table) nil nil)))
    (uiop:symbol-call (find-package :caten) :print-avm avm :args (schedule-depends-on sched))))
(defun render-graph (name graph)
  (let* ((avm (make-avm graph name (make-hash-table) nil nil)))
    (uiop:symbol-call (find-package :caten) :print-avm avm :args (nodes-depends-on (graph-nodes graph)))))

(defun print-schedules (list) (map 'list #'render-schedule list))
(defun print-pipeline (pipeline)
  (maphash
   #'(lambda (ts graph)
       (render-graph (intern (format nil "T~a" ts) "KEYWORD") graph))
   pipeline))
(defun replace-string (string from to)
  (concatenate
   'string
   (loop for s across string
	 if (equal s from) collect to
	   else collect s)))

(defun make-uconst-buffer () (make-buffer 0 nil nil *default-uint* nil))
(defun make-const-buffer (dtype) (make-buffer 0 nil nil dtype nil))

(defun nodes-depends-on (nodes)
  "Enumerates the unsolved buffer ids from the sched graph."
  (declare (type list nodes))
  (let ((seen) (depends-on))
    (loop for node in nodes do
      (dolist (r `(,@(node-reads node) ,@(getattr node :_loop_bound_nodes)))
	(when (null (find r seen))
	  (when (symbolp r)
	    (push r depends-on))
	  (push r seen)))
      (dolist (w (node-writes node))
	(push w seen)))
    (reverse depends-on)))

(defun nodes-depends-on/buffers (nodes)
  "Enumerates the unsolved buffer ids from the sched graph."
  (declare (type list nodes))
  (let ((seen `(t nil)) (depends-on))
    (loop for node in nodes do
      (loop for r in `(,@(node-reads node) ,@(getattr node :_loop_bound_nodes))
	    for typ in `(,@(relay-reads (read-type-relay node)) ,@(getattr node :_loop_bound_nodes_type))
	    if (null (find r seen)) do
	      (when (symbolp r) (push (cons r typ) depends-on))
	      (push r seen))
      (loop for read in (node-reads node) do
	(loop for shape in (buffer-reconstruct-view-args read)
	      if (null (find shape seen)) do
		(push (cons shape (make-uconst-buffer)) depends-on)
		(push shape seen)))
      (dolist (w (node-writes node))
	(push w seen)))
    (reverse depends-on)))

(defun nodes-gather-args (nodes &key (get-nodes nil))
  (declare (type list nodes))
  (let ((args (remove-duplicates
	       (loop for node in nodes
		     if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
		       collect (if get-nodes node (getattr node :value))))))
    (loop for a in args
	  unless (find (if get-nodes (getattr a :value) a) `(t nil))
	    collect a)))

(defun avm-gather-args (avm)
  (declare (type avm avm))
  (nodes-gather-args (graph-nodes (avm-graph avm))))

(defun infer-vm-io-types (avm scalars)
  (declare (type avm avm) (type list scalars))
  (let ((out (make-hash-table)))
    (map
     'list
     #'(lambda (x)
	 (let ((type (read-type-relay
		      (or
		       (find x (graph-nodes (avm-graph avm))
			     :test #'eql
			     :key #'(lambda (node) (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)) (getattr node :value))))
		       (find x (graph-nodes (avm-graph avm)) :test #'eql :key (compose #'car #'node-writes))
		       (error "(Bug) No definition for ~a in AVM!~%~a" x avm)))))
	   (setf (gethash x out) type)))
     scalars)
    out))

(defun reveal-buffer (object)
  "Extracts the initial-value from the nested buffer/fake-array"
  (declare (type (or buffer fakearray integer-t string) object))
  (if (stringp object)
      object
      (if (buffer-p object)
	  (if (fakearray-p (buffer-value object))
	      (fakearray-initial-element (buffer-value object))
	      (buffer-value object))
	  (if (fakearray-p object)
	      (fakearray-initial-element object)
	      object))))

(defmacro with-inlined-foreign-funcall-mode (&body body)
  "Enables %\"foreign-name\":return-dtype &rest args syntax under the body execution"
  `(macrolet ((% (fname return-type &rest args)
		`(foreign-funcall ,fname ,@args ,return-type)))
     ,@body))

(defun remove-iteration-ir (pipeline)
  (loop for nth being each hash-keys of pipeline
	  using (hash-value graph)
	do (setf (graph-nodes graph)
		 (loop for node in (graph-nodes graph)
		       unless (or (eql (node-type node) :FOR) (eql (node-type node) :ENDFOR))
			 collect node))))

(defun get-subgraph-recursively (node graph dynamic-shapes dtype &aux (seen nil))
  (declare (type node node) (type graph graph) (optimize (speed 3)))
  (labels ((explore (node graph)
	     (when (null (find (node-id node) seen))
	       (push (node-id node) seen)
	       (nconc
		(loop for r in (node-reads node)
		      if (symbolp r)
			append (explore (id->value graph r) graph))
		(if (find (the symbol (car (node-writes node))) (the list dynamic-shapes))
		    (with-context-nodes
			(_ (%load (%salloc :dtype dtype) (car (node-writes node)) :id (car (node-writes node)))))
		    (list node))))))
    (explore node graph)))

(defun get-subgraph (id graph) (get-subgraph-recursively (id->value graph id) graph nil nil))

(defun recursively-find-output-id (id graph &aux (seen nil))
  "Exploring the graph from id, returns a list of buffer ids which is not used in the graph (i.e.: outputs).
Graph must be verified in advance."
  (declare (type symbol id) (type graph graph))
  (labels ((explore (id)
	     (when (null (find id seen))
	       (push id seen)
	       (let ((outputs (id->users graph id)))
		 (if (null outputs)
		     (list id)
		     (apply #'append (map 'list #'(lambda (x) (explore (car (node-writes x)))) outputs)))))))
    (explore id)))

(defun nodes-output-ids (nodes)
  (flet ((used-p (id) (find id nodes :key #'node-reads :test #'find)))
    (loop for node in nodes
	  append
	  (loop for w in (node-writes node)
		if (not (used-p w)) collect w))))

(defun buffer-reconstruct-view-args (buffer &key (except-for-shape nil))
  "Reconstruct a list of symbols used to compute the buffer bound."
  (when (buffer-p buffer)
    ;; Shape, Stride, and Views
    (loop for shape in (buffer-shape buffer)
	  for stride in (buffer-stride buffer)
	  for nth upfrom 0
	  for view = (nth nth (buffer-views buffer))
	  for upfrom = (nth 0 view)
	  for below = (nth 1 view)
	  for by  = (nth 2 view)
	  for broadcast = (nth 3 view)
	  if broadcast
	    append (if except-for-shape `(,shape) nil)
	  else
	    append
	    (loop for val in (if except-for-shape
				 `(,stride ,upfrom ,upfrom ,by)
				 `(,shape ,stride ,upfrom ,below ,by))
		  for r = (and val (reveal-buffer val))
		  if (and val (symbolp r)) collect r))))

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

(defun zenity/prompt-new-value (code)
  (let* ((zenity-cmd `("zenity" "--text-info" "--editable" "--width=800" "--height=600" "--title=Code_Editor"))
	 (process-info (uiop:launch-program zenity-cmd :input :stream :output :stream :error-output :stream))
	 (input (uiop:process-info-input process-info))
	 (output (uiop:process-info-output process-info))
	 (error-out (uiop:process-info-error-output process-info)))
    (unwind-protect
         (progn
           (princ code input)
           (close input)))
    (if (zerop (uiop:wait-process process-info))
        (alexandria:read-stream-content-into-string output)
        (error "Failed: ~a" (alexandria:read-stream-content-into-string error-out)))))

(defun render-graph/sort-by-time (rendering-graph)
  "FUNCALL existing in the same loop, can be recognised as the same schedule."
  (declare (type graph rendering-graph))
  (let ((groups (list nil)))
    (loop for node in (graph-nodes rendering-graph)
	  if (eql (node-type node) :FUNCALL)
	    do (setf (car groups) (append (car groups) (list (getattr node :idx))))
	  if (or (eql (node-type node) :ENDFOR) (eql (node-type node) :FOR))
	    do (push nil groups))
    (reverse (loop for g in groups if g collect g))))

;; TODO: Delete this
(defun break-schedule (schedule split-at)
  (declare (type scheduled-items schedule) (type list split-at))
  (let ((new-schedules nil) (stack))
    (loop for node in (si-nodes schedule)
	  if (find (car (node-writes node)) split-at)
	    do (push node stack) (push stack new-schedules) (setf stack nil)
	  else
	    do (push node stack))
    (when stack (push stack new-schedules))
    (map 'list #'(lambda (x &aux (x (reverse x)))
		   (let ((out (make-scheduled-items (list (car x) (si-latest schedule) (si-latest-id schedule)))))
		     (setf (si-nodes out) x)
		     out))
	 (reverse new-schedules))))

(defun break-group (group split-at)
  (declare (type group group) (type list split-at))
  (let ((new-groups nil) (stack))
    (loop for node in (graph-nodes (group-graph group))
	  if (find (car (node-writes node)) split-at)
	    do (push node stack) (push stack new-groups) (setf stack nil)
	  else
	    do (push node stack))
    (when stack (push stack new-groups))
    (map
     'list
     #'(lambda (x &aux (x (reverse x)))
	 ;; force-realize-vm groups are always standalone.
	 (make-group x (group-realize-on-vm group)))
     (reverse new-groups))))

(defun schedule/resolve-isolated-ops (schedules seen-old)
  "    A   B
        \ / C   D
         |   \ /
tgt-id-> C    D
          \   /             C <- violates the dependency graph (if not scheduled in the same group)
           out <- from-node |
            \          ..  /
            
Consider the subgraph above, C was appeared in the another subgraph, therefore, C cannot be merged
in a single timestamp otherwise recursive dependencies will occur.
"
  (let ((out) (stashed) (seen (copy-list seen-old)))
    (labels ((seen-p (x) (assert (not (listp x))) (or (numberp x) (find x seen :test #'eql)))
	     (read-p (deps) (every #'seen-p deps)))
      (loop for schedule in schedules
	    for deps = (nodes-depends-on (si-nodes schedule))
	    if (read-p deps) do
	      (dolist (node (si-nodes schedule))
		(dolist (w (node-writes node)) (push w seen)))
	      (push schedule out)
	    else
	      do (push (cons deps schedule) stashed)
	    end
	    do (loop with finish-p = nil
		     with changed-p = nil
		     while (not finish-p)
		     do (setf changed-p nil)
			(loop for (deps-old . sched-old) in stashed
			      if (read-p deps-old)
				do (push sched-old out)
				   (setf changed-p t)
				   (dolist (node (si-nodes sched-old))
				     (dolist (w (node-writes node)) (push w seen)))
				   (setf stashed (remove (si-name sched-old) stashed :key (compose #'si-name #'cdr) :test #'equal)))
			(setf finish-p (not changed-p))))
      (when stashed
	(let ((split-at))
	  (loop for (deps . sched) in (reverse stashed) do
	    (dolist (d deps)
	      (unless (seen-p d) (push d split-at))))
	  (setf split-at (remove-duplicates split-at))
	  ;; Split the schedule and try again
	  (let ((old-size (length schedules))
		(new-schedule (apply #'append (map 'list #'(lambda (x) (break-schedule x split-at)) schedules))))
	    (when (= old-size (length new-schedule))
	      (error "Could not resolve this circular dependencies: ~a" (map 'list #'car stashed)))
	    (return-from schedule/resolve-isolated-ops
	      (schedule/resolve-isolated-ops
	       new-schedule
	       seen-old)))))
      (values (reverse out) seen))))

(defun group/resolve-dependencies (groups &key (seen-old nil))
  "    A   B
        \ / C   D
         |   \ /
tgt-id-> C    D
          \   /             C <- violates the dependency graph (if not scheduled in the same group)
           out <- from-node |
            \          ..  /
            
Consider the subgraph above, C was appeared in the another subgraph, therefore, C cannot be merged
in a single timestamp otherwise recursive dependencies will occur.
"
  (declare (type list groups seen-old))
  (let ((out) (stashed) (seen (copy-list seen-old)))
    (labels ((seen-p (x) (assert (not (listp x))) (or (numberp x) (find x seen :test #'eql)))
	     (read-p (deps) (every #'seen-p deps)))
      (loop for group in groups
	    for nodes = (graph-nodes (group-graph group))
	    for deps = (nodes-depends-on nodes)
	    if (read-p deps) do
	      (dolist (node nodes)
		(dolist (w (node-writes node)) (push w seen)))
	      ;; satisfied all dependencies -> OK
	      (push group out)
	    else
	      ;; the dep was not satisfied -> wait till satisfied w/ missing vars.
	      do (push (cons deps group) stashed)
	    end
	    do (loop with finish-p = nil
		     with changed-p = nil
		     while (not finish-p)
		     do (setf changed-p nil)
			(loop for (deps-old . group-old) in stashed
			      for old-nodes = (graph-nodes (group-graph group-old))
			      if (read-p deps-old)
				;; waiting until all deps are satisfied, and ready -> OK
				do (push group-old out)
				   (setf changed-p t)
				   (dolist (node old-nodes)
				     (dolist (w (node-writes node)) (push w seen)))
				   (setf stashed (remove (group-id group-old) stashed :key (compose #'group-id #'cdr) :test #'equal)))
			(setf finish-p (not changed-p))))
      ;; still some groups are remained? -> split the group at
      (when stashed
	(let ((split-at))
	  (loop for (deps . group) in (reverse stashed) do
	    (dolist (d deps)
	      ;; enumerate failing points
	      (unless (seen-p d) (push d split-at))))
	  (setf split-at (remove-duplicates split-at))
	  ;; Split the schedule and try again
	  (let ((old-size (length groups))
		(new-groups (apply #'append (map 'list #'(lambda (x) (break-group x split-at)) groups))))
	    (when (= old-size (length new-groups))
	      (error "Could not resolve this circular dependencies: ~a" (map 'list #'car stashed)))
	    (return-from group/resolve-dependencies
	      (group/resolve-dependencies
	       new-groups
	       :seen-old seen-old)))))
      (values (reverse out) seen))))

(defun remove-unused-allocs (graph)
  (apply
   #'make-graph
   (loop for node in (graph-nodes graph)
	 if (and
	     (eql (node-type node) :Allocate)
	     (find (car (node-writes node)) (graph-nodes graph) :key #'node-reads :test #'find))
	   collect node
	 if (not (eql (node-type node) :Allocate)) collect node)))

(defun remove-unused-nodes (graph use)
  (flet ((f (&aux (changed-p nil) (read-by-time (map 'list #'node-reads (graph-nodes graph))))
	   (loop for node in (graph-nodes graph)
		 for writes = (node-writes node)
		 for nth upfrom 0
		 for reads = (flatten (nthcdr (1+ nth) read-by-time))
		 for dep = (nconc reads use)
		 if (and (not (eql (node-type node) :pause/backward))
			 (every #'(lambda (x) (null (find x dep))) writes))
		   do (remnode graph (node-id node))
		      (setf changed-p t))
	   changed-p))
    (loop while (f))))

(defmethod clean-up-attrs ((graph graph))
  (every #'(lambda (node) (remattr node :_reads_old_for_multiexpr) node) (graph-nodes graph))
  graph)

(defun optimize-non-in-place-buffers (base-avm avm graph seen verbose)
  (let* ((kernel-arg-symbols
	   (loop for node in (graph-nodes graph)
		 if (eql (node-type node) :JIT_KERNEL)
		   append
		   (loop for r in (node-reads node)
			 unless (find r seen) collect r)))
	 (declared
	   (loop for node in (graph-nodes graph)
		 unless (eql (node-type node) :JIT_KERNEL)
		   append (node-writes node)))
	 (non-in-place-list
	   (loop for k in kernel-arg-symbols
		 if (null (find k declared))
		   collect k))
	 (extra-allocs
	   (loop for name in non-in-place-list
		 for node = (find name (graph-nodes (avm-graph avm)) :test #'find :key #'node-writes)
		 if node
		   collect
		   (let* ((pos  (position name (node-writes node)))
			  (typ  (nth pos (relay-writes (read-type-relay node)))))
		     (make-node :Buffer :Allocate
				(list name) (map 'list #'reveal-buffer `(,@(buffer-shape typ) ,@(buffer-stride typ)))
				:nrank (buffer-nrank typ)
				:dtype (buffer-dtype typ)
				:_type_relay (make-inferred-type nil (list typ)))))))
    (when verbose (format t "~%A number of buffers that failed to mutate in-place: ~a" (length extra-allocs)))
    ;; [TODO] Schedule to reuse the allocated buffer in non-in-place-list
    ;; Relocate to the most nearest
    (flet ((consume (alloc)
	     (setf extra-allocs (remove (car (node-writes alloc)) extra-allocs :key (compose #'car #'node-writes)))
	     alloc))
      (setf (graph-nodes graph)
	    (loop for node in (graph-nodes graph)
		  if (eql (node-type node) :JIT_KERNEL)
		    append (loop for read in (node-reads node)
				 for alloc = (find read extra-allocs :key (compose #'car #'node-writes))
				 if alloc collect (consume alloc))
		    and
		      collect node
		  else
		    collect node))
      (flet ((finalize (id-base id-comp)
	       (let ((result1 (find id-base (graph-nodes (avm-graph base-avm)) :key (compose #'car #'node-writes)))
		     (result2 (find id-comp (reverse (graph-nodes (avm-graph avm))) :key (compose #'car #'node-writes))))
		 (when (and result1 result2)
		   (case (node-type result1)
		     (:Allocate
		      (when (eql :Allocate (node-type result2)) result2))
		     (:View
		      (let ((view1 (copy-node result1)))
			(flet ((newid (x)
				 (let* ((val-node (id->value (avm-graph base-avm) x)))
				   (if (eql (node-type val-node) :Load)
				       (getattr val-node :value)
				       (if (numberp x)
					   x
					   (progn
					     (warn ":View cannot be traced in the graph recursively")
					     (return-from finalize nil)))))))
			  (setf
			   (node-writes view1) (list id-comp)
			   (node-reads view1) `(,id-comp ,@(map 'list #'newid (cdr (node-reads view1)))))
			  view1))))))))
	(loop for additional-graph in (map 'list #'finalize (avm-fw-outputs base-avm) (avm-fw-outputs avm))
	      if additional-graph collect (nconc (graph-nodes graph) (list additional-graph)))
	(remove-unused-nodes graph (append (avm-fw-outputs avm) (avm-bw-outputs avm)))
	graph))))

(defun pipeline/upper-nrank (pipeline)
  (apply
   #'max
   (loop for key in (hash-table-keys pipeline)
	 collect (length (graph->loop-factors (gethash key pipeline))))))

(defun padding-list (list rank &key (with 0))
  (append list (loop for i in (range 0 (- rank (length list))) collect with)))
