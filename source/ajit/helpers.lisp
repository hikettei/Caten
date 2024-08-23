(in-package :caten/ajit)

(defun gid (n) (intern (format nil "_GID~a" n)))
(defun symb (&rest args) (intern (with-output-to-string (out) (dolist (n args) (princ n out)))))

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

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

(defun nodes-gather-args (nodes)
  (declare (type list nodes))
  (let ((args (remove-duplicates
	       (loop for node in nodes
		     if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
		       collect (getattr node :value)))))
    (loop for a in args
	  unless (find a `(t nil))
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

(defun alloc-args-p (node)
  (and
   (eql (node-type node) :Allocate)
   (null (getattr node :_tmp))
   (not (= (getattr node :nrank) 0))))

(defun purge-allocations (poly pipeline dynamic-shapes render-graph
			  &aux
			    (pipeline-ids (render-graph/get-timestamps render-graph))
			    (allocs nil) (types (poly-vm-io-types poly)) (outputs (poly-vm-outputs poly)))
  "Collects a list of allocation that moved to args"
  (declare (type polyhedral poly) (type hash-table pipeline))
  (maphash
   #'(lambda (k graph)
       (when (find k pipeline-ids)
	 (setf (graph-nodes graph)
	       (loop for node in (graph-nodes graph)
		     if (alloc-args-p node)
		       do (push node allocs)
		     else unless (and (eql (node-type node) :Allocate) (find (car (node-writes node)) outputs))
			    collect node))))
   pipeline)
  (let* ((tensor-allocs (remove-duplicates allocs :key (compose #'car #'node-writes)))
 	 (shapes (map 'list
		      #'(lambda (x &aux (type (or (gethash x types) (error "~a is not inferred by poly-vm-io-types" x))))
			  (%alloc 0 nil nil :dtype (buffer-dtype (car (relay-writes type))) :id x))
		      dynamic-shapes))
	 (allocs (remove-duplicates `(,@shapes ,@tensor-allocs) :key (compose #'car #'node-writes))))
    (loop for alloc in allocs
	  if (find (car (node-writes alloc)) (poly-vm-inputs poly)) ;; Shapes are not pointer
	    do (setf (getattr alloc :_pointer) nil)
	  else
	    do (setf (getattr alloc :_pointer) t))
    allocs))

(defun get-subgraph-recursively (node graph dynamic-shapes dtype)
  (declare (type node node) (type graph graph))
  (append
   (loop for r in (node-reads node)
	 if (symbolp r)
	   append (get-subgraph-recursively (id->value graph r) graph dynamic-shapes dtype))
   (if (find (car (node-writes node)) dynamic-shapes)
       (with-context-nodes
	   (_ (%load (%salloc :dtype dtype) (car (node-writes node)) :id (car (node-writes node)))))
       (list node))))
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

(defun buffer-reconstruct-view-args (buffer)
  "Reconstruct a list of symbols used to compute the buffer bound."
  (when (buffer-p buffer)
    ;; Shape, Stride, and Views
    (let* ((shape (buffer-shape buffer))
	   (stride (buffer-stride buffer))
	   (views (apply #'append (map 'list #'(lambda (x) (and x (not (fourth x)) (subseq x 0 3))) (buffer-views buffer)))))
      (loop for s1 in `(,@shape ,@stride ,@views)
	    for s = (reveal-buffer s1)
	    if (and (symbolp s) s) collect s))))

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
  ;; If (xxx) wo kesu
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
