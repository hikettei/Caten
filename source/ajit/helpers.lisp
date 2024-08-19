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
      (dolist (r (node-reads node))
	(when (null (find r seen))
	  (when (symbolp r)
	    (push r depends-on))
	  (push r seen)))
      (dolist (w (node-writes node))
	(push w seen)))
    (reverse depends-on)))

(defun avm-gather-args (avm)
  (declare (type avm avm))
  (let ((args (remove-duplicates
	       (loop for node in (graph-nodes (avm-graph avm))
		     if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
		       collect (getattr node :value)))))
    (loop for a in args
	  unless (find a `(t nil))
	    collect a)))

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

(defun purge-allocations (poly pipeline dynamic-shapes &aux (allocs nil) (types (poly-vm-io-types poly)) (outputs (poly-vm-outputs poly)))
  (declare (type polyhedral poly) (type hash-table pipeline))
  (maphash
   #'(lambda (k graph)
       (declare (ignore k))
       (setf (graph-nodes graph)
	     (loop for node in (graph-nodes graph)
		   if (alloc-args-p node)
		     do (push node allocs)
		   else unless (and (eql (node-type node) :Allocate) (find (car (node-writes node)) outputs))
			  collect node)))
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
