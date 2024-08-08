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

(defun print-schedules (list) (map 'list #'render-schedule list))
(defun replace-string (string from to)
  (concatenate
   'string
   (loop for s across string
	 if (equal s from) collect to
	   else collect s)))

(defun avm-gather-args (avm)
  (declare (type avm avm))
  (let ((args (remove-duplicates
	       (loop for node in (graph-nodes (avm-graph avm))
		     if (and (eql (node-type node) :Load) (getattr node :value) (symbolp (getattr node :value)))
		       collect (getattr node :value)))))
    (loop for a in args
	  unless (find a `(t nil))
	    collect a)))

(defun reveal-buffer (object)
  "Extracts the initial-value from the nested buffer/fake-array"
  (declare (type (or buffer fakearray integer-t) object))
  (if (buffer-p object)
      (if (fakearray-p (buffer-value object))
	  (fakearray-initial-element (buffer-value object))
	  (buffer-value object))
      (if (fakearray-p object)
	  (fakearray-initial-element object)
	  object)))

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

(defun purge-allocations (pipeline &aux (allocs nil))
  (maphash
   #'(lambda (k graph)
       (declare (ignore k))
       (setf (graph-nodes graph)
	     (loop for node in (graph-nodes graph)
		   if (eql (node-type node) :Allocate)
		     do (push node allocs)
		   else
		     collect node)))
   pipeline)
  (remove-duplicates allocs :key (compose #'car #'node-writes)))
