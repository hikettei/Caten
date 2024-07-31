(in-package :caten/ajit)

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))
(defun gid (n) (intern (format nil "_GID~a" n)))

(defun render-schedule (sched)
  (let* ((graph (apply #'make-graph (si-nodes sched)))
	 (avm (make-avm graph (si-name sched) (make-hash-table) nil nil)))
    (uiop:symbol-call (find-package :caten) :print-avm avm :args (schedule-depends-on sched))))

(defun print-schedules (list) (map 'list #'render-schedule list))
