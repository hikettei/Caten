(defpackage :caten/codegen/helpers
  (:use :cl :caten/air)
  (:export
   #:nodes-depends-on
   #:nodes-write-to))

(in-package :caten/codegen/helpers)

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

(defun nodes-write-to (nodes)
  (flet ((used-p (id) (find id nodes :key #'node-reads :test #'find)))
    (loop for node in nodes
	  append
	  (loop for w in (node-writes node)
		if (not (used-p w)) collect w))))
