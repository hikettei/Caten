(defpackage :caten/codegen/helpers
  (:use :cl :caten/air)
  (:export
   #:gid
   #:range
   #:nodes-depends-on
   #:nodes-write-to
   #:render-list
   #:permute-list
   #:ensure-string-as-compilable))

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

(defun gid (nth)
  (declare (type fixnum nth))
  (intern (format nil "_GID~d" nth)))

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defun render-list (list) (apply #'concatenate 'string (butlast (loop for n in list append (list (format nil "~a" n) ", ")))))
;; The same algorithm in function.lisp (class Permute)
(defun permute-list (order list)
  (assert (= (length order) (length list)) () "cannot shuffle ~a and ~a" order list)
  (loop for nth in order collect (nth nth list)))

(defun ensure-string-as-compilable (name)
  "Ensures that the string is a compilable to foreign library."
  (declare (type string name))
  (macrolet ((def (from to)
               `(setf name (cl-ppcre:regex-replace-all ,from name ,to))))
    (def "!=" "NEQ")
    (def "=" "EQ")
    (def "<" "LT")
    ;; Ensuring not containing any special characters. (e.g: a-b produces an error on clang)
    (def "[^a-zA-Z0-9_]" "_")
    name))
