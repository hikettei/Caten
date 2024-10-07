(in-package :caten/isl)

(define-isl-object schedule-node
  :abstract t
  :free %isl-schedule-node-free
  :copy %isl-schedule-node-copy)

(macrolet ((def (name)
	     `(progn
		(export ',name)
		(define-isl-object ,name :superclass schedule-node)
		(defmethod print-object ((value ,name) stream)
		  (print-unreadable-object (value stream :type t)
		    (write-string (%isl-schedule-node-to-str (isl-object-handle value)) stream))))))
  (def schedule-node-leaf)
  (def schedule-node-filter)
  (def schedule-node-sequence)
  (def schedule-node-band)
  (def schedule-node-domain)
  (def schedule-node-expansion)
  (def schedule-node-extension)
  (def schedule-node-mark)
  (def schedule-node-set)
  (def schedule-node-context)
  (def schedule-node-guard)
  (def schedule-node-error))

(defun %make-schedule-node (handle)
  (ecase (%isl-schedule-node-get-type handle)
    (:Schedule-Node-Leaf (%make-schedule-node-leaf handle))
    (:Schedule-Node-Filter (%make-schedule-node-filter handle))
    (:Schedule-Node-Sequence (%make-schedule-node-sequence handle))
    (:Schedule-Node-Band (%make-schedule-node-band handle))
    (:Schedule-Node-Domain (%make-schedule-node-domain handle))
    (:Schedule-Node-Expansion (%make-schedule-node-expansion handle))
    (:Schedule-Node-Extension (%make-schedule-node-extension handle))
    (:Schedule-Node-Mark (%make-schedule-node-mark handle))
    (:Schedule-Node-Set (%make-schedule-node-set handle))
    (:Schedule-Node-Context (%make-schedule-node-context handle))
    (:Schedule-Node-Guard (%make-schedule-node-guard handle))
    (:Schedule-Node-Error (%make-schedule-node-error handle))))

(define-isl-function schedule-get-root %isl-schedule-get-root
  (:give schedule-node)
  (:keep schedule))

(define-isl-function schedule-node-graft-after %isl-schedule-node-graft-after
  (:give schedule-node)
  (:take schedule-node)
  (:take schedule-node))

(define-isl-function schedule-node-graft-before %isl-schedule-node-graft-before
  (:give schedule-node)
  (:take schedule-node)
  (:take schedule-node))

(define-isl-function schedule-insert-partial-schedule %isl-schedule-insert-partial-schedule
  (:give schedule)
  (:take schedule)
  (:take multi-union-pw-aff))

(define-isl-function schedule-node-insert-partial-schedule %isl-schedule-node-insert-partial-schedule
  (:give schedule-node)
  (:take schedule-node)
  (:take multi-union-pw-aff))

(define-isl-function schedule-node-from-domain %isl-schedule-node-from-domain
  (:give schedule-node)
  (:take union-set))

(define-isl-function schedule-node-get-schedule %isl-schedule-node-get-schedule
  (:give schedule)
  (:keep schedule-node))

(define-isl-function schedule-node-get-domain %isl-schedule-node-get-domain
  (:give union-set)
  (:keep schedule-node))

(define-isl-function schedule-node-filter-get-filter %isl-schedule-node-filter-get-filter
  (:give union-set)
  (:keep schedule-node))

(define-isl-function schedule-node-insert-filter %isl-schedule-node-insert-filter
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set))

(defun schedule-node-get-child (schedule-node n)
  (let ((x (%isl-schedule-node-get-child (schedule-node-handle schedule-node) n)))
    (%make-schedule-node (%isl-schedule-node-copy x))))

(defun schedule-node-get-children (schedule-node)
  (declare (type schedule-node schedule-node))
  (when (eql :bool-true (isl::%isl-schedule-node-has-children (schedule-node-handle schedule-node)))
    (let ((n (isl::%isl-schedule-node-n-children (schedule-node-handle schedule-node))))
      (loop for nth upfrom 0 below n
	    collect (isl::%isl-schedule-node-child (schedule-node-handle schedule-node) nth)))))

(defun schedule-node-get-type (schedule-node)
  (declare (type schedule-node schedule-node))
  (%isl-schedule-node-get-type (schedule-node-handle schedule-node)))

(define-isl-function schedule-insert-sequence %isl-schedule-node-insert-sequence
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set-list))

(defun schedule-node-get-tree-depth (schedule-node)
  (%isl-schedule-node-get-tree-depth (schedule-node-handle schedule-node)))
