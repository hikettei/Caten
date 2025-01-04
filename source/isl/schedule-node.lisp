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

(define-isl-function schedule-node-band-get-partial-schedule %isl-schedule-node-band-get-partial-schedule
  (:give multi-union-pw-aff)
  (:keep schedule-node))

(define-isl-function schedule-node-band-get-partial-schedule-union-map %isl-schedule-node-band-get-partial-schedule-union-map
  (:give union-map)
  (:keep schedule-node))

(define-isl-function schedule-node-get-domain %isl-schedule-node-get-domain
  (:give union-set)
  (:keep schedule-node))

(define-isl-function schedule-node-band-get-space %isl-schedule-node-band-get-space
  (:give space)
  (:keep schedule-node))

(defun schedule-node-get-child (schedule-node n)
  (%make-schedule-node (%isl-schedule-node-get-child (schedule-node-handle schedule-node) n)))

(defun schedule-node-get-ancestor (schedule-node generation)
  (%make-schedule-node (%isl-schedule-node-ancestor (schedule-node-handle schedule-node) generation)))

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

(define-isl-function schedule-node-insert-mark %isl-schedule-node-insert-mark
  (:give schedule-node)
  (:take schedule-node)
  (:take identifier))

(define-isl-function schedule-node-delete %isl-schedule-node-delete
  (:give schedule-node)
  (:take schedule-node))

(define-isl-function schedule-node-first-child %isl-schedule-node-first-child
  (:give schedule-node)
  (:take schedule-node))

(define-isl-function schedule-node-band-set-ast-build-options %isl-schedule-node-band-set-ast-build-options
  (:give schedule-node)
  (:take schedule-node)
  (:take union-set))

(define-isl-function schedule-node-band-tile %isl-schedule-node-band-tile
  (:give schedule-node)
  (:take schedule-node)
  (:take multi-val))

(define-isl-function schedule-node-band-get-ast-isolate-option %isl-schedule-node-band-get-ast-isolate-option
  (:give set)
  (:keep schedule-node))

(defun schedule-node-band-member-set-isolate-ast-loop-type (schedule-node-band pos type)
  (%make-schedule-node (%isl-schedule-node-band-member-set-isolate-ast-loop-type (schedule-node-handle (copy schedule-node-band)) pos type)))

(define-isl-function schedule-node-mark-get-id %isl-schedule-node-mark-get-id
  (:give identifier)
  (:keep schedule-node))

(defun schedule-node-get-schedule-depth (node)
  (declare (type schedule-node node))
  (%isl-schedule-node-get-schedule-depth (schedule-node-handle node)))
