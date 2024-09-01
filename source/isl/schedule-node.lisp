(in-package :caten/isl)

(define-isl-object schedule-node
  :abstract t
  :free %isl-schedule-node-free
  :copy %isl-schedule-node-copy)

(macrolet ((def (name)
	     `(define-isl-object ,name :superclass schedule-node)))
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
  (def schedule-node-guard))

(defun %make-schedule-node (handle)
  (ecase (%isl-schedule-node-get-type handle)
    (:Schedule-Node-Leaf (%make-schedule-node-leaf handle))
    (:Schedule-Node-Filter (%make-schedule-node-Filter handle))
    (:Schedule-Node-Sequence (%make-schedule-node-sequence handle))
    (:Schedule-Node-Band (%make-schedule-node-band handle))
    (:Schedule-Node-Domain (%make-schedule-node-domain handle))
    (:Schedule-Node-Expansion (%make-schedule-node-expansion handle))
    (:Schedule-Node-Extension (%make-schedule-node-extension handle))
    (:Schedule-Node-Mark (%make-schedule-node-mark handle))
    (:Schedule-Node-Set (%make-schedule-node-set handle))
    (:Schedule-Node-Context (%make-schedule-node-context handle))
    (:Schedule-Node-Guard (%make-schedule-node-guard handle))))
