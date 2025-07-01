(in-package :caten/air)

;; Difficult Point:
;; How to prepresent (A B) Tensor for example?
(defstruct (Typed
            (:constructor make-typed (dtype shape)))
  (dtype dtype :type keyword)
  (shape shape :type list))

(defun graph-infer-type-relay (graph)
  (declare (type graph graph) (optimize (speed 3)))
  (let ((id->type-map (make-hash-table)) (nodes (tpsort-graph graph)))
    (declare (type list nodes))
    (loop for node in nodes
          for type-relay = (%node-get-type-relay (node-type node))
          for outs = (funcall (the function type-relay) id->type-map node) do
            (assert (listp outs) () "type-relay should return a list!")
            (assert (= (length outs) (length (node-writes node))) () "Mismatch in the number of outputs when inferecing the type of ~a" node)
            (print outs)
            (assert (every #'typed-p outs) () "The type-relay should return a list of Typed, getting ~a" outs)
            (loop for ot in outs
                  for w in (node-writes node) do
                    (setf (gethash w id->type-map) ot)))
    (loop for node in nodes do
      (print node)      
      )
    id->type-map))
