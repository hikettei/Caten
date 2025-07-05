(in-package :caten/air)

(defclass AType () nil)

(defun graph-infer-type-relay (graph)
  (declare (type graph graph) (optimize (speed 3)))
  (let ((id->type-map (make-hash-table)) (nodes (tpsort-graph graph)))
    (declare (type list nodes))
    (macrolet ((handled (form msg)
                 `(handler-case ,form (error (e) (error "~a: ~a" ,msg e)))))
      (loop for node in nodes
            for type-relay = (handled (%node-get-type-relay (node-type node)) (format nil "Looks like the node ~a does not have a type relay definition." (node-type node)))
            for outs = (handled (funcall (the function type-relay) id->type-map node) (format nil "Caught an error during propagating the type-relay inference of ~a" (node-type node))) do
              (assert (listp outs) () "The :type-relay function of ~a returned ~a, expecting a list of AType" (node-type node) outs)
              (assert (= (length outs) (length (node-writes node))) () "Mismatch in the number of outputs when inferecing the type of ~a" node)
              (assert (every #'(lambda (x) (typep x 'AType)) outs) () "The :type-relay function of ~a should return a list of AType class" (node-type node))
              (loop for ot in outs
                    for w in (node-writes node) do
                      (setf (gethash w id->type-map) ot)))
      (flet ((r (sym)
               (if (symbolp sym)
                   (or (gethash sym id->type-map) (error "graph-infer-type-relay: Cannot determine the type of ~a." sym))
                   nil)))
        (loop for node in nodes
              for relay = (make-relay :reads (map 'list #'r (node-reads node)) :writes (map 'list #'r (node-writes node))) do
                (setf (node-type-relay node) relay))
        id->type-map))))
