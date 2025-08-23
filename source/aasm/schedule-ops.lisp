(in-package :caten/aasm)
;; ~~ ScheduleGraph ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass ScheduleGraph (FastGraph)
  ((symbolic :initarg :symbolic)))

(defmethod print-object ((graph ScheduleGraph) stream)
  (format stream "
ScheduleGraph[outputs=~a] {
~a}
"
	  (graph-outputs graph)
	  (with-output-to-string (out)
	    (dolist (node (graph-nodes (->graph-with-tpsort graph)))
              (loop for line in (cl-ppcre:split "\\n" (print-object node nil))
                    do (format out "    ~a~%" line))))))

(defun ->schedule-graph (graph)
  (declare (type Graph graph))
  (assert (null (graph-seen graph)) () "->schedule-graph: Partial graph should not be a schedule-graph! (remove graph-seen)")
  (->fast-graph graph :cls 'ScheduleGraph :args (list :symbolic nil)))
;; ~~ Schedule Items ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun $affine (writes reads)
  (declare (type list writes reads))
  (emit (make-node :Schedule :Affine writes reads)))

(defun $nonaffine (writes reads &key (items))
  (declare (type list writes reads))
  (emit (make-node :Schedule :NonAffine writes reads :items items)))

(defmethod print-node ((node Node) (id (eql :Affine)))
  (with-output-to-string (out)
    (format out "~a = Affine(~a){~%" (render-list (node-writes node)) (render-list (node-reads node)))
    (format out "    TODO: [Program]~%")
    (format out "}")))

(defmethod print-node ((node Node) (id (eql :NonAffine)))
  (with-output-to-string (out)
    (format out "~a = NonAffine(~a){~%" (render-list (node-writes node)) (render-list (node-reads node)))
    (dolist (item (getattr node :items))
      (let ((prefix (if (find (car (node-writes item)) (node-writes node))
                        (format nil "  return ")
                        (format nil "  ~(~a~) = " (render-list (node-writes item))))))
        (format out "~a~(~a~)(~(~a~));~%" prefix (node-type item) (render-list (node-reads item)))))
    (format out "}")))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
