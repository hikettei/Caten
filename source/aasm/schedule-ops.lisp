(in-package :caten/aasm)
;; ~~ ScheduleGraph ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass ScheduleGraph (FastGraph)
  ;; [TODO]
  ;; - Symbolics (e.g.: DynamicShape)
  ;; - Inputs (e.g.: TensorInput)
  ((symbolic :initarg :symbolic :accessor schedule-graph-symbolcs)))

(defmethod print-object ((graph ScheduleGraph) stream)
  (format stream "
ScheduleGraph[() -> (~a)] {
~a}
"
          (render-list (graph-outputs graph))
	  (with-output-to-string (out)
	    (dolist (node (graph-nodes (->graph-with-tpsort graph)))
              (loop for line in (cl-ppcre:split "\\n" (print-object node nil))
                    do (format out "    ~a~%" line))))))

(defmethod verify-schedule-graph ((graph ScheduleGraph))
  (dolist (item (graph-nodes graph))
    (assert (or (eql (node-type item) :Affine) (eql (node-type item) :NonAffine))
            ()
            "verify-schedule-graph: ScheduleGraph should be consisted of :Affine or :NonAffine, getting ~a" item))
  graph)

(defun ->schedule-graph (graph)
  (declare (type Graph graph))
  (assert (null (graph-seen graph)) () "->schedule-graph: Partial graph should not be a schedule-graph! (remove graph-seen)")
  (verify-schedule-graph (->fast-graph graph :cls 'ScheduleGraph :args (list :symbolic nil))))
;; ~~ Schedule Items ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun $affine (writes reads &key (polyhedron) (blueprint))
  (declare (type list writes reads))
  (emit (make-node :Schedule :Affine writes reads :polyhedron polyhedron :blueprint blueprint)))

(defun $nonaffine (writes reads &key (items))
  (declare (type list writes reads))
  (emit (make-node :Schedule :NonAffine writes reads :items items)))

(defmethod print-node ((node Node) (id (eql :Affine)))
  (with-output-to-string (out)
    (format out "~a = Affine(~a){~%" (render-list (node-writes node)) (render-list (node-reads node)))
    (let ((sched (uiop:symbol-call
                  :caten/codegen/search/ast :ast->str
                  (uiop:symbol-call
                   :caten/codegen/search/ast
                   :compute-ast-from-schedule
                   (uiop:symbol-call
                    :caten/codegen/search/polyhedral
                    :psi-theta (getattr node :polyhedron))))))
      (dolist (w (node-writes node))
        (format out "  ~(~a~) = get_from_memory_pool(:~(~a~));~%" w w))
      (loop for line in (cl-ppcre:split "\\n" sched) do
        (format out "  ~a~%" line))
      (format out "}"))))

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
