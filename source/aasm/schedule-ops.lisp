(in-package :caten/aasm)
;; ~~ ScheduleGraph ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass ScheduleGraph (FastGraph)
  ;; [TODO]
  ;; - Symbolics (e.g.: DynamicShape)
  ;; - Inputs (e.g.: TensorInput)
  ((symbolic :initarg :symbolic :accessor schedule-graph-symbolcs)))

(defmethod print-object ((graph ScheduleGraph) stream)
  (print-unreadable-object (graph stream)
    (format stream "ScheduleGraph[() -> (~a)]~%" (render-list (graph-outputs graph)))
    (dolist (node (graph-nodes (->graph-with-tpsort graph)))
      (print-schedule node (node-type node) stream 4)
      (format stream "~%"))))

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
(defun $affine (writes reads &key (polyhedron) (blueprint) (reduction) (storage-map (make-hash-table)))
  (declare (type list writes reads) (type hash-table storage-map))
  (emit (make-node :Schedule :Affine writes reads :polyhedron polyhedron :blueprint blueprint :reduction reduction :storage-map storage-map)))

(defun $nonaffine (writes reads &key (items))
  (declare (type list writes reads))
  (emit (make-node :Schedule :NonAffine writes reads :items items)))

(defmethod print-schedule ((node Node) (id (eql :Affine)) stream indent)
  (labels ((indent (&optional (indent indent)) (dotimes (i indent) (princ " " stream))))
    (indent) (format stream "~a = Affine(~a){~%" (render-list (node-writes node)) (render-list (node-reads node)))
    (let ((sched
            (uiop:symbol-call
             :caten/codegen/search/ast :ast->str
             (uiop:symbol-call
              :caten/codegen/search/ast
              :compute-ast-from-schedule
              (uiop:symbol-call
               :caten/codegen/search/polyhedral
               :psi-theta (getattr node :polyhedron)))
             :polyhedron (getattr node :polyhedron)
             :indent (+ indent 2))))
      (dolist (w (node-writes node))
        (indent (+ indent 2))
        (format stream "~(~a~) = get_from_memory_pool(:~(~a~));~%" w (gethash w (getattr node :storage-map) w)))
      (princ sched stream)
      (indent (+ indent 2)) (format stream "return ~(~a~);" (render-list (node-writes node)))
      (format stream "~%")
      (indent) (format stream "}"))))

(defmethod print-schedule ((node Node) (id (eql :NonAffine)) stream indent)
  (labels ((indent (&optional (indent indent)) (dotimes (i indent) (princ " " stream))))
    (indent)
    (format stream "~a = NonAffine(~a){~%" (render-list (node-writes node)) (render-list (node-reads node)))
    (dolist (item (getattr node :items))
      (let ((prefix (if (find (car (node-writes item)) (node-writes node))
                        (format nil "return ")
                        (format nil "~(~a~) = " (render-list (node-writes item))))))
        (let ((attrs ""))
          (when (eql (node-type item) :LOAD)
            (setf attrs (format nil ", value=~a" (getattr item :value))))
          (indent (+ indent 2))
          (format stream "~a~(~a~)(~(~a~)~a);~%" prefix (node-type item) (render-list (node-reads item)) attrs))))
    (indent) (format stream "}")))

(defmethod print-node ((node node) (id (eql :Affine)))
  (with-output-to-string (out)
    (print-schedule node id out 0)))

(defmethod print-node ((node node) (id (eql :NonAffine)))
  (with-output-to-string (out)
    (print-schedule node id out 0)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
