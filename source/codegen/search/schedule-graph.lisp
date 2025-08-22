(defpackage :caten/codegen/schedule-graph
  (:use :cl :caten/air)
  (:export
   #:tensor-graph->schedule-graph))

(in-package :caten/codegen/schedule-graph)
;; ~~ Grids ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Grids
  (is-affine t :type boolean)
  (id 0 :type fixnum)
  ;; iterator
  (items nil :type list))
;; [TODO]
;; Binary/Ternary no case ha touzen broadcast suru hituyou ga aru
;; but: they are same-ranked so the code should be still simple...
;; %Grids-Coalesce
(defun make-grids-from-node (node id->grids)
  (declare (type node node))
  (case (node-type node)
    (:Allocate
     ;; If allocate produces scalar computation, no worth to explore it.
     (if (null (node-reads node))
         (make-grids :is-affine nil)
         (make-grids :is-affine t)))
    (:View
     ;; If view produces scalar computation (e.g.: A[0])

     )
    (otherwise
     (if (typep (node-attr node) 'caten/aasm::JITAble)
         (progn
           ;; BinaryOps?
           )
         (make-grids :is-affine nil :id 0 :items (list node))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 最後のViewも残せるようにしたい！ (e.g.: Matmul ...)
;; Allocate -> View Rewriting
;; [TODO] Run benchmark!
;; local-gensym on utils w/ type inference (e.g.: can generate string)
(defun tensor-graph->schedule-graph (graph)
  "Constructs ScheduleGraph from the given tensorgraph."
  (declare (type Graph graph) (optimize (speed 3)))
  (assert (null (graph-seen graph)) () "tensor-graph->schedule-graph: Scheduling partial graph is not allowed! Set graph-seen = nil")
  (print graph)
  (let ((id->grids (make-hash-table)) (scheduled) (queue)
        (in-degrees (make-hash-table)) (out-degrees (make-hash-table)))
    (flet ((butseen (list) (loop for l in list for v = (id->value graph l) if (and v (symbolp l)) collect v)))
      (loop for node in (graph-nodes graph) do
        (setf (gethash (node-id node) in-degrees) (butseen (node-reads node)))
        (dolist (r (butseen (node-reads node)))
          (when (null (find (the symbol (node-id node)) (the list (gethash (node-id r) out-degrees)) :key #'node-id))
            (push node (gethash (node-id r) out-degrees))))))
    ;; [TODO]
    ;; Insert (car (node-writes backward)) to node-reads of all backward nodes
    (loop for node in (graph-nodes graph)
          if (null (gethash (node-id node) in-degrees)) do
            (push node queue))
    (loop while queue
          for node = (pop queue)
          for new-grid = (make-grids-from-node node id->grids) do
            (dolist (w (node-writes node))
              (setf (gethash w id->grids) new-grid))
            (push node scheduled)
            (dolist (adj (gethash (node-id node) out-degrees))
              (setf (gethash (node-id adj) in-degrees) (remove (node-id node) (gethash (node-id adj) in-degrees) :key #'node-id))
              (when (null (gethash (node-id adj) in-degrees))
                (push adj queue)))
            (remhash (node-id node) out-degrees))
    (assert (= 0 (hash-table-count out-degrees)) ()
            "The following nodes are not scheduled. circular dependencies?~%~a" (alexandria:hash-table-values out-degrees))
    ;; Construct Graph
    (maphash
     #'(lambda (id grids)
         ;; each grid has unique id generated from gensym
         ;; OR, make it unique (add counter)
         ;; each kernel has name like
         ;; K_1
         )
     id->grids)
    ))


(defun schedule-graph-solve-ilp (graph)
  ;; Objective: Maximize the volume of Affine Nodes
  )
