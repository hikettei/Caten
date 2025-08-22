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
;; view w/o items  ==> rewrite as non-affine
;; bring back symbolic ...
;; scalar computation vs load
;; the accumlator should be mutated as scalar
;; - run tensor-relay-scalarify?
;; - or measure locality?
;;  ==> simplify ast.lisp
(defun make-grids-from-node (node id->grids)
  (declare (type node node))
  (let ((next-id (hash-table-count id->grids)))
    (case (node-type node)
      (:Allocate
       (if (node-reads node)
           (make-grids :id next-id :is-affine nil :items (list node))
           (make-grids :id next-id :is-affine t :items (list node))))
      (:View (make-grids :id next-id :is-affine t :items (list node)))
      (otherwise
       (if (typep (node-attr node) 'caten/aasm::JITAble)
           (let* ((parent-grids
                    (loop for r in (node-reads node)
                          for g = (gethash r id->grids)
                          if (and g (grids-is-affine g)) collect g))
                  (items (apply #'append (list node) (map 'list #'grids-items parent-grids)))
                  (new-grids
                    (make-grids :id next-id :items items)))
             (dolist (n items)
               (dolist (w (node-writes n))
                 (setf (gethash w id->grids) new-grids)))
             new-grids)
           (make-grids :id next-id :is-affine nil :id 0 :items (list node)))))))

(defun grids-init (grids id->grids id->users)
  (declare (type Grids grids) (type hash-table id->grids id->users) (optimize (speed 3)))
  ;; View w/o items ==> rewrite as non-affine
  ;; how to deal w/ ?
  ;; A -> [VIEW] -> [VIEW] -> B
  (when (and (= 1 (length (grids-items grids)))
             (eql :VIEW (node-type (car (grids-items grids)))))
    (setf (grids-is-affine grids) nil))
  (labels ((node-is-output-p (node)
             (some
              #'(lambda (usr)
                  (let ((usr (gethash (car (node-writes usr)) id->grids)))
                    (assert usr)
                    (not (= (grids-id grids) (grids-id usr)))))
              (gethash (car (node-writes node)) id->users))))
    (print (map 'list #'node-is-output-p (grids-items grids)))
    ;; [todo] all shape space should match here
    ;; [todo] coalesce
    ;; Construct blueprint?
    ))
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
  (let ((id->grids (make-hash-table)) (id->users (make-hash-table)) (queue)
        (in-degrees (make-hash-table)) (out-degrees (make-hash-table)))
    (flet ((butseen (list) (loop for l in list for v = (id->value graph l) if (and v (symbolp l)) collect v)))
      (loop for node in (graph-nodes graph) do
        (assert (= 1 (length (the list (node-writes node)))))
        (setf (gethash (node-id node) in-degrees) (butseen (node-reads node)))
        (dolist (r (butseen (node-reads node)))
          (let ((node-id (car (node-writes r))))
            (when (null (find (node-id node) (the list (gethash node-id id->users)) :key #'node-id))
              (push node (gethash node-id id->users))))
          (when (null (find (the symbol (node-id node)) (the list (gethash (node-id r) out-degrees)) :key #'node-id))
            (push node (gethash (node-id r) out-degrees))))))
    ;; [TODO]
    ;; Insert (car (node-writes backward)) to node-reads of all backward nodes
    (loop for node in (graph-nodes graph) if (null (gethash (node-id node) in-degrees)) do (push node queue))
    (loop while queue
          for node = (pop queue)
          for new-grid = (make-grids-from-node node id->grids) do
            (dolist (w (node-writes node)) (setf (gethash w id->grids) new-grid))
            (dolist (adj (gethash (node-id node) out-degrees))
              (setf (gethash (node-id adj) in-degrees) (remove (node-id node) (gethash (node-id adj) in-degrees) :key #'node-id))
              (when (null (gethash (node-id adj) in-degrees))
                (push adj queue)))
            (remhash (node-id node) out-degrees))
    (assert (= 0 (hash-table-count out-degrees)) ()
            "The following nodes are not scheduled. circular dependencies?~%~a" (alexandria:hash-table-values out-degrees))
    ;; Construct Graph
    (let ((all-grids (make-hash-table)) (n-scheduled 0))
      (declare (type fixnum n-scheduled))
      ;; circular dependency of schedule graph? will it happen?
      (maphash
       #'(lambda (id grids)
           (declare (ignore id))
           (setf (gethash (grids-id grids) all-grids) grids))
       id->grids)
      (maphash
       #'(lambda (id grids)
           (incf n-scheduled (length (the list (grids-items grids))))
           (grids-init grids id->grids id->users)
           (format t "~a -> ~a~%" id grids)
           )
       all-grids)
      (assert (= n-scheduled (length (the list (graph-nodes graph)))))
      )))

;; Solve Graph Partition Problem
(defun schedule-graph-solve-ilp (schedule-graph)
  ;; Objective: Maximize the volume of Affine Nodes
  ;; Firstly, single Affine single reduce
  ;; Secondly, fuse reduction and reduction
  )
