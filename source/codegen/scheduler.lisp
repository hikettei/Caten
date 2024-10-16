(defpackage #:caten/codegen/scheduler
  (:documentation "
The scheduler is responsible for converting the `caten/aasm` graph into a list of Schedule-Item.
One Schedule-item corresponds to one kernel in GPU.
")
  (:use :cl :caten/air)
  (:import-from
   #:caten/air
   #:defnode

   #:FastGraph
   #:graph-outputs
   #:id->value
   #:id->users)
  (:export
   #:graph-schedule))

(in-package #:caten/codegen/scheduler)

(defnode (:GRAPH :Schedule-Item) ()
         "
name = the name of the kernel (a.k.a: the function name)
dst <- Schedule-Item(src)
items = a list of nodes to execute, sorted by the execution order.
storage-id-src: an indicator to the variable name. created by running memory-planner
storage-id-dst: an indicator to the variable name. created by running memory-planner
"
         :slots
         ((buffers)
          (name :type string)
          (items :type list)
          (storage-id-src :type list)
          (storage-id-dst :type list)))

;; print scheduled item
(defun %schedule-item (items)

  )

(defun recursive-schedule (graph id &key (seen (make-hash-table)))
  "
What items are scheduled to the same loop?
Do not consider about the access dependencies.
"
  (declare (type graph Graph))
  
  )

(defgeneric graph-schedule (graph) (:documentation "Returns a scheduled each node is `FastGraph` consisted of :Schedule-Item."))

(defmethod graph-schedule ((graph Graph)) (error "Cannot schedule a graph that is not FastGraph(DAG)!"))

;; [TODO] Add :FUNCALL node like :FUNCALL :name=embedding, this is not JIT and independentantly schedules
;; [TODO] Support multiple outputs
;; Fuse Symbolic !randn < 1 kernels (and it means a success)
;; Loop Collapse
(defmethod graph-schedule ((graph FastGraph))
  ;; Split the graph into multiple graphs
  (let* ((seen (make-hash-table))
         (items (map 'list #'(lambda (x) (recursive-schedule graph x :seen seen)) (graph-outputs graph))))
    (print graph)
    ;; -> Returns a graph
    items))
