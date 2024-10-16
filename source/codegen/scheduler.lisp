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
items = a list of nodes to execute.
storage-id-src: an indicator to the variable name. created by running memory-planner
storage-id-dst: an indicator to the variable name. created by running memory-planner
"
         :slots
         ((name :type string)
          (items :type list)
          (storage-id-src :type list)
          (storage-id-dst :type list)))

(defun %schedule-item ()

  )


(defgeneric graph-schedule (graph) (:documentation "Returns a scheduled each node is `FastGraph` consisted of :Schedule-Item."))

(defmethod graph-schedule ((graph Graph)) (error "Cannot schedule a graph that is not FastGraph(DAG)!"))

(defmethod graph-schedule ((graph FastGraph))
  (let* ((seen (make-hash-table))
         (items (map 'list #'(lambda (x) (recursive-schedule x :seen seen)) (graph-outputs graph))))
    items))
