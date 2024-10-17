(defpackage #:caten/codegen/scheduler
  (:documentation "
The scheduler is responsible for converting the `caten/aasm` graph into a list of Schedule-Item.
One Schedule-item corresponds to one kernel in GPU.
")
  (:use :cl :caten/air)
  (:import-from
   #:caten/air
   #:defnode
   #:Node
   #:node-type
   
   #:FastGraph
   #:graph-outputs
   #:id->value
   #:id->users)
  (:import-from
   #:caten/codegen/shape-inference
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:buffer-merge-dims)
  (:export
   #:graph-schedule))

(in-package #:caten/codegen/scheduler)

(defnode (:GRAPH :Schedule-Item) ()
         "
name = the name of the kernel (a.k.a: the function name)
dst <- Schedule-Item(src)
items = a list of nodes to execute. the execution order does not matter.
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

(defun si-append (si item)
  (declare (type Node item))
  (assert (eql (node-type si) :Schedule-Item))
  (if (eql (node-type item) :Allocate)
      (push item (getattr si :buffers))
      (push item (getattr si :items))))

(defun recursive-schedule (graph id &key (seen (make-hash-table)))
  "
What items are scheduled to the same loop?
Do not consider about the access dependencies.

;; allocateから伸びるノードは分割する
;; reshape/permuteのMergeabilityを考慮する
;; the more fuse the better, loop fisson by ISL
;; 
"
  (declare (type graph Graph))
  ;; allbufs
  (let ((bufs (loop for n in (graph-nodes graph)
                    append (relay-reads (read-type-relay n)))))
    (dolist (b bufs)
      (when b
        (multiple-value-bind (s stride) (buffer-merge-dims graph b)
          (print s)
          (print stride)))))
  ;; Find all binary ops whose :reduction is T, and pair them with element-wise operations
  )

(defgeneric graph-schedule (graph) (:documentation "Returns a scheduled each node is `FastGraph` consisted of :Schedule-Item."))

;; [TODO] Add :FUNCALL node like :FUNCALL :name=embedding, this is not JIT and independentantly schedules
;; [TODO] Support multiple outputs
;; Fuse Symbolic !randn < 1 kernels (and it means a success)
;; Loop Collapse
(defmethod graph-schedule ((graph Graph))
  ;; Split the graph into multiple graphs
  (let* ((seen (make-hash-table))
         (items (map 'list #'(lambda (x) (recursive-schedule graph x :seen seen)) (graph-outputs graph))))
    (print graph)
    ;; -> Split the items to resolve circular dependencies
    ;; -> Returns a graph
    items))
