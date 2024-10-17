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

(defstruct Group
  (key (gensym) :type symbol)
  (items nil :type list)
  (reduced nil :type boolean))

(defmethod verify-group ((group Group))
  (when (find :Allocate (group-items group) :key #'node-type)
    (assert (= (length (group-items group)) 1) () "Allocate should be scheduled standalone")))

(defmethod group-mergeable-p ((group Group) read graph read-type)
  (let ((node (id->value graph read)))
    (when (null node) (return-from group-mergeable-p nil))
    (when (eql (node-type node) :Allocate) (return-from group-mergeable-p nil))
    (when (and (group-reduced group) (getattr node :reduction :allow-undefined t))
      (return-from group-mergeable-p nil))
    (let ((write-type (car (relay-writes (read-type-relay node)))))
      ;; T=0 | A[write_type] = ...
      ;; T=1 | x[...] = node(... A[read_type])
      ;; write_type and read_type could be merged if they are located in the same group?
      ;; Try permutation to A write (like i did in transform.lisp)
      
      t)))

(defun schedule-groups (list)
  (remove-duplicates (loop for l in list if l collect l) :key #'group-key))

(defun recursive-create-group (id graph &key (seen (make-hash-table)) (parent (make-group)))
  "
What items are scheduled to the same loop?
Do not consider about the access dependencies.

;; allocateから伸びるノードは分割する
;; reshape/permuteのMergeabilityを考慮する
;; the more fuse the better, loop fisson by ISL
;;

;; ReduceとElemmentWiseをつくっけたデータ構造(Group)を作る, (1 group 1 reduce, no dependency breaks)
;; Group <-> GroupでLoop Fusionを考える
"
  (declare (type graph Graph))
  (symbol-macrolet ((->failed (return-from recursive-create-group)))
    (when (gethash id seen)->failed)
    (let ((node (id->value graph id)))
      (when (null node)->failed)
      (setf (gethash id seen) t)
      (when (getattr node :reduction :allow-undefined t)
        (assert (null (group-reduced parent)) () "one group one reduction")
        (setf (group-reduced parent) t))
      (push node (group-items parent))
      ;; Reduce
      ;; out1 = 0.0
      ;; ID <- NODE(out1, arg1, arg2) ...
      ;; ElementWise
      ;; arg3[0:10] = sin(x[...])
      ;; ID <- NODE(arg1, arg2, arg3[10:0:-1])...
      (schedule-groups
       (append
        (list parent)
        ;; :Allocate should be scheduled standalone
        (loop with buffer-p = (eql (node-type node) :Allocate)
              for read in (node-reads node)
              for read-type in (relay-reads (read-type-relay node))
              for mergeable-p = (group-mergeable-p parent read graph read-type)
              if (and (null buffer-p) mergeable-p)
                append (recursive-create-group read graph :seen seen :parent parent)
              else
                append (recursive-create-group read graph :seen seen)))))))

(defgeneric graph-schedule (graph) (:documentation "Returns a scheduled each node is `FastGraph` consisted of :Schedule-Item."))

;; [TODO] Add :FUNCALL node like :FUNCALL :name=embedding, this is not JIT and independentantly schedules
;; [TODO] Support multiple outputs
;; Fuse Symbolic !randn < 1 kernels (and it means a success)
;; Loop Collapse
;; breathe first search
(defmethod graph-schedule ((graph Graph))
  ;; Split the graph into multiple graphs
  (let* ((seen (make-hash-table))
         (groups (apply #'append (map 'list #'(lambda (x) (recursive-create-group x graph :seen seen)) (graph-outputs graph)))))
    (mapc #'verify-group groups)

    ;; Serialize ADD (Embedding Embedding)
    (print groups)
    nil))
