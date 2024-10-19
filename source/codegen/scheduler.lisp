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
   #:caten/avm
   #:Buffer
   #:buffer-shape
   #:buffer-views
   #:buffer-nrank)
  (:import-from
   #:caten/codegen/shape-inference
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters
   #:buffer-merge-dims
   #:iteration-space
   #:iteration-space-shape
   #:iteration-shape-strides
   #:iteration-space-views)
  (:import-from
   #:caten/codegen/helpers
   #:nodes-depends-on
   #:nodes-write-to)
  (:export
   #:graph-schedule))

(in-package #:caten/codegen/scheduler)

(deftype Schedule-Type ()
  `(and keyword (member :reduction :element-wise :permute :shrink :allocate)))

(defnode (:GRAPH :Schedule-Item) ()
         "
name = the name of the kernel (a.k.a: the function name)
dst <- Schedule-Item(src)
items = a list of nodes to execute. the execution order does not matter.
storage-id-src: an indicator to the variable name. created by running memory-planner
storage-id-dst: an indicator to the variable name. created by running memory-planner
"
         :slots
         ((blueprint :type list :initform nil)
          (iterations :type list :initform nil)
          (buffers)
          (allocate-p :type boolean)
          (name :type symbol)
          (items :type list)
          (storage-id-src :type list)
          (storage-id-dst :type list)))

(defmethod print-node (node (id (eql :Schedule-Item)))
  (flet ((r (x y)
           (apply
            #'concatenate
            'string
            (butlast
             (loop for x1 in x
                   for nth upfrom 0
                   for y1 = (nth nth y)
                   if (or (eql x1 y1) (null y1))
                     append (list (format nil "~a" x1) ", ")
                   else
                     append (list (format nil "~a[~a]" x1 y1) ", "))))))
    (format nil "<[Schedule-Item] : ~a <- ~a where lowered-p=~a ~a>"
            (r (node-writes node) (getattr node :storage-id-dst))
            (r (node-reads node) (getattr node :storage-id-src))
            (if (getattr node :blueprint)
                "t" "nil")
            (if (getattr node :allocate-p)
                ":allocate-p=T"
                (format nil ":name=~a" (getattr node :name))))))

(defstruct Group
  (key (gensym) :type symbol)
  (iterators nil :type list)
  (items nil :type list)
  (reduced nil :type boolean)
  (view-objects nil :type list))

(defmethod verify-group ((group Group))
  (when (find :Allocate (group-items group) :key #'node-type)
    (assert (= (length (group-items group)) 1) () "Allocate should be scheduled standalone")))

(defmethod make-unique-schedule-name ((group Group))
  (gensym
   (with-output-to-string (out)
     (princ "FUSED" out)
     (dolist (c (group-items group))
       (format out "_~a" (node-type c))))))

(defmethod group->schedule ((group Group))
  (let ((reads (nodes-depends-on (group-items group)))
        (writes (nodes-write-to (group-items group)))
        (allocate-p (find :Allocate (group-items group) :key #'node-type)))
    (make-node :GRAPH :Schedule-Item writes reads :name (make-unique-schedule-name group)
               :allocate-p (when allocate-p t)
               :storage-id-dst writes
               :storage-id-src reads
               :items (group-items group))))

(defmethod group-mergeable-p ((group Group) read graph read-type ri)
  (let ((node (id->value graph read)))
    (when (null node) (return-from group-mergeable-p nil))
    (when (eql (node-type node) :Allocate) (return-from group-mergeable-p nil))
    (when (and (group-reduced group) (getattr node :reduction :allow-undefined t))
      (return-from group-mergeable-p nil))
    (let ((write-type (car (relay-writes (read-type-relay node))))
          (wi (car (relay-write-iters (read-type-relay node)))))
      ;; T=0 | A[write_type] = ...
      ;; T=1 | x[...] = node(... A[read_type])
      ;; write_type and read_type could be merged if they are located in the same group?
      ;; 1. Merge element-wise and non-viewed operations
      (flet ((base-p (view) (or (null view) (every #'null view))))
        (when (and (base-p (buffer-views read-type)) (buffer-views write-type))
          (return-from group-mergeable-p t)))
      ;; [TODO]
      ;; Test w/ transposed gemm and ConvND
      ;; T=0 |  wi  = ...
      ;; T=1 | ...  = f(..., ri, ...)
      ;; Consider this kernel is valid when T0 and T1 belongs to the same iteration domain.
      ;; for (int idx = ...; ... ; ...);
      ;;   T=0 | wi = ...
      ;;   T=1 | ... = f(..., ri, ...)
      ;; If valid, they should be grouped to the same Group
      ;; Otherwise, they should be separated, and never fused.
      ;; Note that applying the permutation to `wi` is allowed.
      ;; For example, T0 can T1 can be grouped if you permute T0.
      ;; T=0 | T0(c1, c2)    T0(c2, c1)
      ;; T=1 | T1(c2, c1) => T1(c2, c1)
      ;; ?
      (flet ((elwise-p (x)
               (and (= (length (iteration-space-views x)) 1)
                    (every #'null (iteration-space-views x)))))
        (when (or (elwise-p ri) (elwise-p wi))
          (return-from group-mergeable-p t)))
      t)))

;; (defparameter *model* (Transformer 64 4 2 1e-5 32))
;; (caten/codegen:jit (time (caten (call *model* (make-tensor `(1 10)) (iconst 'n)))))

(defun schedule-groups (parent parent-groups)
  (flet ((f (x) (when x (when (not (eql (group-key parent) (group-key x))) x))))
    (let ((lst (append (list parent) (map 'list (alexandria:compose #'f #'car) parent-groups) (apply #'append (map 'list #'cdr parent-groups)))))
      (remove-duplicates (loop for l in lst if l collect l) :key #'group-key))))

(defmethod group-update-iterators ((group Group) (is Iteration-Space))
  (when (null (group-iterators group))
    (setf (group-iterators group) (iteration-space-shape is))
    (return-from group-update-iterators))
  ;; Flattened + Flattened -> Flattened
  (when (= 1 (length (group-iterators group)) (length (iteration-space-shape is)))
    (return-from group-update-iterators))
  ;; Flattened + Complicated -> Complicated
  (when (= (length (group-iterators group)) 1)
    (setf (group-iterators group) (iteration-space-shape is))
    (return-from group-update-iterators))
  ;; Complicated + Complicated - >Merge
  ;;(print "+++MERGE+++")
  ;; Total sizeがonazininaruyounisuru (they are broadcasted)
  ;; (10 10)   (1 1)
  ;; (10 10)   (1 1)
  ;; (10 10)   (10 10) <- VIEW
  ;;     \     /
  ;;     (10 10)
  ;;       |
  ;;     [VIEW]
  ;;       |
  ;;     (1 1)
  ;;       |
  ;;     [sin]
  ;;       |
  ;;     (1 1)
  ;(print (group-iterators group))
  ;(print is)
  )

;;(with-no-grad
;; (time (caten/codegen:jit (caten (!sin (!view (!add (make-tensor `(1 1)) (make-tensor `(3 3) :initial-element 1.0)) `(0 2) 1))))))
(defmethod group-add-node ((group Group) node)
  (push node (group-items group))
  (dolist (r (relay-read-iters (read-type-relay node)))
    (when r (group-update-iterators group r)))
  (dolist (r (relay-write-iters (read-type-relay node)))
    (when r (group-update-iterators group r))))

;; BroadcastとReshapeは同時に発生しうるので無理
(defmethod identify-view-type ((view Node))
  (assert (eql :VIEW (node-type view)))
  (when (some #'identity (getattr view :broadcast))
    (return-from identify-view-type :broadcast))
  (when (getattr view :permute)
    (return-from identify-view-type :permute))
  (flet ((shrink-p (size view)
           (assert (= (length view) 4) () "not a view")
           (multiple-value-bind (from to by broadcast) (apply #'values view)
             (assert (null broadcast))
             (or
              (not (eql from 0))
              (not (eql to size))
              (not (eql by 1))))))
    (let* ((base-buffer (car (relay-reads (read-type-relay view))))
           (views (buffer-views (car (relay-writes (read-type-relay view))))))
      (when (and
             (= (buffer-nrank base-buffer) (getattr view :nrank))
             (some #'shrink-p (buffer-shape base-buffer) views))
        (return-from identify-view-type :shrink)))
    ;; Reshape = operation that changes the stride
    :reshape))

(defun view-type-list (views) (map 'list #'identify-view-type views))

(defun recursive-create-group (id graph &key (seen (make-hash-table)) (parent (make-group)))
  "
What items are scheduled to the same loop?
Do not consider about the access dependencies.

The more fused kernels the better, Loop Fission by ISL Scheduler

;; ReduceとElemmentWiseをつくっけたデータ構造(Group)を作る, (1 group 1 reduce, no dependency breaks)
;; Group <-> GroupでLoop Fusionを考える

- Embedding後のContiguousがくっつく場所をちゃんと考える
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
      (group-add-node parent node)
      ;; Reduce
      ;; out1 = 0.0
      ;; ID <- NODE(out1, arg1, arg2) ...
      ;; ElementWise
      ;; arg3[0:10] = sin(x[...])
      ;; ID <- NODE(arg1, arg2, arg3[10:0:-1])...
      (schedule-groups
       parent
       ;; :Allocate should be scheduled standalone
       ;; [IMPORTANT]
       ;; Smaller Rank -> Higher Ranknihanaranai
       ;; HigherRank -> SmallerRankのPathを見つけたら，Higher RankにShapeを統一する。
       ;; その過程でPermuteを復元してもいいかもしれない・・・
       (loop with buffer-p = (eql (node-type node) :Allocate)
             for read in (node-reads node)
             for read-type in (relay-reads (read-type-relay node))
             for ri in (relay-read-iters (read-type-relay node))
             for views in (getattr node :_read_views)
             for view-objs = (when views (view-type-list views))
             for view-mg-p = (mergeable-view-p parent read graph view-objs)
             for mergeable-p = (group-mergeable-p parent read graph read-type ri)
             if (and (null buffer-p) mergeable-p)
               collect (recursive-create-group read graph :seen seen :parent parent)
             else
               collect (recursive-create-group read graph :seen seen))))))

(defgeneric graph-schedule (graph) (:documentation "Returns a scheduled each node is `FastGraph` consisted of :Schedule-Item."))

;; [TODO] Add :FUNCALL node like :FUNCALL :name=embedding, this is not JIT and independentantly schedules
;; [TODO] Support multiple outputs
;; Fuse Symbolic !randn < 1 kernels (and it means a success)
;; Loop Collapse
;; breathe first search

;; (Add (Embedding Embedding))
;; (defsimplifier serialize ...
;; [Note] :shrink MOVE is mergeable

;; Pattern Matcher
;; :Reduce :Reduce |
;;   :ELemwise     |
;; Plan:
;; 1. Schedule-Item: :Reduce/:Permute/:Reshapeとかを一つだけ持つ
;; 2. Pattern Matcher

;; :Reduce -> :Reduce ...
;; for ...
;;;  ...
;;  end
;;; for ...
;;   ...
;;; end
;; みたいにしてMerge可能

;; BP Lowererはこの方針で決定，Add Embedding EmbeddingをFuseする
(defsimplifier
    (schedule-graph-rewriter)
    ;; ここでLoop Forの生成をする，失敗にしたらNILになる
    )

;; Itersize >= になるAssertを作る

(defmethod graph-schedule ((graph Graph))
  ;; Split the graph into multiple graphs
  (let* ((seen (make-hash-table))
         (groups (apply #'append (map 'list #'(lambda (x) (nreverse (recursive-create-group x graph :seen seen))) (graph-outputs graph)))))
    (mapc #'verify-group groups)
    ;; Serialize ADD (Embedding Embedding)
    ;; Merge two independent groups
    (print "SCHEDULED")
    (dolist (g groups)
      (when (not (eql (node-type (car (group-items g))) :Allocate))
        (print g)))
    (let ((schedule (apply #'make-graph (map 'list #'group->schedule groups))))
      (setf (graph-outputs schedule) (graph-outputs graph))
      (setf schedule (->fast-graph schedule))
      (print schedule)
      ;; [TODO] (Add (Embedding[Reduce] Embedding[Reduce])) Fusion Using Pattern Matcher
      schedule)))

;; [TODO] 明日やる　↓をADD REDUCE REDUCEにSchedule
;; (with-no-grad
;  (Time (caten/codegen:jit (caten (!add (forward (Embedding 10 10) (make-tensor `(10 10))) (forward (Embedding 10 10) (make-tensor `(10 10))))))))
