(defpackage #:caten/codegen/scheduler
  (:documentation "
`caten/codegen/scheduler` is responsible for partitioning a (huge amount of!) computation graph into several smaller subgraphs with the same iteration space.

`graph-schedule` as en entry point, it receives a graph of `caten/aasm` created by running `caten/apis/iseq.lisp`, and returns a graph of `Schedule-Item`.
One Schedule-Item corresponds to one kernel in GPU. Therefore, in general, the more computation grouped in the same group, the better, in term of the memory-locality. Loops may be distributed elsewhere, but never fused except for `recursive-create-group`.")
  (:use :cl :caten/air :caten/codegen/expr)
  (:import-from :caten/aasm #:JITAble)
  (:import-from
   #:caten/air
   #:defnode
   #:Node
   #:node-type
   #:node-attr
   #:FastGraph
   #:graph-outputs
   #:id->value
   #:id->users)
  (:import-from
   #:caten/avm
   #:Buffer
   #:buffer-dtype
   #:buffer-shape
   #:buffer-stride
   #:buffer-views
   #:buffer-nrank
   #:buffer-inferred-permute)
  (:import-from
   #:caten/codegen/shape-inference
   #:mergeable-view-p
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters
   #:buffer-merge-dims
   #:iteration-space
   #:iteration-space-shape
   #:iteration-space-strides
   #:iteration-space-views
   #:iteration-space-procedure)
  (:import-from
   #:caten/codegen/helpers
   #:nodes-depends-on
   #:nodes-write-to)
  (:import-from
   #:caten/codegen/rewriting-rules
   :nodes-apply-static-gensym)
  (:export
   #:graph-schedule))

(in-package #:caten/codegen/scheduler)

(defnode (:GRAPH :Schedule-Item) ()
         "
Schedule-Item is an intermidate object to represent a one kernel in GPU.

It has a unique `name`, and `cache-name`. If `cache-name` was assigned, the compiler will fail to compile this schedule-item and reuse the kernel named `cache-
name` instead.

In order to lowering the computation graph as the foreign language, `items` must be consisted of JITAble operations (except for special irs and :allocate). If it qualifies, `jitable` is set to T.

Otherwise, the scheduled items are relocated to the compiled avm directly. Specifially, if the item was :ALLOCATE, :allocated-p is set to T.

- blueprint[list] is a lowered schedule-item
- polyhedral[list] is a Polyhedral IR obtained by lowering blueprint
- auto-schedule-p[list] is set to T if it is worth to run auto-scheduler. (If there is a symbolic incremental, loop is not an affine and cannot run isl scheduler)
- items[list] are the scheduled items
- items-to-cache[list] are the copies of items but having the unique read/write. It is used to determine the equivalence of the two schedule-items.
- rank[fixnum] is the highest rank of the iteration space.
- storage-id-src[list] is the list of the storage-id of the source buffer (optimized by running memory-planner)
- storage-id-dst[list] is the list of the storage-id of the destination buffer (optimized by running memory-planner)
"
         :slots
         ((blueprint :type list :initform nil)
          (polyhedral)
          (jitable :type boolean)
          (allocate-p :type boolean)
          (auto-schedule-p :type boolean)
          (name :type symbol) (cache-name :type symbol)
          (items :type list) (items-to-cache :type list)
          (rank :type fixnum)
          (read-types :type list) (write-types :type list)
          (storage-id-src :type list)
          (storage-id-dst :type list)
          
          (rendered-object)
          (compiled-object)))

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
            (if (getattr node :allocate-p)
                (subseq (node-reads (car (getattr node :items))) 0 (getattr (car (getattr node :items)) :nrank))
                (r (node-reads node) (getattr node :storage-id-src)))
            (if (getattr node :blueprint)
                "t" "nil")
            (if (getattr node :allocate-p)
                ":allocate-p=T"
                (format nil ":name=~a" (getattr node :name))))))

(defmethod identify-view-type ((view Node))
  ;; [TODO] Rename :broadcast/:reshape as a :broadcast-or-reshape
  (assert (eql :VIEW (node-type view)))
  (when (some #'identity (getattr view :broadcast))
    ;; Note that this is relies on the assertion that: Slice and Broadcast are not mixed in `!view`.
    (return-from identify-view-type :broadcast)) ;; [Note] Broadcast may change the shape!
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

(defun jitable-p (node)
  (and
   (null (find (node-type node) `(:ALLOCATE :PAUSE/BACKWARD)))
   (typep (node-attr node) 'JITAble)))

(defun view-type-list (views)
  (loop for v in views if v collect (identify-view-type v)))

(defstruct Group
  (key (gensym) :type symbol)
  (reduce-dims nil :type list)
  (items nil :type list))

(defmethod node-reduce-axes ((node Node))
  (when (getattr node :reduction :allow-undefined t)
    (let ((write-buffer (car (relay-writes (read-type-relay node)))))
      (let ((out
              (loop for v in (buffer-views write-buffer)
                    if (fourth v)
                      collect t
                    else
                      collect nil)))
        (when (some #'identity out) out)))))

(defmethod verify-group ((group Group))
  (when (find :Allocate (group-items group) :key #'node-type)
    (assert (= (length (group-items group)) 1) () "Allocate should be scheduled standalone")))

(defun pname (name)
  (cl-ppcre:regex-replace-all
   "PAUSE/"
   (cl-ppcre:regex-replace-all "-" (cl-ppcre:regex-replace-all "GRAPH/" (princ-to-string name) "") "_")
   ""))

(defmethod make-unique-schedule-name ((group Group))
  (let ((names) (seen))
    (dolist (item (group-items group))
      (if (and (typep (node-attr item) 'JITAble) (car (getattr item :_lowering_history)))
          (multiple-value-bind (name id) (values (caar (getattr item :_lowering_history)) (cdar (getattr item :_lowering_history)))
            (when (null (find id seen))
              (push id seen)
              (push (pname name) names)))
          (push (pname (node-type item)) names)))
    (gensym
     (with-output-to-string (out)
       (princ "FUSED" out)
       (dolist (n names)
         (format out "_~a" n))))))

(defmethod group->schedule ((group Group))
  (let ((reads (nodes-depends-on (group-items group)))
        (writes (nodes-write-to (group-items group)))
        (allocate-p (find :Allocate (group-items group) :key #'node-type))
        (no-symbolic-incremental-p t)
        (full-scalar-p t) (rank 0))
    (dolist (node (group-items group))
      (dolist (r (append (relay-reads (read-type-relay node)) (relay-writes (read-type-relay node))))
        (when r
          (when (> (buffer-nrank r) 0)
            (setf full-scalar-p nil))
          (setf rank (max rank (buffer-nrank r)))
          (dolist (v (buffer-views r))
            (when (and v (third v) (symbolp (third v))) ;; v=(upfrom below by broadcast_p)
              (setf no-symbolic-incremental-p nil))))))
    (make-node :GRAPH :Schedule-Item writes reads :name (make-unique-schedule-name group)
               :jitable (and (every #'jitable-p (group-items group)) (null full-scalar-p))
               :allocate-p (when allocate-p t)
               :auto-schedule-p (and no-symbolic-incremental-p (null full-scalar-p))
               :storage-id-dst writes
               :storage-id-src reads
               :rank rank
               :items (group-items group)
               :items-to-cache (nodes-apply-static-gensym (map 'list #'copy-node (group-items group))))))

(defun broadcastable-p (t1 t2)
  (flet ((butone (s)
           (loop for x in s unless (eql x 1) collect x)))
    (equal (butone (buffer-shape t1)) (butone (buffer-shape t2)))))

(defmethod group-mergeable-p ((group Group) graph read read-type ri read-views)
  "
Returns T if it is valid to merge the access from R to W without transforming the views of R or W.
```
T=0 | W = f1(...)
T=1 | ... = f2(..., R(storage_id=W))
```
"
  (let ((node (id->value graph read)))
    (when (null node) (return-from group-mergeable-p nil))
    (when (eql (node-type node) :Allocate) (return-from group-mergeable-p nil))
    (when (not (jitable-p node)) (return-from group-mergeable-p nil))
    (let ((write-type (car (relay-writes (read-type-relay node))))
          (wi (car (relay-write-iters (read-type-relay node)))))
      (when (or (null ri) (null wi)) (return-from group-mergeable-p nil))
      ;; T=0 | A[write_type] = ...
      ;; T=1 | x[...] = node(... A[read_type])
      ;; 1. Merge element-wise and non-viewed operations
      (flet ((base-p (view) (or (null view) (every #'null view))))
        (when (and (base-p (buffer-views read-type)) (base-p (buffer-views write-type)))
          ;; For debugging...
          ;;(assert (= (buffer-nrank read-type) (buffer-nrank write-type)))
          (return-from group-mergeable-p t)))
      ;; when :read is shrink -> separate
      (when (find :shrink (view-type-list read-views))
        (return-from group-mergeable-p nil))
      (when (find :permute (view-type-list read-views))
        (return-from group-mergeable-p nil))
      ;; T=0 |  wi  = ...
      ;; T=1 | ...  = f(..., ri, ...)
      ;; Consider this kernel is valid when T0 and T1 belongs to the same iteration domain.
      ;; for (int idx = ...; ... ; ...);
      ;;   T=0 | wi = ...
      ;;   T=1 | ... = f(..., ri, ...)
      ;; If valid, they should be grouped to the same Group
      ;; Otherwise, they should be separated, and never fused in the single kernel.
      (flet ((elwise-p (x)
               (and (= (length (iteration-space-views x)) 1)
                    (every #'mergeable-view-p (iteration-space-views x) (iteration-space-shape x)))))
        (when (and
               (or (elwise-p ri) (elwise-p wi))
               ;; originated from the same iteration space?
               (= (buffer-nrank write-type) (buffer-nrank read-type)))
          (return-from group-mergeable-p t)))
      (flet ((meq (a b)
               (or
                (expr-scalar-equivalent-p b (expr-const 1 :int64))
                (expr-scalar-equivalent-p a b))))
        ;; [TODO] Fix this line, this is import for merging (!gelu (!matmul ...))
        (when (and (or (= (buffer-nrank write-type) (buffer-nrank read-type))
                       (and
                        (= (length (iteration-space-shape wi)) (length (iteration-space-shape ri)))
                        (broadcastable-p write-type read-type)))
                   (every #'meq (iteration-space-shape wi) (iteration-space-shape ri)))
          (return-from group-mergeable-p t)))
;      (print "++MERGEABLE++")
;      (print read)
;      (print wi)
;      (print write-type)
;      (print ri)
;      (print read-type)
      nil)))

(defmethod transform-and-mergeable-p ((group Group) graph read read-type ri read-views)
  "
Returns T if it is valid to merge the access from R to W with merging the views of R or W.
Trying to merge X and Y in the same group connected like:
```
   X --------
   |        |
 [MOVE]     |
   |        |
 [VIEW]     |
   |        | (Transform X and relocated to Y)
 [VIEW]     |
   |        |
 [VIEW]     |
   |        |
   Y <-------
```
"
  (when (not (symbolp read)) (return-from transform-and-mergeable-p nil))
  (when (find :shrink (view-type-list read-views)) (return-from transform-and-mergeable-p nil))
  (let ((node (id->value graph read)))
    (when (null node) (return-from transform-and-mergeable-p nil))
    (when (eql (node-type node) :Allocate) (return-from transform-and-mergeable-p nil))
    (when (not (jitable-p node)) (return-from transform-and-mergeable-p nil))
    ;; Only targeting !contiguous
    (when (not (eql (node-type node) :MOVE)) (return-from transform-and-mergeable-p nil))
    (let* ((write-type (car (relay-writes (read-type-relay node))))
           (write-views (car (getattr node :_write_views)))
           (wi (car (relay-write-iters (read-type-relay node))))
           (tmp (id->value graph (car (node-reads node))))
           (tgt-views (second (getattr node :_read_views)))
           (tgt-write-views (getattr node :_write_views))
           (tgt-rel (read-type-relay node))
           (tgt-type (second (relay-reads tgt-rel)))
           (tgt-is (second (relay-read-iters tgt-rel))))
      (when (or (null tmp) (not (eql (node-type tmp) :Allocate))) (return-from transform-and-mergeable-p nil))
      (when (not (every #'null tgt-write-views)) (return-from transform-and-mergeable-p nil))
      (when (not (every #'null (buffer-views write-type))) (return-from transform-and-mergeable-p nil))
      (when (not (every #'null write-views)) (return-from transform-and-mergeable-p nil))
      (when (not (eql (buffer-dtype read-type) (buffer-dtype write-type))) (return-from transform-and-mergeable-p nil))
      (when (not (eql (buffer-dtype tgt-type) (buffer-dtype write-type))) (return-from transform-and-mergeable-p nil))
      ;; T=0 | write[wi] = MOVE(some_temporary_buffer, tgt[ti])
      ;; T=1 | ...       =  f(..., read[ri], ...)
      ;; =>
      ;; T=0 | ... = f(..., tgt[new_iter], ...)
      ;; Algorithm adopted here are a little compicated, so the implementation is separated in `fusion-rules.lisp`
      (caten/codegen/fusion-rules:apply-fusion-rules
       graph
       (view-type-list tgt-views) tgt-views
       (view-type-list read-views) read-views
       read-type ri
       write-type wi
       tgt-type tgt-is))))

(defmethod group-add-node ((group Group) node)
  (push node (group-items group)))

(defun schedule-groups (parent parent-groups)
  (flet ((f (x) (when x (when (not (eql (group-key parent) (group-key x))) x))))
    (let ((lst (append (list parent) (map 'list (alexandria:compose #'f #'car) parent-groups) (apply #'append (map 'list #'cdr parent-groups)))))
      (remove-duplicates (loop for l in lst if l collect l) :key #'group-key))))

(defun merge-broadcast (shape list1 &key (default 1) &aux (list (copy-list list1)))
  (loop for s in shape
        if (eql s 1)
          collect default
        else
          collect (or (pop list) (error "merge-broadcast: cannot merge shape ~a and map ~a." shape list1))))

(defmethod group-fixup-rank ((group Group) graph)
  "Buffers in the same group should have the same ranked buffers.
Rewrite all buffer in the chain of element-wise ops to have the same rank.
```
read_id[wi]   <- F2(...,)
write_id[...] <- F1(..., read_id[ri])
```

(10 1 10)    (10 1 10)
    |     =>     |
 (10 10)     (10 1 10)
"
  (when (find :Allocate (group-items group) :key #'node-type) (return-from group-fixup-rank))
  (let* ((rank (loop for node in (group-items group)
                     maximize
                     (loop for r in (append (relay-reads (read-type-relay node)) (relay-writes (read-type-relay node)))
                           when r maximize
                                  (buffer-nrank r))))
         (common-shape (make-list rank)))
    (loop for node in (group-items group) do
      (loop for r in (append (relay-reads (read-type-relay node)) (relay-writes (read-type-relay node)))
            when r do
              (loop for axis upfrom 0
                    for s in (buffer-shape r)
                    do (setf (nth axis common-shape) (if (eql s 1) s (or (nth axis common-shape) s))))))
    (assert (every #'identity common-shape))
    (flet ((new (buffer)
             (values
              (merge-broadcast common-shape (buffer-shape buffer))
              (merge-broadcast common-shape (buffer-stride buffer))
              (if (not (every #'null (buffer-views buffer)))
                  (merge-broadcast
                   common-shape
                   (buffer-views buffer)
                   :default nil)
                  (loop repeat (length common-shape) collect nil)))))
      (labels ((rpl (bf)
                 (when (and bf (> (buffer-nrank bf) 0) (not (= (buffer-nrank bf) (length common-shape))))
                   (multiple-value-bind (new-shape new-stride new-views) (new bf)
                     (setf (buffer-shape bf) new-shape
                           (buffer-stride bf) new-stride
                           (buffer-views bf) new-views
                           (buffer-nrank bf) (length new-shape)))))
               (explore (node)
                 (loop for buf in (relay-writes (read-type-relay node))
                       for nth upfrom 0
                       when buf do
                         (rpl buf)
                         (setf (nth nth (relay-write-iters (read-type-relay node))) (buffer-merge-dims graph buf)))
                 (loop for buf in (relay-reads (read-type-relay node))
                       for nth upfrom 0
                       when buf do
                         (rpl buf)
                         (setf (nth nth (relay-read-iters (read-type-relay node))) (buffer-merge-dims graph buf)))))
        (mapc #'explore (group-items group))))))

(defmethod group-force-move-reduce-in-the-group ((group Group) graph read path-reduced)
  "Force merging MOVE(OUT, C) and Reduce
```
<Cluster-Out-Fusable>
                     Load(A, 0.0)
                          |  
    Reduce: C = BinaryOps(A, B) ---- <Injective>
            |
  MOVE(OUT, C)
            |
  <Complex-Out-Fusable>
```"
  (symbol-macrolet ((->ok (return-from group-force-move-reduce-in-the-group t)))
    (when (null path-reduced) ->ok)
    (let ((node (id->value graph read)))
      (when (or (null node) (not (eql (node-type node) :MOVE))) ->ok)
      (let ((reduction (id->value graph (second (node-reads node)))))
        (when (null reduction)->ok)
        (when (not (getattr reduction :reduction :allow-undefined t))->ok)
        nil))))

(defun force-merge-pattern-p (graph node read)
  "Force merging Load(A, 0.0) and BinaryOps"
  (when (getattr node :reduction :allow-undefined t)
    (let* ((load (id->value graph read)))
      (and load (eql (node-type load) :LOAD) (= (getattr load :value) 0)))))

(defun recursive-reduce-p (id graph &aux (seen))
  (declare (optimize (speed 3))
           (type graph graph)
           (type list seen))
  (labels ((explore (x)
             (when (and (symbolp x) (null (find x seen)))
               (push x seen)
               (let ((node (id->value graph x)))
                 (when node
                   (if (getattr node :reduction :allow-undefined t)
                       (return-from recursive-reduce-p (values (node-reduce-axes node) seen))
                       (mapc #'explore (node-reads node))))))))
    (explore id)
    nil))

(defmethod group-reduce-mergeable-p ((group group) graph node read path-reduced)
  (when (null (group-reduce-dims group)) (return-from group-reduce-mergeable-p t))
  (multiple-value-bind (red seen) (recursive-reduce-p read graph)
    (when (null red) (return-from group-reduce-mergeable-p t))
    (if (equal (group-reduce-dims group) red)
        t
        (when (getattr node :reduction :allow-undefined t)
          (find read seen)))))

(defun recursive-create-group (id graph &key (seen (make-hash-table)) (parent (make-group)) (path-reduced nil))
  "Breaks a big graph into small graphs by recursively exploring and creating subgraph."
  (declare (type graph Graph))
  (symbol-macrolet ((->failed (return-from recursive-create-group)))
    (when (gethash id seen)->failed)
    (let ((node (id->value graph id)))
      (when (null node)->failed)
      (setf (gethash id seen) t)
      (group-add-node parent node)
      (when (and (null (group-reduce-dims parent)) (getattr node :reduction :allow-undefined t))
        (setf (group-reduce-dims parent) (node-reduce-axes node)))
      (group-fixup-rank parent graph)
      (schedule-groups ;; merge+sort+cleanup
       parent
       ;; [Note] :Allocate is a vm instruction, accordingly should be scheduled standalone
       (loop with buffer-p = (eql (node-type node) :Allocate)
             with jitable-p = (jitable-p node)
             for read in (node-reads node)
             for read-type in (relay-reads (read-type-relay node))
             for ri in (relay-read-iters (read-type-relay node))
             for views in (getattr node :_read_views)
             for mergeable-p = (group-mergeable-p parent graph read read-type ri views)
             for reduce-mergeable-p = (group-reduce-mergeable-p parent graph node read path-reduced)
             for nth upfrom 0
             for force-group = (group-force-move-reduce-in-the-group parent graph read path-reduced)
             for force-p = (force-merge-pattern-p graph node read)
             if (or force-p (and jitable-p reduce-mergeable-p (null buffer-p) mergeable-p force-group)) ;; Elemwise or contiguous opfusion is here.
               collect (recursive-create-group read graph :seen seen :parent parent :path-reduced (or path-reduced (node-reduce-axes (id->value graph read))))
             else
               collect
               (multiple-value-bind (new-type new-is new-write-type new-write-is)
                   (and jitable-p reduce-mergeable-p force-group (transform-and-mergeable-p parent graph read read-type ri views))
                 (if (and jitable-p new-type new-is new-write-type new-write-is)
                     (let ((move (id->value graph read)))
                       (assert (and move (eql (node-type move) :MOVE)))
                       ;; Forcibly merging the next MOVE, btf the type is are rewritten as:
                       ;; write[new_write_type] = MOVE(alloc[new_write_type], tgt[new-type])
                       (setf (relay-writes (read-type-relay move)) (list new-write-type)
                             (relay-write-iters (read-type-relay move)) (list new-write-is)
                             (relay-reads (read-type-relay move)) (list new-write-type new-type)
                             (relay-read-iters (read-type-relay move)) (list new-write-is new-is))
                       (recursive-create-group read graph :seen seen :parent parent :path-reduced (or path-reduced (node-reduce-axes (id->value graph read)))))
                     ;; Complicated Fusion (e.g.: Elwise+Permute) is here.
                     (recursive-create-group read graph :seen seen))))))))

(defgeneric graph-schedule (graph) (:documentation "Returns a scheduled each node is `FastGraph` consisted of :Schedule-Item."))

(defmethod graph-schedule ((graph Graph))
  ;; Split the graph into multiple graphs
  (let* ((seen (make-hash-table))
         (groups (apply #'append (map 'list #'(lambda (x) (nreverse (recursive-create-group x graph :seen seen))) (graph-outputs graph)))))
    (mapc #'verify-group groups)
    ;; Serialize ADD (Embedding Embedding)
    ;; Merge two independent groups
    (when (>= (ctx:getenv :JIT_DEBUG) 4)
      (format t "[graph-schedule] Prescheduled ~a groups:~%" (length groups))
      (dolist (g groups)
        (when (not (eql (node-type (car (group-items g))) :Allocate))
          (print g)))
      (fresh-line))
    (let ((schedule (apply #'make-graph (map 'list #'group->schedule groups))))
      (setf (graph-outputs schedule) (graph-outputs graph))
      (setf schedule (->fast-graph schedule))
      (when (>= (ctx:getenv :JIT_DEBUG) 3)
        (format t "[graph-schedule] Schedule Graph:~%~a~%" schedule))
      schedule)))

;; Milestone (after complete them, move to implement renderer/memory planner)
;; - [ ] Getting a perfect schedule
;; - [ ] ConvND = 1 Kernel
;; - [ ] Getting a perfect tile/vectorize/parallelize
;; - [ ] Ez to deploy to gpu?

;; (caten/codegen:jit (caten (!add (!sin (make-tensor `(10 10))) (call (Embedding 10 10) (make-tensor `(10 10))))))
;; - [ ] Module Lowering is very slow
;; - [ ] Needs more optim
;; (caten/codegen:jit (caten (!argmax (!matmul (make-tensor `(10 32)) (call (layerNorm `(10)) (make-tensor `(10 32 10)))))))
;; (caten/codegen:jit (caten (!matmul (make-tensor `(10 32)) (!softmax (make-tensor `(32 10))))))
;; - 細かく分けて考えて，なぜSchedulingが失敗するか考えてみる
;;   - [x] !mean   | (caten/codegen:jit (caten (!mean (Make-tensor `(3 3 3)) :axis 0)))
;;   - [x] ConvND  | (stride is NIL?) -> OK
;;   - [ ] Transformer EntireGraph
;;   - [ ] Collapse (caten/codegen:jit (caten (call (Embedding 100 90) (Make-tensor `(b c)))))
;;   - [x] (caten/codegen:jit (caten (!contiguous (!t (!matmul (make-tensor `(10 10 10 10)) (!t (make-tensor `(10 10))))))))
;;   - [x] (caten/codegen:jit (caten (!mean (Make-tensor `(3 3 3)) :axis `(0 2))))
;;   - [ ] (serialize) (caten/codegen:jit (caten (!add (!softmax (make-tensor `(3 3))) (!softmax (make-tensor `(3 3))))))
;;   - [x] !randn (offsets ...)
;;   - [x] !argmax in a single kernel
;;   - [x] (!matmul (!matmul ... ...))
;;   - [ ] (caten/codegen:jit (caten (call (Attention 64 8 32) (make-tensor `(10 32 64)) (iconst 3))))
;;   - [ ] Propagate index-cmoponents
;;   - [x] If reduction, the position of axes must the same
;;   - [ ] (caten/codegen:jit (caten (!sum (!matmul (make-tensor `(10 10)) (!matmul (make-tensor `(10 10)) (make-tensor `(10 10)))))))
;;   - [x] (caten/codegen:jit (time (caten (call (LayerNorm `(10)) (call (Embedding 10 10) (make-tensor `(10 10)))))))
;;   - [x] randint
;;  -  [ ] (caten/codegen:jit (caten (!argmax (!matmul (make-tensor `(10 10)) (make-tensor `(10 10)))))) INDEX COMPONENTSがFuseされない・・・
;; - [ ] Running w/ tests?

;; - [ ] Schedule !mean in the single group (caten/codegen:jit (caten (!mean (Make-tensor `(3 3 3)) :axis 0))) also ids are invaild ... (should have a global hash table)
;; - [ ] Fix randn auto scheduler
;; - [ ] dont jit the scalar kernel
;; (with-no-grad (time (caten/codegen:jit (caten (!sin (!view (!add (make-tensor `(1 1)) (make-tensor `(3 3) :initial-element 1.0)) `(0 2) 1))))))
;; - [x] Insert 1 to proper position to determine the fused loop axis
;;   - [x] Let ConvND, and sin(matmul(x, y)) working
;;   - [x] Thinking ConvND step-by-step
;;     - [x] Step1: Reduction w/ permuted MULADD
;;     - [ ] Step2: Merge and purge multiple views (By permuting loops, they can be merged)
;; - [ ] Permutation Inference at scheduler.lisp level
;;   - [ ] Transposed Matmul < 1 Kernel
;; - [ ] Graph Partition
;; - [ ] Group Reduce Refactor
;; - [x] RMSNorm, Softmax, 1 Reduction 1 Group?
;; =====> That is
;; - [ ] Merge MOVE+Permutation into the same group by transform-and-mergeable-p, making ConvND, Transpose+Matmul < 1 Kernels
;; - [x] Redution+MOVE is a pair (caten/codegen:jit (caten (!add (forward (Embedding 10 10) (make-tensor `(10 10))) (forward (Embedding 10 10) (make-tensor `(10 10))))))
;;   - [ ] they are in the same group
;;   - [ ] (!gelu (!Matmul )) shape inference is still invaild
;; - [x] Softmax/RMSNorm Scheduling
;;   - [ ] Softmax: eliminate _gid1 (subsequence loops removed if all axes are broadcasted)
;; - [x] Allow double-reduce in the group
;;   - [ ] Proper Partition the :reduction in blueprint.lisp
;;   - [ ] Partitioning the entire graph w/o relying on reduction (=> Large Graph Partition)
;; - [ ] Symbolic
;;   - [ ] gensym <-> EXPRなTableを作りたい (cache stride computation)
;; - [ ] Scalar

;; [TODO]
;; - [x] Non JITAble standalone
;; - [x] Complete Expr Grouping
;; - [x] Fix randn, softmax (scalar load)
;; - [ ] Implement Renderer
;; - [ ] Fix poly (a*b) is not an affine
;; - [ ] Tiling/Parallelizing
;; - [ ] Vectorize


;; - Scheduler Remained stuff ...
;;  - [ ] Permute or View Fusion
;;  - [x] Fix !gelu (FIRST)
;;  - [ ] Large Graph Partition (Transformer!) (looks working well?)

;; -> Next ...
;;  - [ ] Expr Multi Grouping
;;  - [ ] Scalar Transform (backward)
;;  - [ ] AutoDiff
;;  - [ ] Symbolic
;;  - [ ] Auto Tuner
;; !gelu -> cannot repro?
;; (caten/codegen:jit (caten (!gelu (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25))))))
;; replacing forward -> call: failing
;; - [ ] Identify the equivalent kernel in the recursive structure network.
;;   - [ ] For Example: Transformer Layer is scheduled/compiled at once
;;   - [ ] Add: COMPILE_SPEED Option
;;   -   [ ]  Set=0 to no cache
;;   -   [ ]  Set=1 to cache only shapes are equivalent
;;   -   [ ]  Set=2 to aggresively cache them (force shapes to be a symbol)
;;   - [ ] AIR: From which module are they lowered?
;; (caten/codegen:jit (caten (!matmul (make-tensor `(10 10)) (!matmul (make-tensor `(10 10)) (make-tensor `(10 10))))))
;; (with-no-grad
;  (Time (caten/codegen:jit (caten (!add (forward (Embedding 10 10) (make-tensor `(10 10))) (forward (Embedding 10 10) (make-tensor `(10 10))))))))

;; - This is the case lowerer cannot handle well
;;   - (caten/codegen:jit (caten (!sin (!add (forward (Embedding 10 10) (make-tensor `(10 10))) (!sin (forward (Embedding 10 10) (make-tensor `(10 10))))))))
;; [!] Need process replay..
;; Involve the following things to the test
;; - [x] Initial Schedule
;;   - [x] Mean axis=0, axis=1,...
;;   - [x] Embedding
;;   - [x] Double Reduce (add embedding embedding), add matmul matmul
;;   - [x] Triple Reduce (add embedding embedding embedding)
;;   - [ ] WPE+WTE in Transformer is a single kernel.
;; - [ ] Permutation
;;   - [ ] Matmul, and ConvND
;; - [ ] Permute Fuse
;;   - [ ] Matmul+Transpose
;; - [ ] Graph Partition
;;   - [ ] Transfomer
;;   - [ ] Attention View will be properly scheduled?
;;   - [ ] Split the grpah as soon as :shrink was detected to schedule !randn
;; - [ ] Dynamic Shape
;;(with-no-grad (time (caten/codegen:jit (caten (!add (make-tensor `(3 3)) (!sin (make-tensor `(3))))))))

;; (defparameter *model* (Transformer 64 4 2 1e-5 32))
;; (caten/codegen:jit (time (caten (call *model* (make-tensor `(1 10)) (iconst 'n)))))
;; [TODO] Loopの操作はPolyhedral Compilerに任せる。。。
;; Optimal Embeddingが無理だったら，GIDを，Reduceが一番最後に来るようにPermuteする。
;; - [ ] relu(gemm)
;; - [ ] conv
;; (caten/codegen:jit (caten (call (TransformerBlock 64 4) (make-tensor `(10 10 64)) (iconst 2))))
