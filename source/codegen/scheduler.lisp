(defpackage #:caten/codegen/scheduler
  (:documentation "
The scheduler is responsible for converting the `caten/aasm` graph into a list of Schedule-Item.
One Schedule-item corresponds to one kernel in GPU.
")
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
   #:buffer-shape
   #:buffer-stride
   #:buffer-views
   #:buffer-nrank
   #:buffer-inferred-permute)
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
   #:iteration-space-strides
   #:iteration-space-views
   #:iteration-space-procedure)
  (:import-from
   #:caten/codegen/helpers
   #:nodes-depends-on
   #:nodes-write-to)
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
         ((blueprint :type list :initform nil)
          (polyhedral)
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
            (if (getattr node :allocate-p)
                (subseq (node-reads (car (getattr node :items))) 0 (getattr (car (getattr node :items)) :nrank))
                (r (node-reads node) (getattr node :storage-id-src)))
            (if (getattr node :blueprint)
                "t" "nil")
            (if (getattr node :allocate-p)
                ":allocate-p=T"
                (format nil ":name=~a" (getattr node :name))))))

(defmethod identify-view-type ((view Node))
  ;; [TODO] Rename Broadcast/Reshape -> Broadcast_Or_Reshape
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

(defun view-type-list (views)
  (loop for v in views if v collect (identify-view-type v)))

(defstruct Group
  (key (gensym) :type symbol)
  (items nil :type list))

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
        (allocate-p (find :Allocate (group-items group) :key #'node-type)))
    (make-node :GRAPH :Schedule-Item writes reads :name (make-unique-schedule-name group)
               :allocate-p (when allocate-p t)
               :storage-id-dst writes
               :storage-id-src reads
               :items (group-items group))))

(defmethod group-mergeable-p ((group Group) graph read read-type ri read-views)
  "
Returns T if it is valid to merge the access from R to W without transforming R or W.
```
T=0 | W = f1(...)
T=1 | ... = f2(..., R(storage_id=W))
```
"
  (let ((node (id->value graph read)))
    (when (null node) (return-from group-mergeable-p nil))
    (when (eql (node-type node) :Allocate) (return-from group-mergeable-p nil))
    ;(when (and (group-reduced group) (getattr node :reduction :allow-undefined t))
    ;  (return-from group-mergeable-p nil))
    (let ((write-type (car (relay-writes (read-type-relay node))))
          (wi (car (relay-write-iters (read-type-relay node)))))
      ;; T=0 | A[write_type] = ...
      ;; T=1 | x[...] = node(... A[read_type])
      ;; 1. Merge element-wise and non-viewed operations
      (flet ((base-p (view) (or (null view) (every #'null view))))
        (when (and (base-p (buffer-views read-type)) (base-p (buffer-views write-type)))
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
                    (every #'null (iteration-space-views x)))))
        (when (and
               (or (elwise-p ri) (elwise-p wi))
               ;; originated from the same iteration space?
               (= (buffer-nrank write-type) (buffer-nrank read-type)))
          (return-from group-mergeable-p t)))
      (when (and (= (length (iteration-space-shape wi)) (length (iteration-space-shape ri)))
                 (every #'expr-scalar-equivalent-p (iteration-space-shape wi) (iteration-space-shape ri)))
        (return-from group-mergeable-p t))
      nil)))

(defmethod transform-and-mergeable-p ((group Group) graph read read-type ri read-views)
  "
Returns T if it is valid to merge the access from R to W with modifying R or W.
Trying to merge X and Y in the same group connected like:
```
   X
   |
 [VIEW]
   |
 [VIEW]
   |
 [VIEW]
   |
   Y
```
"
  (when (not (symbolp read)) (return-from transform-and-mergeable-p nil))
  (when (find :shrink (view-type-list read-views)) (return-from transform-and-mergeable-p nil))
  (let ((node (id->value graph read)))
    (when (null node) (return-from transform-and-mergeable-p nil))
    (when (eql (node-type node) :Allocate) (return-from transform-and-mergeable-p nil))
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
      ;; T=0 | write[wi] = MOVE(some_temporary_buffer, tgt[ti])
      ;; T=1 | ...       =  f(..., read[ri], ...)
      ;; =>
      ;; T=0 | ... = f(..., tgt[new_iter], ...)

      ;;  val_28[((((100*_gid0)+0)+_gid2)+(10*_gid3))]
      ;; -> val_28[0+0+gid2+10*_gid3]
      ;(print "+Mergeable?+")
      ;(print node)
      ;(print read-type)
      ;(print tgt-type)
      ;; tgt-typeをwrite-typeの次元数と一致したViewに書き換えることが目的
      (when (and
             (equal `(:broadcast) (view-type-list read-views))
             (equal nil (view-type-list write-views)))
        (cond
          ((equal `(:permute) (view-type-list tgt-views))
           
           )))
      nil)))

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

(defmethod group-fixup-uprank ((group Group) graph write-id read-id rt wt)
  "
Rewrite all buffer in the chain of element-wise ops w/ the highest rank.
```
read_id[wi]   <- F2(...,)
write_id[...] <- F1(..., read_id[ri])
```

(10 1 10)    (10 1 10)
    |     =>     |
 (10 10)     (10 1 10)
"
  (let ((common-shape (buffer-shape wt)))
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
                 (when (and bf (> (buffer-nrank bf) 0) (< (buffer-nrank bf) (length common-shape)))
                   (multiple-value-bind (new-shape new-stride new-views) (new bf)
                     (setf (buffer-shape bf) new-shape
                           (buffer-stride bf) new-stride
                           (buffer-views bf) new-views
                           (buffer-nrank bf) (length new-shape)))))
               (explore (node)
                 (dolist (r (relay-writes (read-type-relay node)))
                   (rpl r))
                 (dolist (r (relay-reads (read-type-relay node)))
                   (rpl r))))
        (mapc #'explore (group-items group))))))

(defmethod group-force-move-reduce-in-the-group ((group Group) graph read)
  (symbol-macrolet ((->ok (return-from group-force-move-reduce-in-the-group t)))
    (when (not (group-reduced group)) ->ok)
    (let ((node (id->value graph read)))
      (when (or (null node) (not (eql (node-type node) :MOVE))) ->ok)
      (let ((reduction (id->value graph (second (node-reads node)))))
        (when (null reduction)->ok)
        (when (not (getattr reduction :reduction :allow-undefined t))->ok)
        nil))))

(defun recursive-create-group (id graph &key (seen (make-hash-table)) (parent (make-group)))
  "Breaks a big graph to small graphs by recursively exploring and creating subgraph.
We refer to subgraph as:
- Total iteration size are equivalent. (that's why we break the graph at :shrink)
- the centroid is :reduce.

What items are scheduled to the same loop?
Do not consider about the access dependencies.

Generally the more fusion the better for us, loop fission by ISL Scheduler
"
  (declare (type graph Graph))
  (symbol-macrolet ((->failed (return-from recursive-create-group)))
    (when (gethash id seen)->failed)
    (let ((node (id->value graph id)))
      (when (null node)->failed)
      (setf (gethash id seen) t)
      (group-add-node parent node)
      (schedule-groups ;; merge+sort+cleanup
       parent
       ;; [Note] :Allocate is a vm instruction, accordingly should be scheduled standalone
       (loop with buffer-p = (eql (node-type node) :Allocate)
             for read in (node-reads node)
             for read-type in (relay-reads (read-type-relay node))
             for ri in (relay-read-iters (read-type-relay node))
             for views in (getattr node :_read_views)
             for mergeable-p = (group-mergeable-p parent graph read read-type ri views)
             if (and (null buffer-p) mergeable-p) ;; merged due to element-wise operation
               ;; Simple Contigous OpFusion is here
               do (group-fixup-uprank parent graph id read read-type (car (relay-writes (read-type-relay (id->value graph read)))))
               and collect (recursive-create-group read graph :seen seen :parent parent)
             else
               collect
               (let ((out (transform-and-mergeable-p parent graph read read-type ri views)))
                 (if out
                     (recursive-create-group read graph :seen seen :parent parent) ;; Complicated Fusion (e.g.: Elwise+Permute) is here.
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

;; [TODO] randn rng_counter -> _gid0=0..200のループの外側に出す
;; 1 kernel 1 reduction -> delete?
;; 1 path 1 reduction instead?
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
