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
   #:copy-buffer
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
   #:range
   #:permute-list
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

(defstruct Group
  (key (gensym) :type symbol)
  (reduce-dims nil :type list)
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

(defmethod group-get-type ((group Group))
  (let* ((last (nodes-write-to (group-items group)))
         (node (when last (find (car last) (group-items group) :key #'node-writes :test #'find))))
    (when node
      (car (relay-writes (read-type-relay node))))))

(defun jitable-p (node)
  (and
   (null (find (node-type node) `(:ALLOCATE :PAUSE/BACKWARD)))
   (typep (node-attr node) 'JITAble)))

(defmethod node-reduce-axes ((node Node))
  (when (getattr node :reduction :allow-undefined t)
    (let ((write-buffer (car (relay-writes (read-type-relay node)))))
      (let ((out
              (loop for v in (buffer-views write-buffer)
                    for s in (buffer-shape write-buffer)
                    if (fourth v)
                      collect s
                    else
                      collect nil)))
        ;; Returns uncollapsed rank list
        (when (some #'identity out) out)))))

(defmethod group-items-st-rewriter ((group Group) f)
  (dolist (item (group-items group))
    (loop for typ in (relay-reads (read-type-relay item))
          for nth upfrom 0
          unless (or (null typ) (= 0 (buffer-nrank typ)))
            do (setf (nth nth (relay-reads (read-type-relay item))) (or (funcall f typ) typ)))
    (loop for typ in (relay-writes (read-type-relay item))
          for nth upfrom 0
          unless (or (null typ) (= 0 (buffer-nrank typ)))
            do (setf (nth nth (relay-writes (read-type-relay item))) (or (funcall f typ) typ)))))

(defun group-rank (group)
  (let ((buff (group-get-type group)))
    (when buff (buffer-nrank buff))))
;; (caten/codegen:jit (caten (!matmul (!tril (make-tensor `(10 1 1 10))) (!triu (make-tensor `(10 1))))))
(defun apply-view-fusor (tgt-rank mask group)
  ;; T = broadcasted, NIL = old axes2
  (group-items-st-rewriter
   group
   #'(lambda (typ)
       (when (= (buffer-nrank typ) tgt-rank)
         (let ((typ (copy-buffer typ)))
           (let ((shp (copy-list (buffer-shape typ)))
                 (str (copy-list (buffer-stride typ)))
                 (views (copy-list (buffer-views typ))))
             (setf (buffer-shape typ)
                   (loop for b in mask
                         if b collect 1 else collect (or (pop shp) 1))
                   (buffer-stride typ)
                   (loop for b in mask
                         if b collect 0 else collect (or (pop str) 1))
                   (buffer-views typ)
                   (loop for b in mask
                         if b collect `(0 1 1 t) else collect (pop views))
                   (buffer-nrank typ) (length (buffer-shape typ)))
             ;; Consumed all masks?
             (assert (= (buffer-nrank typ) (length mask))))
           typ)))))

(defun broadcastable-p (prev new)
  (let ((prev-shape (copy-list (buffer-shape prev)))
        (new-shape  (copy-list (buffer-shape new))))
    (let ((p (loop for p in prev-shape if (not (eql p 1)) collect p))
          (n (loop for n in new-shape if (not (eql n 1)) collect n)))
      (equal p n))))

(defun buffer-mergeable-p (g b1 b2)
  (flet ((lazy-eq (a b)
           (or (eql a 1) (eql b 1) (eql a b)
               (and (symbolp a) (symbolp b) (expr-scalar-equivalent-p (expr-from-graph a g) (expr-from-graph b g))))))
    (every #'lazy-eq (buffer-shape b1) (buffer-shape b2))))

(defun buffer-complex-out-fusable-p (g b1 b2 mask)
  "b1 = self, b2 = parent"
  (flet ((lazy-eq (a b a-view b-view m)
           (declare (ignore a-view))
           (if m
               (flet ((ok (size view)
                        (or (eql size 1) (fourth view) (eql m 1))))
                 (and ;; self (ok a a-view)
                      (ok b b-view)))
               (or (eql a 1) (eql b 1) (eql a b)
                   (and (symbolp a) (symbolp b) (expr-scalar-equivalent-p (expr-from-graph a g) (expr-from-graph b g)))))))
    (every #'lazy-eq (buffer-shape b1) (buffer-shape b2) (buffer-views b1) (buffer-views b2) mask)))

(defun group-assert-rank (group r1 r2 view &aux (rank (max r1 r2)))
  (loop for item in (group-items group)
        do (loop for typ in (append (relay-reads (read-type-relay item)) (relay-writes (read-type-relay item)))
                 for nth upfrom 0
                 unless (or (null typ) (= 0 (buffer-nrank typ)))
                   do (assert (= rank (buffer-nrank typ)) ()
                              "Rank mismatch: (expected from ~a -> ~a)~%view=~a~%buffer:~%~a~%group~%~a"
                              (min r1 r2) rank view typ group))))

(defmethod group-merge-p ((self Group) (graph Graph) (node Node) (parent-group Group) nth)
  (symbol-macrolet ((->ok
                      (progn
                        (setf (group-reduce-dims self) (or (group-reduce-dims self) (group-reduce-dims parent-group)))
                        (return-from group-merge-p t)))
                    (->ng (return-from group-merge-p nil)))
    (when (and (group-reduce-dims self) (group-reduce-dims parent-group))
      ;; Both groups are reduced?
      (when (not (equal (group-reduce-dims self) (group-reduce-dims parent-group)))
        ;; Reduced at the same rank?
        ->ng))
    (let* ((read (nth nth (node-reads node)))
           (read-node (id->value graph read))
           (read-view (car (nth nth (getattr node :_read_views))))
           (read-type (group-get-type parent-group))
           (write-view (getattr node :_write_views)))
      (assert (every #'null write-view))
      (assert (<= (length (nth nth (getattr node :_read_views))) 1))
      ;; Relations between group and parent-group:
      ;; ```
      ;; group=parent | X[write_type]{write_iter} = f(...)
      ;; group=self   | ... = f(..., X[read_type]{read_iter})
      ;; ```
      (when (or (not (jitable-p node)) (not (jitable-p read-node)))->ng)
      ;; ~~ merge views ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      (let ((r1 (group-rank self))
            (r2 (group-rank parent-group)))
        ;; r2 -> r1
        (cond
          ((or (= r1 0) (= r2 0))->ok)
          ((= r1 r2)
           (when (group-reduce-dims parent-group)
             (if (buffer-complex-out-fusable-p graph (group-get-type self) (group-get-type parent-group) (group-reduce-dims parent-group))
                 ->ok
                 ->ng))
           (if (buffer-mergeable-p graph (group-get-type parent-group) (group-get-type self))
               ->ok
               ->ng))
          (T
           (let ((self-type (group-get-type self))
                 (c (< r1 r2)))
             (when (null self-type)->ng)
             (if (broadcastable-p read-type self-type)
                 (let ((mask (map 'list #'(lambda (x) (eql x 1)) (buffer-shape (if c read-type self-type)))))
                   (assert (some #'identity mask))
                   (apply-view-fusor (min r1 r2) mask self)
                   (apply-view-fusor (min r1 r2) mask parent-group)
                   (group-assert-rank self r1 r2 read-view)
                   (group-assert-rank parent-group r1 r2 read-view)
                   ->ok)
                 (if (and read-view (some #'identity (getattr read-view :broadcast)))
                     (let ((mask (getattr read-view :broadcast)))
                       (when (not (= (length mask) (max r1 r2)))
                         (setf mask (map 'list #'fourth (buffer-views (if c read-type self-type)))))
                       (when (not (= (length mask) (max r1 r2)))->ng)
                       (apply-view-fusor (min r1 r2) mask self)
                       (apply-view-fusor (min r1 r2) mask parent-group)
                       (group-assert-rank self r1 r2 read-view)
                       (group-assert-rank parent-group r1 r2 read-view)
                       ->ok)
                     ->ng)))))))))

(defmethod merge-groups ((self Group) parents mergeable-list)
  (let* ((p (loop for m in mergeable-list for p in parents
                  if m collect p))
         (ranks (map 'list #'group-rank p))
         (srank  (group-rank self)))
    (loop for r in ranks
          for group in p
          for eql-p = (or (eql 0 r) (eql 0 srank) (= r srank))
          unless eql-p do
            (assert (< r srank))
            (let* ((typ1 (group-get-type self))
                   (m (map 'list #'fourth (buffer-views typ1))))
              (assert (some #'identity m))
              (apply-view-fusor r m group)
              (group-assert-rank group srank srank nil))))
  (loop for m in mergeable-list
        for p in parents
        if m do
          (assert (or (null (group-reduce-dims p)) (null (group-reduce-dims self)) (equal (group-reduce-dims self) (group-reduce-dims p)))
                  ()
                  "Reduce dims = ~a ~a" (group-reduce-dims p) (group-reduce-dims self))
          (setf (group-items self) (append (group-items p) (group-items self))
                (group-reduce-dims self) (or (group-reduce-dims self) (group-reduce-dims p))))
  self)

(defun recursive-create-groups (id graph &key (seen))
  (declare (type symbol id) (type graph graph) (type hash-table seen))
  (when (gethash id seen) (return-from recursive-create-groups))
  (setf (gethash id seen) t)
  (let* ((node (id->value graph id))
         (self
           (make-group
            :items (list node)
            :reduce-dims (node-reduce-axes node)))
         (parents
           (reverse (map 'list #'(lambda (x) (and (symbolp x) (recursive-create-groups x graph :seen seen))) (reverse (node-reads node))))))
    (declare (type node node) (type list parents))
    ;; Consider this structured graph:
    ;; parents[0] parents[1] parents[2] ...
    ;;        \      |      /
    ;;              self
    ;;               |
    ;; => The function returns this flattend list
    ;; (list               (list
    ;;   parents[0]          parent[2]
    ;;   parents[1]            ...
    ;;   parents[2]    =>    group(items=self+parent[0]+parent[1]))
    ;;     ...
    ;;   self)
    ;;  (No fuse)        (When parent[0] and parent[1] are fusable)
    (let ((mergeable-p-list
            (loop for parent in parents
                  for parent-return = (car parent)
                  for nth upfrom 0
                  if parent-return
                    collect (group-merge-p self graph node parent-return nth)
                  else
                   collect nil)))
      (assert (= (length mergeable-p-list) (length parents)))
      (append
       (list (merge-groups self (map 'list #'car parents) mergeable-p-list))
       (loop for p in parents
             for m in mergeable-p-list
             if m ;; mergeable
               append (cdr p)
             else ;; unmergeable
             append p)))))

(defgeneric graph-schedule (graph) (:documentation "Returns a scheduled each node is `FastGraph` consisted of :Schedule-Item."))

(defmethod graph-schedule ((graph Graph))
  ;; Split the graph into multiple graphs
  (let* ((seen (make-hash-table))
         (groups (apply #'append (map 'list #'(lambda (x) (recursive-create-groups x graph :seen seen)) (graph-outputs graph)))))
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

;; - (caten/codegen:jit (caten (!add (call (Embedding 10 10) (make-tensor `(10 10))) (forward (Embedding 10 10) (!cast (!add (iconst 'n) (!index-components `(1 10))) :float32)))))
;; - argmax = 1 kernels
;; [TODO] shape-inference.lisp => ShapeTrackerを作って回す？
;; - TensorComprehensionみたいなLowererが結局必要なのか。。。
;; - Shape-Inferece.lispで，Tinygrad-LikeなView Simplifyをする
;; - !contiguousを廃止する
;; - TODO
;; - fix double reduce
;; - Update scheduler.lisp
;;   Update !contiguous
;;   Masked Reshape to fuse ConvND = 1 Kernel

;; Milestone (after complete them, move to implement renderer/memory planner)
;; - [ ] Getting a perfect schedule
;; - [ ] ConvND = 1 Kernel
;; - [ ] Getting a perfect tile/vectorize/parallelize
;; - [ ] Ez to deploy to gpu?
;; - [ ] 明日:
;;   - [ ] MergeDimsを削除+MergeDimsはいちばん最後にやる get-grouped-dimsで，共通のViewとかを入れてMerge
;;   - [ ] Matmul+ActivationをFuse
;;   - [ ] fix for padding?

;; ConvND Step by step:
;;   - [ ] (caten/codegen:jit (caten (!mul (make-tensor `(10 1 6 21 21 3 5 5)) (!reshape (make-tensor `(6 3 5 5)) `(1 1 6 1 1 3 5 5)))))
;;   - [ ] (caten/codegen:jit (caten (!contiguous (caten/nn::_pool (!padding2d (make-tensor `(10 10)) `(2 2 2 2)) `(5 5) `(1 1) `(1 1)))))
;;   - [ ] (caten/codegen:jit (caten (caten/nn::_pool (make-tensor `(10 3 25 25)) `(5 5) `(1 1) `(1 1))))
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
