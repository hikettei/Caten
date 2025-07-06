(defpackage :caten/codegen/memory-planner
  (:documentation "`Memory Planner` is a data structure that abstracts the allocation and freeing of memory over time.
It is responsible for optimizing memory allocation by overlapping allocation to minimize the maximum memory usage (heap_size) required for all the time `t`.

Implementation:

1. Construct a timestamp from schedule-graph.

t=0 | [:FOR ...]
t=1 | [:PROGN ...]
t=2 | [:EXPR ...]

2. Map them into MemoryBlock
")
  (:use :cl :caten/air :caten/aasm :caten/aasm/expr :alexandria)
  (:export
   #:run-memory-planner))
(in-package :caten/codegen/memory-planner)
;; ~~~ Implementation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Timestamp
            (:constructor make-timestamp (time type node bp schedule-item)))
  (type type :type (and keyword (member :BLOCK_START :BLOCK_END :ALLOCATE :STMT_VM :STMT_EXPR)))
  (node node :type (or null Node))
  (bp bp :type (or null Graph))
  (schedule-item schedule-item :type (or null Node))
  (time time :type fixnum))

(defmethod print-object ((ts timestamp) stream)
  (print-unreadable-object (ts stream)
    (format stream "[t=~a] : ~a" (timestamp-time ts) (timestamp-node ts))))

(defun pprint-timestamp (timestamps &aux (indent 0))
  (declare (type list timestamps))
  (princ
   (with-output-to-string (out)
     (flet ((indent () (dotimes (i indent) (princ " " out))))
       (loop for ts in timestamps do
         (case (timestamp-type ts)
           (:BLOCK_START (fresh-line out) (indent) (princ "{" out) (incf indent 2))
           (:BLOCK_END   (fresh-line out) (decf indent 2) (indent) (princ "}" out))
           (:ALLOCATE    (fresh-line out) (indent) (format out "allocate[~a];" (node-reads (car (getattr (timestamp-node ts) :items)))))
           (:STMT_VM     (fresh-line out) (indent) (format out "stmt_vm();"))
           (:STMT_EXPR   (fresh-line out) (indent) (format out "stmt_expr();"))))))))

(defun blueprint->timestamp (graph writer &aux (seen))
  "Extracts the scope of variables in the blueprint"
  (declare (type function writer))
  (labels ((r (s &aux (val (id->value graph s)))
             (when (and val (null (find (node-id val) seen)))
               (prog1
                   (f val) (push (node-id val) seen))))
           (f (node)
             (case (node-type node)
               (:PROGN
                 `(,(funcall writer :BLOCK_START nil)
                   ,@(apply #'append (map 'list #'r (node-reads node)))
                   ,(funcall writer :BLOCK_END nil)))
               (:EXPR
                `(,(funcall writer :STMT_EXPR node graph)))
               ((:FOR :IF)
                `(,(funcall writer :BLOCK_START nil)
                  ,@(r (second (node-reads node)))
                  ,(funcall writer :BLOCK_END nil)))
               ((:DEFINE-GLOBAL :RANGE :ALLOCATE :LOAD :AREF) (error "They should not occur here???"))
               (otherwise (error "Cannot construct a timestamp from blueprint, add a case for ~a" node)))))
    `(,(funcall writer :BLOCK_START nil)
      ,@(f (id->value graph (car (graph-outputs graph))))
      ,(funcall writer :BLOCK_END nil))))

(defun schedule-graph->timestamp (schedule-graph &aux (count 0))
  (declare (type FastGraph schedule-graph))
  (labels ((node->ts (type node &optional bp si)
             (prog1
                 (make-timestamp count type node bp si)
               (incf count))))
    ;; [TODO] TimeStamp should not be a 1D array, GPUs can execute multiple kernels in the same time.
    (loop for item in (tpsort-graph schedule-graph)
          do (assert (eql (node-type item) :Schedule-Item))
          append
          (case (getattr item :type)
            (:kernel
             (let ((timestamps (blueprint->timestamp (getattr item :blueprint) #'node->ts)))
               (mapc #'(lambda (x) (setf (timestamp-schedule-item x) item)) timestamps)
               timestamps))
            (:allocate (list (node->ts :ALLOCATE item)))
            (otherwise (list (node->ts :STMT_VM item)))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct MemoryScope
  (level 0 :type fixnum)
  (vars (make-hash-table) :type hash-table))

(defun expr-gather-aref (id graph &aux seen results)
  (labels ((explore (x &aux (node (id->value graph x)))
             (when (or (null node) (find (node-id node) seen)) (return-from explore))
             (push (node-id node) seen)
             (when (eql (node-type node) :EXPR) (return-from explore)) ;; If hit on another expr, return
             (when (eql (node-type node) :AREF)
               (push node results))
             (mapc #'explore (node-reads node))))
    (explore id))
  results)

(defun apply-memory-planner (timestamps schedule-graph)
  "Simulates the flow of memory allocation and tries to minimize the peak memory usage by rewriting graph."
  (declare (type list timestamps))
  (let ((scopes) (garbages))
    (labels ((enter-new-scope ()
               (push (make-memoryscope :level (length scopes)) scopes))
             (exit-current-scope () (push (pop scopes) garbages))
             (get-current-scope () (or (car scopes) (error "Mismatch :BLOCK count")))
             (allocate-on-current-scope (id relay)
               (let ((vars (memoryscope-vars (get-current-scope))))
                 (assert (null (gethash id vars)) () "apply-memory-planner: Assignment is expected to be static.")
                 ;; [TODO] find-variable should return nil
                 (setf (gethash id vars) relay)))
             (find-variable (id)
               (loop for scope in scopes ;; finding from deeper -> shallower
                     for vars = (memoryscope-vars scope)
                     if (gethash id vars) do (return-from find-variable (gethash id vars)))
               (error "find-variable: The id ~a is not defined." id))
             (var-is-mutable-p (var schedule-item)
               (let* ((is-readonly-p (null (find var (node-writes schedule-item))))
                      (var-parent (id->value schedule-graph var))
                      (is-just-allocated-p (and var-parent (eql (node-type var-parent) :schedule-item)
                                                (eql :allocate (getattr var-parent :Type)))))
                 ;; Note: 少なくともis-readonly-pは強制，つまり，BINDにしか読まれない
                 ;; -> WAWを破壊する時の十分条件ではない？
                 ;; schedule-itemで，直Rea
                 ;; [TODO] Depthから考えて，Kernel内部にIn-PlaceMutation可能か判定する
                 ;; [TODO] 外に伸びてなくても，InPlaceMutationできない場合がある，特にFuseされてると。
                 ;; [TODO] TensorのShapeから判断する必要がある
                 (and is-just-allocated-p is-readonly-p)))
             (rewrite-bind (from to)
               (loop for ts in timestamps
                     if (eql (timestamp-type ts) :STMT_EXPR) do
                       (funcall
                        (Simplifier
                            ()
                            ((:BIND (x) :value (eql from)) -> (:BIND (x) :value to))
                            ((:AREF ((eql from) y)) -> (:AREF (to y))))
                        (timestamp-bp ts))))
             (apply-in-place-mutation (aref candidates schedule-item)
               (loop for r in candidates
                     if (var-is-mutable-p (car (node-reads r)) schedule-item) do
                       ;; Rewrite aref -> r
                       (assert (eql :AREF (node-type aref)))
                       (assert (eql :AREF (node-type r)))
                       (rewrite-bind (car (node-reads aref)) (car (node-reads r)))
                       ;; ↓がBINDである可能性は？
                       (setf (car (node-reads aref)) (car (node-reads r)))
                       (return-from apply-in-place-mutation t))
               nil))
      (enter-new-scope)
      (pprint-timestamp timestamps)
      (loop for ts in timestamps do
        (ecase (timestamp-type ts)
          (:ALLOCATE
           (let* ((node (car (getattr (timestamp-node ts) :items)))
                  (rel (car (relay-writes (read-type-relay node)))))
             (assert (eql (node-type node) :Allocate))
             (allocate-on-current-scope (car (node-writes node)) rel)))
          (:BLOCK_START (enter-new-scope))
          (:BLOCK_END (exit-current-scope))
          (:STMT_VM) ;; Aah, we cannot track the allocation of vmops...
          (:STMT_EXPR
           ;; STMT_EXPR: out[...] = f(x[...]), Motivation: can we substitute out instead of allocating extra buffer?
           (assert (timestamp-bp ts))
           (let* ((parent-schedule-item (timestamp-schedule-item ts))
                  (expr (timestamp-node ts))
                  (bp-graph (timestamp-bp ts))
                  (expr-store (id->value bp-graph (car (node-reads expr))))
                  (expr-aref  (when expr-store (id->value bp-graph (car (node-reads expr-store))))))
             (assert (eql (node-type expr) :EXPR))
             (assert parent-schedule-item)
             ;; Target EXPR: SETF(AREF(EXTRA_BUFFER, INDEXING), EXPR)
             ;;                             ^ If you rewrite this aref, the allocation is purged from graph.
             (when (and expr-aref expr-store
                        (eql (node-type expr-aref) :AREF)
                        (eql (node-type expr-store) :SETF)) ;; If the toplevel of expr is :SETF, go ahead:
               ;; Reuse Priorities
               ;; 1. In-Place Mutation
               ;; 1. softmax, last reference? (In-place mutation) node-readsが最後のrefかを確認する
               ;;   - アクセスの添字から考えればOK
               ;; 2. Find from garbage それ以外は，garbageから探す
               ;; BINDがあるからSETF ID書き換えるだけでAllocationはPurged, right?
               ;; 1. In-Place Mutation (search from expr-reads which is the last reference in the graph)
               ;; All you have to rewrite is :BIND value
               (let* ((expr-reads (expr-gather-aref (second (node-reads expr-store)) bp-graph)))
                 (or
                  (apply-in-place-mutation expr-aref expr-reads parent-schedule-item)
                  ;; Yet ANother Algorithm
                  )))))))
      (exit-current-scope)
      (assert (= 0 (length scopes)))

      (print "FINAL GRAPH")
      (loop for s in (graph-nodes schedule-graph)
            if (eql :kernel (getattr s :type)) do
              (caten/codegen/blueprint:print-blueprint (getattr s :blueprint) t))
      )))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun id-is-input-p (id graph)
  (let ((node (id->value graph id)))
    (when (and node (eql (node-type node) :Allocate))
      (when (getattr node :from)
        ;; Memory Planner is not allowed to destruct the input. (like: having a weight/parameter)
        t))))

(defun tensor-relay-sizeof (buffer)
  "Computes the size of the buffer in bits."
  (assert (every #'numberp (tensor-relay-shape buffer)))
  (* (apply #'* (tensor-relay-shape buffer)) (caten/common.dtype:dtype/size-of (tensor-relay-dtype buffer))))

(defun evaluate (timestamps &aux (fixed-region 0) (n-tensors 0))
  (declare (type list timestamps))
  (loop for ts in timestamps
        if (eql (timestamp-type ts) :ALLOCATE) do
          (let ((buf (car (relay-writes (read-type-relay (car (getattr (timestamp-node ts) :items)))))))
            (when (every #'numberp (tensor-relay-shape buf))
              (incf fixed-region (tensor-relay-sizeof buf)))
            (incf n-tensors)))
  ;; (values counter total_size[GB])
  ;; [TODO] Improve the case of dynamic graph
  (values n-tensors (float (/ fixed-region 8e+9))))

(defun run-memory-planner (schedule-graph symbolics base-graph)
  (declare (type Graph schedule-graph base-graph))
  (let ((timestamps (schedule-graph->timestamp schedule-graph)))
    (multiple-value-bind (before-count before-size)
        (when (>= (ctx:getenv :JIT_DEBUG) 2) (evaluate timestamps))
      (apply-memory-planner timestamps schedule-graph)
      (multiple-value-bind (after-count after-size)
          (when (>= (ctx:getenv :JIT_DEBUG) 2) (evaluate timestamps))
        (when (>= (ctx:getenv :JIT_DEBUG) 2)
          (let ((compressing-rate
                  (if (> before-size 0)
                      (format nil "~2,3f%" (/ (* 100 (- before-size after-size)) before-size))
                      (format nil "<Not Available in dynamic graph>"))))
            (caten/common.logger:print-info " | number of allocations: ~a -> ~a" before-count after-count)
            (caten/common.logger:print-info " | total allocation size: ~a GB -> ~a GB" before-size after-size)
            (caten/common.logger:print-info " | Compressing rate(GB):  ~a" compressing-rate)))))))
