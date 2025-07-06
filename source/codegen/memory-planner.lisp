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
            (:constructor make-timestamp (time type node bp)))
  (type type :type (and keyword (member :BLOCK_START :BLOCK_END :ALLOCATE :STMT_VM :STMT_EXPR)))
  (node node :type (or null Node))
  (bp bp :type (or null Graph))
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
  (labels ((node->ts (type node &optional bp)
             (prog1
                 (make-timestamp count type node bp)
               (incf count))))
    ;; [TODO] TimeStamp should not be a 1D array, GPUs can execute multiple kernels in the same time.
    (loop for item in (tpsort-graph schedule-graph)
          do (assert (eql (node-type item) :Schedule-Item))
          append
          (case (getattr item :type)
            (:kernel (blueprint->timestamp (getattr item :blueprint) #'node->ts))
            (:allocate (list (node->ts :ALLOCATE item)))
            (otherwise (list (node->ts :STMT_VM item)))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct MemoryScope
  (level 0 :type fixnum)
  (vars (make-hash-table) :type hash-table))

(defun apply-memory-planner (timestamps)
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
               (error "find-variable: The id ~a is not defined." id)))
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
          (:STMT_VM) ;; Cannot track the flow of allocations?...
          (:STMT_EXPR
           ;; STMT_EXPR: out[...] = f(x[...]), outの書き込む先に何か代用できるALLOCは存在するかを考える
           (assert (timestamp-bp ts))
           (let* ((expr (timestamp-node ts))
                  (bp-graph (timestamp-bp ts))
                  (expr-store (id->value bp-graph (car (node-reads expr)))))
             (assert (eql (node-type expr) :EXPR))
             ;; SETF(ID, EXPR), IDを書き換えるだけで，Allocationは消滅する。
             ;; Priorities
             ;; 1. softmax, last reference?
             ;; 2. Find from garbage
             (when (eql (node-type expr-store) :SETF) ;; If the toplevel of expr is :SETF, go ahead:
               ;; BINDがあるからSETF ID書き換えるだけでAllocationはPurged, right?
               (print expr))))))
      (exit-current-scope)
      (assert (= 0 (length scopes)))
      )))
  
(defstruct (MemoryBlock
	    (:constructor make-memoryblock (id type create release &key (lock nil))))
  "Ab abstraction for a memory allocation and release pipeline.
    |
 i  |  (create)  (release)
 d  |     |----------| 
    |
-------------------------
   t i m e
MemoryBlock(id) is allocated when t=create, preserved until t become `release`."
  (id id :type symbol)
  (answer nil :type symbol)
  (type type :type TensorRelay)
  (create create :type fixnum)
  (release release :type fixnum)
  (lifetime (- release create) :type (integer 0))
  (lock lock :type boolean))

(defmethod print-object ((mb MemoryBlock) stream)
  (format stream "MemoryBlock(~(~a~) -> ~(~a~)) : (~a, ~a, ~a, lock=~a)~%" (memoryblock-id mb) (memoryblock-answer mb) (tensor-relay-shape (memoryblock-type mb)) (memoryblock-create mb) (memoryblock-release mb) (memoryblock-lock mb)))

(defmethod allocate-p ((mb MemoryBlock) time) (= time (memoryblock-create mb)))
(defmethod created-p ((mb MemoryBlock) time) (>= time (memoryblock-create mb)))
(defmethod preserved-p ((mb MemoryBlock) time) (< time (memoryblock-release mb)))
(defmethod release-p ((mb MemoryBlock) time) (= time (memoryblock-release mb)))
(defmethod freed-p ((mb MemoryBlock) time) (and (created-p mb time) (>= time (memoryblock-release mb))))

(defun buffer-orig-shape (buffer)
  "Returns a shape of the buffer, which is not VIEWED."
  (declare (type AbstractBuffer buffer))
  (or
   (buffer-orig-buffer-shape buffer) ;; non-viewed-size
   (buffer-shape buffer)))

(defun buffer-element-size (buffer)
  (let ((shape (buffer-orig-shape buffer))
        (count nil)
        (symbols nil))
    (loop for s in shape
          if (symbolp s) do (push s symbols)
          else do (push s count))
    (cons (apply #'* count) symbols)))

(defun buffer-size-eq (a b)
  (let ((s1 (buffer-element-size a))
        (s2 (buffer-element-size b)))
    (and
     (= (car s1) (car s2)) ;; fixed parts
     (= (length (cdr s1)) (length (cdr s2))) ;; number of symbols
     (let ((stack (cdr s1)))
       (dolist (k (cdr s2)) (setf stack (remove k stack :test #'eql)))
       (null stack)))))
;; Paper: Best-Fit Heuristic https://arxiv.org/pdf/1804.10001
(defun greedy-solve-dsa (I total-time black-lists)
  "A greedy solver for minimizing `peak_mem`"
  (declare (type list I))
  (let ((locked))
    (labels ((choose-from-fragments (mb time &aux (candidates nil))
	       (loop for candidate in I
		     if (and (null (find (memoryblock-id candidate) locked))
			     (freed-p candidate time)
                             (null (find (memoryblock-id candidate) (gethash (memoryblock-id mb) black-lists)))
                             (not (= -1 (buffer-nrank (memoryblock-type mb))))
                             (not (= -1 (buffer-nrank (memoryblock-type candidate))))
			     (buffer-shape (memoryblock-type mb)) ;; <=> assure the memory-block is a tensor
                             (buffer-size-eq (memoryblock-type candidate) (memoryblock-type mb))
			     (equal (buffer-dtype (memoryblock-type candidate)) (buffer-dtype (memoryblock-type mb)))
                             ;; [TODO] If offsets were created but size are equivalent; they are not cached right?
			     (equal (buffer-views (memoryblock-type candidate)) (buffer-views (memoryblock-type mb))))
		       do (push candidate candidates))
	       (flet ((use (x)
			(push (memoryblock-id x) locked)
			(return-from choose-from-fragments x)))
		 (when candidates (use (car (sort candidates #'> :key #'memoryblock-lifetime))))))
	     (apply-creation (time)
	       (loop for mb in I
		     if (allocate-p mb time) do
		       (let ((buffer (and (null (memoryblock-lock mb)) (choose-from-fragments mb time))))
			 (if buffer
			     (setf (memoryblock-answer mb) (memoryblock-id buffer))
			     (setf (memoryblock-answer mb) (memoryblock-id mb))))))
	     (apply-release (time)
	       (loop for mb in I
		     if (and (release-p mb time) (memoryblock-answer mb)) do
		       (setf locked (remove (memoryblock-answer mb) locked)))))
      (dotimes (time total-time)
	(apply-release time)
	(apply-creation time))
      I)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun id-is-input-p (id graph)
  (let ((node (id->value graph id)))
    (when (and node (eql (node-type node) :Allocate))
      (when (getattr node :from)
        ;; Memory Planner is not allowed to destruct the input. (like: having a weight/parameter)
        t))))

(defun rewrite-bp-with-newid (item newid)
  "Rewrites the given schedule item with newid"
  (dolist (bp (getattr item :blueprint))
    (setf (node-writes bp) (map 'list newid (node-writes bp))
          (node-reads bp) (map 'list newid (node-reads bp)))
    (when (eql (node-type bp) :EXPR)
      (dolist (item (graph-nodes (expr-graph (getattr bp :EXPR))))
        (when (eql (node-type item) :AREF)
          (setf (getattr item :storage-id) (funcall newid (getattr item :storage-id)))))))
  ;; Remove Duplicated :DEFINE_GLOBAL
  ;; NEEDS A UPDATE
  (setf (getattr item :blueprint)
        (loop for item in (getattr item :blueprint)
              if (not (eql (node-type item) :DEFINE-GLOBAL))
                collect item))
  (let* ((reads (map 'list #'cons (getattr item :storage-id-src) (getattr item :read-types)))
         (writes (map 'list #'cons (getattr item :storage-id-dst) (getattr item :write-types)))
         (reads (remove-duplicates reads :key (compose newid #'car)))
         (writes (remove-duplicates writes :key (compose newid #'car)))
         (base-writes (getattr item :storage-id-dst))
         (seen))
    (flet ((only-unseen (items)
             (loop for (id . type) in items
                   if (null (find (funcall newid id) seen))
                     do (push (funcall newid id) seen) and collect (cons id type))))
      (multiple-value-bind (writes reads) (values (only-unseen writes) (only-unseen reads))
        (setf (getattr item :storage-id-src) (map 'list (compose newid #'car) reads)
              (getattr item :storage-id-dst) (map 'list (compose newid #'car) writes)
              (getattr item :read-types) (map 'list #'cdr reads)
              (getattr item :write-types) (map 'list #'cdr writes)
              (getattr item :return-positions) (map 'list #'(lambda (x) (position (funcall newid x) (getattr item :storage-id-dst))) base-writes)))
      (caten/codegen/rewriting-rules:schedule-item-write-define-global item))))

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
      (apply-memory-planner timestamps)
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
