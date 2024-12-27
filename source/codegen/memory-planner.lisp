(defpackage :caten/codegen/memory-planner
  (:documentation "`Memory Planner` is a data structure that abstracts the allocation and freeing of memory over time.
It is responsible for optimizing memory allocation by overlapping allocation to minimize the maximum memory usage (heap_size) required for all the time `t`.")
  (:use :cl :caten/air :caten/runtime/buffer :caten/codegen/shape-inference :caten/codegen/expr :alexandria)
  (:export
   #:run-memory-planner))
(in-package :caten/codegen/memory-planner)
;; ~~~ Implementation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  (type type :type buffer)
  (create create :type fixnum)
  (release release :type fixnum)
  (lifetime (- release create) :type (integer 0))
  (lock lock :type boolean))

(defmethod print-object ((mb MemoryBlock) stream)
  (format stream "MemoryBlock(~(~a~) -> ~(~a~)) : (~a, ~a, ~a, lock=~a)~%" (memoryblock-id mb) (memoryblock-answer mb) (buffer-shape (memoryblock-type mb)) (memoryblock-create mb) (memoryblock-release mb) (memoryblock-lock mb)))

(defmethod allocate-p ((mb MemoryBlock) time) (= time (memoryblock-create mb)))
(defmethod created-p ((mb MemoryBlock) time) (>= time (memoryblock-create mb)))
(defmethod preserved-p ((mb MemoryBlock) time) (< time (memoryblock-release mb)))
(defmethod release-p ((mb MemoryBlock) time) (= time (memoryblock-release mb)))
(defmethod freed-p ((mb MemoryBlock) time) (and (created-p mb time) (>= time (memoryblock-release mb))))

(defun buffer-orig-shape (buffer)
  "Returns a shape of the buffer, which is not VIEWED."
  (declare (type buffer buffer))
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

(defun apply-memory-planner (schedule-graph symbolics base-graph)
  (declare (type graph schedule-graph))
  (let* ((nodes
           (loop for node in (graph-nodes schedule-graph)
                 if (getattr node :jitable)
                   append (getattr node :blueprint-base)
                 else
                   collect node))
         (realized-ids
           (remove-duplicates
            (loop for node in (graph-nodes schedule-graph)
                  append (append (node-writes node) (node-reads node) (getattr node :storage-id-dst) (getattr node :storage-id-src)))))
         (exprs (apply #'make-graph (loop for n in nodes if (eql (node-type n) :EXPR) collect n)))
         (total-time (length nodes))
         (trace-table (make-hash-table))
	 (id2type (make-hash-table))
	 (lock-table (make-hash-table))
         (outputs ;; a list of buffers that do no changed by the memory-planner
             (append ;; If the output were read by other kernels, it should be optimized by the global memory-planner.
              (graph-outputs schedule-graph)
              symbolics))
         (id->depend-loops (make-hash-table))
         (black-list-table (make-hash-table)))
    (loop with stacks = nil
          for node in nodes
          if (eql (node-type node) :FOR) do
            (push node stacks)
          else if (eql (node-type node) :ENDFOR) do
            (setf stacks (remove (getattr node :idx) stacks :key #'(lambda (x) (getattr x :idx))))
          else
            do (setf (gethash (node-id node) id->depend-loops) stacks))
    (dolist (node (graph-nodes schedule-graph))
      (loop for w in (node-writes node)
            for ws in (getattr node :storage-id-dst)
            if (not (eql w ws))
              do (push ws symbolics))) ;; already reserved -> do not change
    (dolist (s symbolics) (setf (gethash s lock-table) t))
    ;; Creating a timestamp table for each node and variable.
    (loop for node in nodes
	  for nth upfrom 0
          if (eql (node-type node) :EXPR) do
            ;; Some hacks to keep the dependency of reduce ops
            ;; for ...
            ;;  for ...
            ;;   for ...
            ;;    x = a * b + c
            ;;  out = x
            ;; Here, out should not be mutated as x, a, b, and c.
            ;; Enumerate such pairs and record then to the black-list-table.
            (loop with node-loops = (gethash (node-id node) id->depend-loops)
                  for read in (node-reads node)
                  for val = (loop for e in (graph-nodes exprs) if (find read (node-writes e)) collect e) do
                    (loop for r in val
                          for parent-loops = (gethash (node-id r) id->depend-loops)
                          if (and r (not (eql (node-id r) (node-id node)))
                                  (getattr r :reduction :allow-undefined t)
                                  ;; reduction after elementwise will never a solution
                                  (intersection node-loops parent-loops :key #'node-id) ;; intersects
                                  (not (= (length node-loops) (length parent-loops)))) ;; but partially
                            do (dolist (read-id (node-reads r))
                                 (dolist (write-id (node-writes node))
                                   ;; Explicit the mutation from W to R is invaild.
                                   (push read-id (gethash write-id black-list-table))))))
          if (eql (node-type node) :Schedule-Item) ; Optimization for non-jitable instructions (like: foreign kernel calls, allocation, pause/backward)
            do (assert (= (length (getattr node :storage-id-src)) (length (getattr node :read-types))))
               (assert (= (length (getattr node :storage-id-dst)) (length (getattr node :write-types))))
               ;; Lock the allocation (its the minimum requirement for running the graph)
               (when (getattr node :allocate-p)
                 (setf (gethash (car (node-writes node)) lock-table) t))
               (loop for val in (getattr node :storage-id-src)
                     for typ in (getattr node :read-types)
                     for time = `(,nth ,@(gethash val trace-table))
                     if (id-is-input-p val base-graph) do (push val outputs)
                       if (symbolp val)
                         do (setf (gethash val id2type) typ (gethash val trace-table) time))
               (loop for val in (getattr node :storage-id-dst)
                     for typ in (getattr node :write-types)
                     for time = `(,nth ,@(gethash val trace-table))
                     if (id-is-input-p val base-graph) do (push val outputs)
                       if (and (symbolp val) (null (gethash val trace-table)))
                         do (setf (gethash val id2type) typ) (gethash val trace-table) (list nth))
          if (and
              (not (eql (node-type node) :Schedule-Item)) ; For jitable and lowered instructions
              (not (eql (node-class node) :Render)))
            do (loop for val in (node-reads node)
		     for typ in (relay-reads (read-type-relay node))
		     for time = `(,nth ,@(gethash val trace-table))
                     if (id-is-input-p val base-graph) do (push val outputs)
                       if (and (symbolp val) (find val realized-ids))
                         do (setf (gethash val id2type) typ (gethash val trace-table) time))
	       (loop for val in (node-writes node)
		     for typ in (relay-writes (read-type-relay node))
                     if (id-is-input-p val base-graph) do (push val outputs)
		       if (and (symbolp val) (null (gethash val trace-table)) (find val realized-ids))
                         ;; ID2Type    -> the variable name and its type
                         ;; TraceTable -> the variable name and timestamps of the variable (when it's used)
                         ;; LockTable  -> Set T to lock (never become in-place)
		         do (setf (gethash val id2type) typ (gethash val trace-table) (list nth))))
    (let* ((memory-blocks
	     (loop for key in (alexandria:hash-table-keys trace-table)
	           for typ = (gethash key id2type)
		   collect
                   ;; [Note] A memory block lives in the range of [min{t}, max{t})
                   ;; Plus, If the same task (e.g.: T0(x) -> T1(x) -> T0(x+1)) is scheduled, the memory block lives from 0 to 2.
		   (make-memoryblock
		    key typ
		    (apply #'min (gethash key trace-table))
                    ;; Set the longest time for the output variables (not to destruct it, and users can see the result)
		    (if (find key outputs)
			total-time
		        (apply #'max (gethash key trace-table)))
		    :lock (gethash key lock-table))))
           ;; Minimize the peak memory usage
	   (solved (greedy-solve-dsa memory-blocks total-time black-list-table))
           ;; Retrive the solution. A hash table of OLD_MEMORY_ID -> NEW_MEMORY_ID
           (alias-map (make-hash-table)))
      (loop for mb in solved
            do (setf (gethash (memoryblock-id mb) alias-map) (or (memoryblock-answer mb) (memoryblock-id mb))))
      ;; Note(hikettei): is this recursively applied? especially for schedule cached and big graph.
      ;; As of this writing(2024/11/10), i am unsure if this is correct. Should be tested by GPT2 in the next pr.
      (labels ((newid (id &key (seen))
                 (if (gethash id alias-map)
                     (if (or (eql (gethash id alias-map) id) (find (gethash id alias-map) seen))
                         id
                         (newid (gethash id alias-map) :seen (append seen (list id))))
                     id)))
        (when (>= (ctx:getenv :JIT_DEBUG) 4)
          (format t "[DEBUG] MemoryPlanner: minimized alias-map~%")
          (maphash
           #'(lambda (k v)
               (format t "   | newid(~a) = ~a, alias-map[~a] = ~a~%" k (newid k) k v))
           alias-map))
        (dolist (node (graph-nodes schedule-graph))
          (rewrite-bp-with-newid node #'newid))))))

(defun buffer-sizeof (buffer)
  "Computes the size of the buffer in bits."
  (assert (every #'numberp (buffer-shape buffer)))
  (* (apply #'* (buffer-shape buffer)) (caten/common.dtype:dtype/size-of (buffer-dtype buffer))))

(defmethod evaluate ((schedule-graph Graph) static-p)
  (let ((seen)
        (tensor-counter 0)
        (total-size 0))
    (dolist (item (graph-nodes schedule-graph))
      (when (getattr item :allocate-p)
        (let ((alloc (car (getattr item :items))))
          (if alloc
              (when (null (find (car (node-writes item)) seen))
                (push (car (node-writes item)) seen)
                (incf tensor-counter)
                (when static-p
                  (incf total-size (buffer-sizeof (car (relay-writes (read-type-relay alloc)))))))
              (warn "evaluate: ~a is not allocation right?" item))))
      (when (getattr item :jitable)
        (loop for w in (getattr item :storage-id-dst)
              for wt in (getattr item :write-types)
              if (null (find w seen)) do
                (push w seen)
                (incf tensor-counter)
                (when static-p
                  (incf total-size (buffer-sizeof wt))))))
    ;; (values counter total_size[GB])
    (values tensor-counter (float (/ total-size 8e+9)))))

(defmethod run-memory-planner ((schedule-graph Graph) (symbolics list) (base-graph Graph))
  (let ((static-graph-p (null symbolics)))
    (multiple-value-bind (before-count before-size)
        (when (>= (ctx:getenv :JIT_DEBUG) 2) (evaluate schedule-graph static-graph-p))
      (apply-memory-planner schedule-graph symbolics base-graph)
      (multiple-value-bind (after-count after-size)
          (when (>= (ctx:getenv :JIT_DEBUG) 2) (evaluate schedule-graph static-graph-p))
        (when (>= (ctx:getenv :JIT_DEBUG) 2)
          (let ((compressing-rate
                  (if (and static-graph-p (> before-size 0))
                      (format nil "~2,3f%" (/ (* 100 (- before-size after-size)) before-size))
                      (format nil "<Not Available in dynamic graph>"))))
            (caten/common.logger:print-info " | number of allocations: ~a -> ~a" before-count after-count)
            (caten/common.logger:print-info " | total allocation size: ~a GB -> ~a GB" before-size after-size)
            (caten/common.logger:print-info " | Compressing rate(GB):  ~a" compressing-rate)))))))
