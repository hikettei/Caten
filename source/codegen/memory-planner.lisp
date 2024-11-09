(defpackage :caten/codegen/memory-planner
  (:use :cl :caten/air :caten/avm :caten/codegen/shape-inference :caten/codegen/expr)
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
;; Paper: Best-Fit Heuristic https://arxiv.org/pdf/1804.10001
(defun greedy-solve-dsa (I total-time)
  "A greedy solver for minimizing `peak_mem`"
  (declare (type list I))
  (let ((locked))
    (labels ((choose-from-fragments (mb time &aux (candidates nil))
	       (loop for candidate in I
		     if (and (null (find (memoryblock-id candidate) locked))
			     (freed-p candidate time)
                             (not (= -1 (buffer-nrank (memoryblock-type mb))))
                             (not (= -1 (buffer-nrank (memoryblock-type candidate))))
			     (buffer-shape (memoryblock-type mb)) ;; <=> assure the memory-block is a tensor
			     (equal (buffer-orig-shape (memoryblock-type candidate))
				    (buffer-orig-shape (memoryblock-type mb)))
			     (equal (buffer-dtype (memoryblock-type candidate))
				    (buffer-dtype (memoryblock-type mb)))
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
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun id-is-input-p (id graph)
  (let ((node (id->value graph id)))
    (when (and node (eql (node-type node) :Allocate))
      (when (getattr node :from)
        ;; If :from is specified => the input should not be destructed.
        t))))

(defmethod run-memory-planner-global ((schedule-graph Graph) (symbolics list) (base-graph Graph))
  "write_1, write_2 = f(write_suite_1, write_suite_2, *[dynamic_shape + read_buffers])
The goal of run-memory-planner is to reduce the number of :allocate-p object in schedule-graph, by rewriting write_suite1 and write_suite2."
  (let* ((trace-table (make-hash-table))
	 (id2type (make-hash-table))
	 (lock-table (make-hash-table))
	 (total-time (length (graph-nodes schedule-graph)))
         (outputs (append (graph-outputs schedule-graph) symbolics))
	 (constants))
    (loop for node in (graph-nodes schedule-graph)
	  for nth upfrom 0
          for lock-p = (null (getattr node :jitable)) do
	    (loop for val in (getattr node :storage-id-src)
		  for typ in (getattr node :read-types)
		  for time = `(,nth ,@(gethash val trace-table))
                  if (id-is-input-p val base-graph) do (push val outputs)
                  if (and (symbolp val) (null (find val constants)))
                    do (setf (gethash val id2type) typ (gethash val trace-table) time)) ;; (incf consume)
	    (loop for val in (getattr node :storage-id-dst)
		  for typ in (getattr node :write-types)
                  if (id-is-input-p val base-graph) do (push val outputs)
		  if (and (symbolp val) (null (gethash val trace-table)))
                    ;; ID2Type    -> the variable name and its type
                    ;; TraceTable -> the variable name and timestamps of the variable (when it's used)
                    ;; LockTable  -> Set T to lock (never become in-place)
		    do (setf (gethash val id2type) typ
			     (gethash val trace-table) (list nth)
                             (gethash val lock-table) lock-p)))
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
	   (solved (greedy-solve-dsa memory-blocks total-time))
           ;; Retrive the solution. A hash table of OLD_MEMORY_ID -> NEW_MEMORY_ID
           (alias-map (make-hash-table)))
      (loop for mb in solved
            do (setf (gethash (memoryblock-id mb) alias-map) (or (memoryblock-answer mb) (memoryblock-id mb))))
      (flet ((newid (id) (or (gethash id alias-map) id)))
        (dolist (node (graph-nodes schedule-graph))
          (when (getattr node :jitable)
            (setf (getattr node :storage-id-dst) (map 'list #'newid (getattr node :storage-id-dst)))))))))

(defun run-memory-planner-local (item schedule-graph symbolics base-graph)
  "Minimizes the number of allocation buffers that are only used in the item."
  (declare (type node item) (type graph schedule-graph))
  (assert (eql (node-type item) :Schedule-Item))
  (let* ((blueprint (getattr item :blueprint))
         (trace-table (make-hash-table))
	 (id2type (make-hash-table))
	 (lock-table (make-hash-table))
	 (total-time (length blueprint))
         (outputs ;; a list of buffers that do no changed by the memory-planner
             (append ;; If the output were read by other kernels, it should be optimized by the global memory-planner.
              (graph-outputs schedule-graph)
              symbolics
              (loop for node in (graph-nodes schedule-graph)
                    if (not (eql (node-id node) (node-id item)))
                      append (node-reads node))))
	 (constants))
    (loop for node in blueprint
	  for nth upfrom 0
          if (not (eql (node-class node) :Render)) do
	    (loop for val in (node-reads node)
		  for typ in (relay-reads (read-type-relay node))
		  for time = `(,nth ,@(gethash val trace-table))
                  if (id-is-input-p val base-graph) do (push val outputs)
                  if (and (symbolp val) (null (find val constants)))
                    do (setf (gethash val id2type) typ (gethash val trace-table) time)) ;; (incf consume)
	    (loop for val in (node-writes node)
		  for typ in (relay-writes (read-type-relay node))
                  if (id-is-input-p val base-graph) do (push val outputs)
		  if (and (symbolp val) (null (gethash val trace-table)))
                    ;; ID2Type    -> the variable name and its type
                    ;; TraceTable -> the variable name and timestamps of the variable (when it's used)
                    ;; LockTable  -> Set T to lock (never become in-place)
		    do (setf (gethash val id2type) typ
                             (gethash val trace-table) (list nth))))
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
	   (solved (greedy-solve-dsa memory-blocks total-time))
           ;; Retrive the solution. A hash table of OLD_MEMORY_ID -> NEW_MEMORY_ID
           (alias-map (make-hash-table)))
      (loop for mb in solved
            do (setf (gethash (memoryblock-id mb) alias-map) (or (memoryblock-answer mb) (memoryblock-id mb))))
      (flet ((newid (id) (or (gethash id alias-map) id)))
        (dolist (bp (getattr item :blueprint))
          (setf (node-writes bp) (map 'list #'newid (node-writes bp))
                (node-reads bp) (map 'list #'newid (node-reads bp)))
          (when (eql (node-type bp) :EXPR)
            (dolist (item (graph-nodes (expr-graph (getattr bp :EXPR))))
              (when (eql (node-type item) :AREF)
                (setf (getattr item :storage-id) (newid (car (node-writes item))))))))
        ;; remove duplicated define-global
        (setf (getattr item :blueprint)
              (loop with seen = nil
                    for item in (getattr item :blueprint)
                    if (or (not (eql (node-type item) :DEFINE-GLOBAL))
                           (null (find (car (node-writes item)) seen)))
                      collect item
                    if (eql (node-type item) :DEFINE-GLOBAL)
                      do (push (car (node-writes item)) seen)))
        (let* ((reads (map 'list #'cons (getattr item :storage-id-src) (getattr item :read-types)))
               (writes (map 'list #'cons (getattr item :storage-id-dst) (getattr item :write-types)))
               (reads (remove-duplicates reads :key (alexandria:compose #'newid #'car)))
               (writes (remove-duplicates writes :key (alexandria:compose #'newid #'car)))
               (seen))
          (flet ((only-unseen (items)
                   (loop for (id . type) in items
                         if (null (find (newid id) seen))
                           do (push (newid id) seen) and collect (cons id type))))
            (multiple-value-bind (reads writes) (values (only-unseen reads) (only-unseen writes))
              (setf (getattr item :storage-id-src) (map 'list (alexandria:compose #'newid #'car) reads)
                    (getattr item :storage-id-dst)  (map 'list (alexandria:compose #'newid #'car) writes)
                    (getattr item :read-types) (map 'list #'cdr reads)
                    (getattr item :write-types) (map 'list #'cdr writes))))))
      alias-map)))

(defun buffer-sizeof (buffer)
  "Returns the size of the buffer in bits"
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

(defun remove-extra-node-writes-to (schedule-node)
  (assert (eql (node-type schedule-node) :Schedule-Item))
  (when (and (getattr schedule-node :jitable)
             (not (= 0 (length (getattr schedule-node :storage-id-dst)))))
    (setf (node-writes schedule-node)
          (loop for d in (getattr schedule-node :storage-id-dst)
                for id in (node-writes schedule-node)
                collect id))))

(defmethod run-memory-planner ((schedule-graph Graph) (symbolics list) (base-graph Graph))
  (let ((static-graph-p (null symbolics)))
    (multiple-value-bind (before-count before-size)
        (when (>= (ctx:getenv :JIT_DEBUG) 2) (evaluate schedule-graph static-graph-p))
      ;; First, applying the memory-planner kernel by kernel.
      ;; The goal is to reduce the number of arguments in the kernel.
      (dolist (item (graph-nodes schedule-graph))
        (when (and (getattr item :jitable) (getattr item :blueprint))
          (run-memory-planner-local item schedule-graph symbolics base-graph)))
      ;; Second, applying the memory-planner in the schedule-graph level
      ;; The goal here is to reduce the number of :allocate-p object in schedule-graph.
      (run-memory-planner-global schedule-graph symbolics base-graph)
      (mapc #'remove-extra-node-writes-to (graph-nodes schedule-graph))
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
