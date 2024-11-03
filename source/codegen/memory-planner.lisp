(defpackage :caten/codegen/memory-planner
  (:use :cl :caten/air :caten/avm)
  (:export #:run-memory-planner))

(in-package :caten/codegen/memory-planner)

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

(defmethod allocate-p ((mb MemoryBlock) time)
  (= time (memoryblock-create mb)))

(defmethod created-p ((mb MemoryBlock) time)
  (>= time (memoryblock-create mb)))

(defmethod preserved-p ((mb MemoryBlock) time)
  (< time (memoryblock-release mb)))

(defmethod release-p ((mb MemoryBlock) time)
  (= time (memoryblock-release mb)))

(defmethod freed-p ((mb MemoryBlock) time)
  (and (created-p mb time) (>= time (memoryblock-release mb))))

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
			     ;; [TODO] This condition can be more simplified? (!randn `(100 100)) is good to test this behaviour.
			     (equal (buffer-views (memoryblock-type candidate))
				    (buffer-views (memoryblock-type mb))))
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

(defmethod run-memory-planner ((schedule-graph Graph))
  "write_1, write_2 = f(write_suite_1, write_suite_2, *[dynamic_shape + read_buffers])
The goal of run-memory-planner is to reduce the number of :allocate-p object in schedule-graph, by rewriting write_suite1 and write_suite2."
  (let* ((trace-table (make-hash-table))
	 (id2type (make-hash-table))
	 (lock-table (make-hash-table))
	 (total-time (length (graph-nodes schedule-graph)))
	 (outputs (graph-outputs schedule-graph))
	 (constants))
    (loop for node in (graph-nodes schedule-graph)
	  for nth upfrom 0
          for lock-p = (null (getattr node :jitable)) do
	    (loop for val in (node-reads node)
		  for typ in (getattr node :read-types)
		  for time = `(,nth ,@(gethash val trace-table))
                  if (and (symbolp val) (null (find val constants)))
                    do (setf (gethash val id2type) typ (gethash val trace-table) time)) ;; (incf consume)
	    (loop for val in (node-writes node)
		  for typ in (getattr node :write-types)
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
            (setf (getattr node :storage-id-dst) (map 'list #'newid (node-writes node))))))
      (when (>= (ctx:getenv :JIT_DEBUG) 2)
        (let ((before (length (remove-duplicates (alexandria:hash-table-keys alias-map))))
              (after (length (remove-duplicates (alexandria:hash-table-values alias-map)))))
          ;; [TODO] unit should be MiB
          (format t "  Memory Planner: n_alloc(~a) -> n_alloc(~a)~%" before after))))))
