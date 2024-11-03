(defpackage :caten/codegen/jit
  (:documentation "
caten/codegen overview:
```
--- [Step1] Scheduling ---------------------------------------------------------------------------------------------
                          [Deep Learning Models (AVM)]
                                       | Partitioning into subgraphs (scheduler.lisp)
 [Embedding_Embedding_ADD], [LayerNorm], [Matmul+GELU], [Attention1], [Attention2 (Cached)] ..., (Nx Schedule-Item)
                                       |
--- [Step2] Auto-Tuning --------------------------------------------------------------------------------------------
                                       | (foreach schedule-item)
  (dolist (schedule-item (remove-duplicates (graph-nodes schedule-graph)))
                 |
             [Lowerer] (blueprint.lisp)
                 |
   [(Optional) Lower to Polyhedral IR]
                 |
            [AutoTuning] (searching the best configuration for Parallelizing/Tiling/Vectorizing, etc...)
                 |
            [Complete])
--- [Step3] Memory Optimizing --------------------------------------------------------------------------------------
                                 |
                     [Optimized Schedule-Graph]
                                 |
                      [Running Memory-Planner]
                                 |
--- [Step4] Rendering ----------------------------------------------------------------------------------------------
                                 |
                         [Render the kernel]
                                 |
                  [Deep Learning Models (Compiled AVM)]
--------------------------------------------------------------------------------------------------------------------
```")
  (:use :cl :caten/air :caten/codegen/shape-inference)
  (:import-from
   :caten/avm
   #:AVM
   #:Buffer
   #:avm-graph
   #:avm-name
   #:avm-tape-length
   #:avm-pc
   #:avm-variables
   #:buffer-nrank
   #:buffer-shape
   #:buffer-views
   #:buffer-stride
   #:buffer-dtype
   #:buffer-inferred-permute
   #:buffer-orig-buffer-shape
   #:%impl)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules
   #:schedule-item-write-define-global)
  (:import-from
   :caten/codegen/scheduler
   #:graph-schedule)
  (:import-from
   :caten/codegen/blueprint
   #:lower-schedule-item
   #:print-blueprint)
  (:import-from
   :caten/codegen/scop
   #:scop
   #:auto-schedule)
  (:import-from
   :caten/common.logger
   #:print-info
   #:with-progress
   #:print-progress)
  (:import-from
   :caten/codegen/expr-cache
   #:with-expr-cache)
  (:import-from
   :caten/codegen/memory-planner
   #:run-memory-planner)
  (:import-from
   :caten/codegen/renderer
   #:get-default-renderer
   #:%compile-kernel
   #:%render-kernel
   #:%renderer-get-auto-scheduler)
  (:export
   #:jit))

(in-package :caten/codegen/jit)

;; ~~ Compiledop instruction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Compiled-Kernel)
  (name (error "name must occur") :type keyword)
  (caller (error "caller must occur") :type function)
  (raw-caller (error "raw-caller must occur") :type list)
  (device (error "device must occur") :type string)
  (code (error "code must occur") :type string))

(defnode (:JIT :JIT_KERNEL) ()
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM."
	 :slots ((output-buffer-n :type fixnum) (kernel-info :type Compiled-Kernel)))

(defmethod make-load-form ((jit Compiled-Kernel) &optional env)
  (declare (ignore env))
  `(make-compiled-kernel
    :name ,(compiled-kernel-name jit)
    :caller ,(compiled-kernel-raw-caller jit)
    :raw-caller ',(compiled-kernel-raw-caller jit)
    :device ,(compiled-kernel-device jit)
    :code ,(compiled-kernel-code jit)))

(defmethod print-object ((s Compiled-Kernel) stream)
  (format stream "<~a[~a]>" (compiled-kernel-device s) (compiled-kernel-name s)))

(defun make-compiled-kernel-from-si (si graph)
  (assert (eql (node-type si) :Schedule-Item))
  (flet ((from-cache (node)
           (or (find (getattr node :cache-name) (graph-nodes graph) :key #'(lambda (x) (getattr x :name)) :test #'equalp)
               (error "The cache item for ~a was not found." node))))
    (make-compiled-kernel
     :name (intern (princ-to-string (or (getattr si :cache-name) (getattr si :name))) "KEYWORD")
     :caller (if (getattr si :cache-name)
                 (compile nil (getattr (from-cache si) :compiled-object))
                 (compile nil (getattr si :compiled-object)))
     :raw-caller (if (getattr si :cache-name)
                     (getattr (from-cache si) :compiled-object)
                     (getattr si :compiled-object))
     :device (princ-to-string (or (ctx:getenv :JIT_BACKEND) :clang))
     :code (if (getattr si :cache-name)
               (getattr (from-cache si) :rendered-object)
               (getattr si :rendered-object)))))

(defun make-compiled-kernel-node (si graph)
  (make-node :JIT :JIT_KERNEL (node-writes si)
             (append
              (getattr si :storage-id-dst) ;; optimized by memory-planner
              (map 'list #'car (getattr si :dynamic-shapes))
              (node-reads si))
             :output-buffer-n (length (node-writes si))
             :kernel-info (make-compiled-kernel-from-si si graph)))

(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((info (getattr node :kernel-info))
        (out-n (getattr node :output-buffer-n)))
    (assert (functionp (compiled-kernel-caller info)) () "Could not find the function caller for the node ~a" node)
    (apply (compiled-kernel-caller info) args)
    (apply #'values (subseq args 0 out-n))))

(defun get-subgraph (graph top-id seen &aux (seen (copy-list seen)))
  (labels ((explore (x)
             (when (null x) (return-from explore))
             (when (eql x t) (return-from explore))
             (when (find x seen) (return-from explore))
             (push x seen)
             (let ((node (id->value graph x)))
               (when (null node) (return-from explore))
               (append
                (list node)
                (apply #'append (map 'list #'explore (node-reads node)))))))
    (values (nreverse (explore top-id)) seen)))

(defun make-alloc+view-node-from-buffer (wt w)
  (let ((view
          (when (some #'identity (buffer-views wt))
            (make-node :Buffer :View (list w)
                       (append
                        (list w)
                        (buffer-shape wt)
                        (map 'list #'first (buffer-views wt))
                        (map 'list #'second (buffer-views wt))
                        (map 'list #'third (buffer-views wt))
                        (buffer-stride wt))
                       :broadcast (map 'list #'fourth (buffer-views wt))
                       :nrank (length (buffer-shape wt)))))
        (alloc (make-node :Buffer :Allocate (list w)
                          (append
                           (loop for s in (buffer-shape wt)
                                 for nth upfrom 0
                                 for v = (nth nth (buffer-views wt))
                                 if (fourth v) collect 1
                                   else collect s)
                           (buffer-stride wt))
                          :nrank (length (buffer-shape wt)) :dtype (buffer-dtype wt))))
    (values view alloc)))

(defun id->output-map (graph)
  (let ((table (make-hash-table)))
    (loop for si in (graph-nodes graph) do
      (loop for item in (getattr si :items)
            for v = (getattr item :_output_type) do
              (when v
                (setf (car (node-writes v)) (car (node-reads v))
                      (gethash (car (node-reads v)) table) v))))
    table))

(defun schedule-graph->vmop (avm graph &aux (map (id->output-map graph)))
  (declare (type Graph graph))
  (verify-graph graph)
  (let ((nodes) (allocated))
    (flet ((merge-id (id)
             (multiple-value-bind (deps new-seen) (get-subgraph (avm-graph avm) id allocated)
               (setf allocated new-seen)
               (dolist (d deps) (push d nodes)))))
      (dolist (node (graph-nodes graph))
        (cond
          ((getattr node :allocate-p)
           (push (car (node-writes node)) allocated)
           (dolist (i (getattr node :items))
             (push i nodes)
             (mapc #'merge-id (node-reads i))))
          ((null (getattr node :jitable)) ;; Relocating non-jitable ops
           (dolist (w (node-writes node)) (push w allocated))
           (dolist (i (getattr node :items))
             (push i nodes)))
          ((getattr node :jitable)
           (loop for w in (getattr node :storage-id-dst)
                 for wt in (getattr node :write-types)
                 if (null (find w allocated)) do
                   (mapc #'merge-id (append (buffer-shape wt) (buffer-stride wt) (apply #'append (buffer-views wt))))
                   (multiple-value-bind (view alloc) (make-alloc+view-node-from-buffer wt w)
                     (push alloc nodes)
                     (when view (push view nodes)))
                   (push w allocated))
           (push (make-compiled-kernel-node node graph) nodes)
           ;; Merging view after the JIT_KERNEL invocation
           (loop for w in (getattr node :storage-id-dst)
                 for out-view = (gethash w map)
                 if out-view do
                   (mapc #'merge-id (node-reads out-view))
                   (push out-view nodes)))
          (T (error "schedule-graph->vmop: dont know how to merge ~a" node))))
      ;; Clean up nodes
      (dolist (node nodes)
        (flet ((replace-id (id)
                 (when (getattr node id :allow-undefined t)
                   (setf (getattr node id) nil))))
          (replace-id :_type_relay)
          (replace-id :_read_views)))
      (apply #'make-graph (nreverse nodes)))))
;; ~~~ Schedule Cache ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun buffer-equal (a b)
  (declare (optimize (speed 3)))
  (and
   (caten/avm:buffer-p a) (caten/avm:buffer-p b)
   (equal (buffer-shape a) (buffer-shape b))
   (equal (buffer-stride a) (buffer-stride b))
   (equal (buffer-views a) (buffer-views b))
   (equal (buffer-dtype a) (buffer-dtype b))
   (equal (buffer-inferred-permute a) (buffer-inferred-permute b))))

(deftype attr-value-type () `(or null symbol number list))

(defun schedule-item-equal (si1 si2 &aux (items1 (getattr si1 :items-to-cache)) (items2 (getattr si2 :items-to-cache)))
  (and
   (= (length items1) (length items2))
   (every
    #'(lambda (x y)
        (and
         (eql (node-type x) (node-type y))
         (equal (node-writes x) (node-writes y))
         (equal (node-reads x) (node-reads y))
         (let ((attrs1 (getattrs x))
               (attrs2 (getattrs y)))
           (and
            (equal attrs1 attrs2)
            (every
             #'(lambda (k1 k2)
                 (let ((a1 (getattr x k1))
                       (a2 (getattr x k2)))
                   ;; Caten/AIR has an assertion that all nodes are "dumpable"
                   ;; i.e.: attrs that impacts on the computation results are always typed number/symbol/list/bool
                   (if (and (typep a1 'attr-value-type) (typep a2 'attr-value-type))
                       (equal a1 a2)
                       t ;; [fixme] isn't it danger? if attrs are not found, they ignore it!
                       )))
             attrs1 attrs2)))
         (let ((xt (read-type-relay x))
               (yt (read-type-relay y)))
           (and
            (every #'(lambda (a b) (buffer-equal a b)) (relay-reads xt) (relay-reads yt))
            (every #'(lambda (a b) (buffer-equal a b)) (relay-writes xt) (relay-writes yt))))))
    items1 items2)))

(defun minify-equivalent-schedule (schedule-graph)
  (let ((tgts (loop for node in (graph-nodes schedule-graph)
                    if (getattr node :jitable)
                      collect node))
        (seen))
    (loop for tgt in tgts
          for eql-schedule = (find tgt seen :test #'schedule-item-equal)
          if eql-schedule
            do (setf (getattr tgt :cache-name) (getattr eql-schedule :name))
          else
            do (push tgt seen))
    schedule-graph))

(defun jit (avm
            &key
              (renderer (or (ctx:getenv :JIT_BACKEND) :clang))
              (dir nil)
            &aux
              (renderer (if (keywordp renderer) (get-default-renderer renderer) renderer))
              (auto-scheduler
               (when (= (ctx:getenv :AUTO_SCHEDULER) 1)
                 (or (%renderer-get-auto-scheduler renderer)
                     (error "Cannot enable auto-scheduler without the renderer support.~%Use define-auto-scheduler and define-hook-auto-scheduler and compilers will recognise it.~%or, set AUTO_SCHEDULER=0 to ignore this error.")))))
  "Runs the JIT compilation (destructive)"
  (declare (type AVM avm))
  ;; 1. Running the shape/offset/type inference
  (run-type-infer avm)
  ;; 2. Applying JIT Specific Graph Rewriting Rules in advance (e.g.: Propagete Views, Symbol Loads, ...)
  (apply-rewriting-rules avm)
  ;; 3. Running the scheduler
  (let ((schedule-graph (graph-schedule (avm-graph avm)))
        ;; 4. Gathering the dynamic shapes used in the graph.
        (symbolics
          (remove-duplicates
           (loop for node in (graph-nodes (avm-graph avm))
                 if (and (eql (node-type node) :LOAD) (symbolp (getattr node :value)))
                   collect (getattr node :value)))))
    (declare (type Graph schedule-graph))
    ;; 5. Minifying the number of schedules, (reuse kernels)
    (minify-equivalent-schedule schedule-graph)
    ;; 6. Start JIT Compilation. (Performing by group)
    (let ((total-kernels (count-if #'(lambda (x) (getattr x :jitable)) (graph-nodes schedule-graph))))
      (when (>= (ctx:getenv :JIT_DEBUG) 2)
        (print-info "JIT Compilation Start (AVM=~a)" (avm-name avm)))
      (verify-graph schedule-graph)
      (setf schedule-graph (->graph schedule-graph))
      (with-progress (total-kernels :debug (if (>= (ctx:getenv :JIT_DEBUG) 2) 1 -1) :timeit nil)
        (with-expr-cache () ;; Initialize a cache to treat (EXPR: a*b) as a symbolic and make symbolic collapsed loops as an affine loop.
          (mapc
           #'(lambda (x &aux (start (get-internal-real-time)))
               (when (and (getattr x :jitable) (getattr x :cache-name))
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (print-progress "~a" (getattr x :name))
                   (format t "=====> (Skipped) redirect to ~a~%" (getattr x :cache-name))))
               (when (getattr x :jitable)
                 (when (and (>= (ctx:getenv :JIT_DEBUG) 2) (null (getattr x :cache-name)))
                   (print-progress "~a" (getattr x :name))
                   (format t "=====> Lowering to blueprint~%"))
                 ;; 7. Running Lowerer
                 (lower-schedule-item x (avm-graph avm) schedule-graph)
                 (when (null (getattr x :cache-name))
                   ;; 8. Lower into Polyhedral IR
                   (when (and (>= (ctx:getenv :JIT_DEBUG) 2) (null (getattr x :auto-schedule-p)) (>= (ctx:getenv :AUTO_SCHEDULER) 1))
                     (format t "=====> Skipping Auto Scheduler (Symbolic incremental or scalar kernel)~%"))
                   (when (and (>= (ctx:getenv :AUTO_SCHEDULER) 1) (getattr x :auto-schedule-p))
                     (when (>= (ctx:getenv :JIT_DEBUG) 2)
                       (format t "=====> Lowering to Polyhedral IR~%"))
                     (scop x symbolics)
                     (when (>= (ctx:getenv :JIT_DEBUG) 2)
                       (format t "=====> Auto Scheduler~%"))
                     ;; 9. Optimizing: Tiles, Parallelizing, Vectorizing, Unrolling
                     (auto-schedule auto-scheduler x)
                     (when (>= (ctx:getenv :JIT_DEBUG) 2)
                       (print (getattr x :polyhedral))
                       (fresh-line)
                       (format t "=====> Optimized kernel~%")
                       (print-blueprint (getattr x :blueprint) t)))
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (format t "Compilation Time : ~A(sec)" (float (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))))
           (graph-nodes schedule-graph)))))
    ;; 10. Running memory-planner, update the storage-id
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Running the memory planner..."))
    ;;(run-memory-planner schedule-graph) disable until fixing weirdness in Padding2D/AutoDiff
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Rendering ..."))
    (dolist (s (graph-nodes schedule-graph))
      (when (and (getattr s :jitable) (getattr s :blueprint))
        (schedule-item-write-define-global s)
        (setf (getattr s :rendered-object) (%render-kernel renderer s))))
    ;; 11. Complete (Render by the renderer)
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Compiling ..."))
    (%compile-kernel renderer (graph-nodes schedule-graph) dir)
    (let ((new-graph (schedule-graph->vmop avm schedule-graph)))
      (setf (avm-graph avm) new-graph
            (avm-tape-length avm) (length (graph-nodes new-graph))
            (avm-pc avm) 0
            (avm-variables avm) (make-hash-table)))
    avm))
