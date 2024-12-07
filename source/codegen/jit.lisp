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
   #:lower-cached-schedule-item
   #:print-blueprint)
  (:import-from
   :caten/codegen/scop
   #:scop
   #:auto-schedule)
  (:import-from
   :caten/codegen/helpers
   #:coerce-dtyped-buffer)
  (:import-from
   :caten/common.logger
   #:print-info
   #:with-progress
   #:print-progress)
  (:import-from
   :caten/codegen/expr-cache
   #:with-expr-cache
   #:read-ptrid)
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
   #:jit
   #:schedule-graph->avm-graph
   #:Compiled-Kernel
   #:compiled-kernel-name
   #:compiled-kernel-caller
   #:compiled-kernel-raw-caller
   #:compiled-kernel-device
   #:compiled-kernel-code))

(in-package :caten/codegen/jit)
;; ~~ Compiledop instruction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Compiled-Kernel)
  (name (error "name must occur") :type keyword)
  (caller (error "caller must occur") :type function)
  (raw-caller (error "raw-caller must occur") :type list)
  (device (error "device must occur") :type string)
  (code (error "code must occur") :type string)
  (out-positions (error "out positions must occur") :type list))

(defnode (:JIT :JIT_KERNEL) ()
	 "The node :JIT_KERNEL is an instruction that calls a jit-compiled kernel from the VM."
	 :slots ((output-buffer-n :type fixnum) (kernel-info :type Compiled-Kernel) (dtypes :type list) (cached-p :type boolean)))

(defmethod make-load-form ((jit Compiled-Kernel) &optional env)
  (declare (ignore env))
  `(make-compiled-kernel
    :name ,(compiled-kernel-name jit)
    :caller ,(compiled-kernel-raw-caller jit)
    :raw-caller ',(compiled-kernel-raw-caller jit)
    :device ,(compiled-kernel-device jit)
    :code ,(compiled-kernel-code jit)
    :out-positions ,(compiled-kernel-out-positions jit)))

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
               (getattr si :rendered-object))
     :out-positions (or (getattr si :return-positions) (loop for i upfrom 0 below (length (node-writes si)) collect i)))))

(defun make-compiled-kernel-node (si graph)
  (make-node :JIT :JIT_KERNEL (node-writes si)
             (append
              (getattr si :storage-id-dst) ;; optimized by memory-planner
              (map 'list #'car (getattr si :dynamic-shapes))
              (getattr si :storage-id-src)) ;; optimized by memory-planner
             :output-buffer-n (length (node-writes si))
             :kernel-info (make-compiled-kernel-from-si si graph)
             :dtypes
             (loop for item in (getattr si :blueprint)
                   if (eql (node-type item) :DEFINE-GLOBAL)
                     collect (getattr item :dtype))
             :cached-p (if (getattr si :cache-name) t nil)))

(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((info (getattr node :kernel-info)))
    ;; (For details, see coerce-dtyped-buffer)
    (let ((args (map 'list #'coerce-dtyped-buffer args (getattr node :dtypes))))
      (assert (functionp (compiled-kernel-caller info)) () "Could not find the function caller for the node ~a" node)
      (apply (compiled-kernel-caller info) args)
      (apply #'values (map 'list #'(lambda (x) (nth x args)) (compiled-kernel-out-positions info))))))

(defun timefy (name time)
  (if (= 0 time) name (intern (format nil "~(~a~)_~a" name time))))

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
    (let ((g (apply #'make-graph (explore top-id))))
      (values (tpsort-graph g) seen))))

(defun select-output-shape (wt)
  (if (and (buffer-orig-buffer-shape wt) (= (length (buffer-orig-buffer-shape wt)) (length (buffer-shape wt))))
      ;; If the view is slice?
      (if (every #'(lambda (view orig-shape) (or (null view) (equal view `(0 ,orig-shape 1 nil)))) (buffer-views wt) (buffer-orig-buffer-shape wt))
          (buffer-shape wt)
          (buffer-orig-buffer-shape wt))
      (buffer-shape wt)))

(defun make-alloc+view-node-from-buffer (wt w base-graph &aux (time 0))
  (when (some #'identity (buffer-views wt))
    ;; Consider the case: (NIL NIL (0 3 1 T))
    (setf (buffer-views wt)
          (loop for v in (buffer-views wt)
                for s in (buffer-shape wt)
                if v collect v
                  else collect (list 0 s 1 nil))))
  (let ((alloc
          (or
           ;; Prefer to use the original node, if w is defined in the graph and that's allocate.
           (let ((node (id->value base-graph w)))
             (when (and node (eql (node-type node) :Allocate))
               node))
           ;; Otherwise create it.
           (make-node :Buffer :Allocate (progn (incf time) (list (timefy w time)))
                      (append
                       (loop for s in (select-output-shape wt)
                             for nth upfrom 0
                             for v = (nth nth (buffer-views wt))
                             if (fourth v) collect 1
                               else collect s)
                       (buffer-stride wt))
                      :nrank (length (buffer-shape wt)) :dtype (buffer-dtype wt))))
        (view
          (when (some #'identity (buffer-views wt))
            (make-node :Buffer :View (progn (incf time) (list (timefy w time)))
                       (append
                        (list (timefy w (1- time)))
                        (buffer-shape wt)
                        (map 'list #'first (buffer-views wt))
                        (map 'list #'second (buffer-views wt))
                        (map 'list #'third (buffer-views wt))
                        (buffer-stride wt))
                       :broadcast (map 'list #'fourth (buffer-views wt))
                       :nrank (length (buffer-shape wt))))))
    (assert (= (length (select-output-shape wt)) (length (buffer-shape wt))))
    (values view alloc time)))

(defun id->output-map (graph)
  (let ((table (make-hash-table)))
    (loop for si in (graph-nodes graph) do
      (loop for item in (getattr si :items)
            for v = (getattr item :_output_type) do
              (when v
                (setf (car (node-writes v)) (car (node-reads v))
                      (gethash (car (node-reads v)) table) v))))
    table))

(defun schedule-graph->avm-graph (base-graph graph &aux (map (id->output-map graph)) (local-gensym-table (make-hash-table)))
  (declare (type Graph graph base-graph))
  (let ((nodes) (allocated))
    (flet ((merge-id (id)
             (multiple-value-bind (deps new-seen) (get-subgraph base-graph id allocated)
               (setf allocated new-seen)
               (dolist (d deps) (push d nodes))))
           (local-gensym (thing)
             (if (null (gethash thing local-gensym-table))
                 (setf (gethash thing local-gensym-table) 0)
                 (incf (gethash thing local-gensym-table)))
             (intern (format nil "~a_~a" thing (gethash thing local-gensym-table)))))
      (dolist (node (graph-nodes graph))
        (cond
          ((getattr node :allocate-p)
           (dolist (i (getattr node :items))
             (when (null (intersection (node-writes i) allocated))
               (push i nodes)
               (mapc #'merge-id (node-reads i))
               (push (car (node-writes i)) allocated))))
          ((null (getattr node :jitable)) ;; Relocating non-jitable ops
           (dolist (i (getattr node :items))
             (when (null (intersection (node-writes i) allocated))
               (mapc #'merge-id (node-reads i))
               (push i nodes)
               (dolist (w (node-writes i)) (push w allocated)))))
          ((getattr node :jitable)
           (let ((replacements (make-hash-table)))
             (loop for w in (getattr node :storage-id-dst)
                   for wt in (getattr node :write-types)
                   if (null (find w allocated)) do
                     (multiple-value-bind (view alloc time) (make-alloc+view-node-from-buffer wt w base-graph)
                       (mapc #'merge-id (node-reads alloc))
                       (push alloc nodes)
                       (when view
                         (mapc #'merge-id (cdr (node-reads view)))
                         (push view nodes))
                       (setf (gethash w replacements) (timefy w time)))
                     (push w allocated))
             (loop for (s . type) in (getattr node :dynamic-shapes)
                   if (and (null (find s allocated)) (id->value base-graph s)) do
                     (merge-id s))
             (dolist (w (node-writes node)) (push w allocated))
             (let ((kernel (make-compiled-kernel-node node graph)))
               ;; Insert view before using variables which is defined as pointer in the kernel but scalar in the vmgraph.
               (loop for r in (node-reads node)
                     for rt in (getattr node :read-types)
                     for nth upfrom (+ (length (node-writes node)) (length (getattr node :dynamic-shapes)))
                     for d = (find r (reverse nodes) :key #'node-writes :test #'member)
                     for dt = (and d (getattr d :_type_relay :allow-undefined t) (car (relay-writes (read-type-relay d))))
                     if (and d dt (or (= 0 (buffer-nrank dt)) (= 0 (buffer-nrank rt))) (not (= (buffer-nrank dt) (buffer-nrank rt))))
                       do (multiple-value-bind (view _ time) (make-alloc+view-node-from-buffer rt r base-graph)
                            (declare (ignore _))
                            (let ((key (local-gensym r)))
                              (mapc #'merge-id (cdr (node-reads view)))
                              (setf (car (node-writes view)) key
                                    (nth nth (node-reads kernel)) key)
                              (push view nodes)
                              (setf (gethash r replacements) (timefy r time)))))
               (setf (node-reads kernel) (map 'list #'(lambda (x) (or (gethash x replacements) x)) (node-reads kernel)))
               (push kernel nodes))
             ;; Merging view after the JIT_KERNEL invocation
             (loop for w in (node-writes node)
                   for out-view = (gethash w map)
                   if out-view do
                     (mapc #'merge-id (cdr (node-reads out-view)))
                     (push out-view nodes))))
          (T (error "schedule-graph->avm-graph: dont know how to merge ~a" node))))
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
   ;; The number of reference counters are the same => memory-planner should produce the same result
   (equal (getattr si1 :reference-counters) (getattr si2 :reference-counters))
   (every #'(lambda (x) (>= x 0)) (getattr si1 :reference-counters))
   (every #'(lambda (x) (>= x 0)) (getattr si2 :reference-counters))
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
                 (assert (eql k1 k2))
                 (let ((a1 (getattr x k1))
                       (a2 (getattr y k2)))
                   ;; Caten/AIR has an assertion that all nodes are "dumpable"
                   ;; i.e.: attrs that impacts on the computation results are always typed number/symbol/list/bool
                   (if (eql k1 :_read_views)
                       t
                       (if (and (typep a1 'attr-value-type) (typep a2 'attr-value-type))
                           (equal a1 a2)
                           t ;; [FIXME] isn't it danger? if attrs are not found, they ignore it!
                           ))))
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

(defun maybe-pmapc (f list &key (slope 1))
  (flet ((is-heavy-p (x)
           (and (getattr x :jitable) (null (getattr x :cache-name)))))
    (let ((list-to-lower
            (loop for x in list
                  ;; pick up the elements which takes a long time to compile
                  if (is-heavy-p x)
                    collect x))
          (list-not-to-lower
            (loop for x in list
                  if (Not (is-heavy-p x))
                    collect x)))
      (if (and (> (ctx:getenv :PARALLEL) 1) (>= (length list-to-lower) (* (ctx:getenv :PARALLEL) slope)))
          (let ((lparallel:*kernel*
                  (lparallel:make-kernel (ctx:getenv :PARALLEL)
                                         :bindings `((caten/codegen/expr-cache:*expr-cache* . ,caten/codegen/expr-cache:*expr-cache*)
                                                     (caten/common.logger::*progress* . ,caten/common.logger::*progress*)))))
            (lparallel:pmapc f list-to-lower)
            (mapc f list-not-to-lower))
          (mapc f list)))))

(defmacro with-printing-as-chunk ((stream) &body body)
  "Synchronize the stream if the compilation is running in parallel."
  `(if (null lparallel:*kernel*)
       (let ((,stream t)) ,@body)
       (format t "~a" (with-output-to-string (,stream) (let ((caten/common.logger:*default-stream* ,stream)) ,@body)))))

(defun jit (avm
            &key
              (renderer (or (ctx:getenv :JIT_BACKEND) :clang))
              (dir nil)
            &aux
              (renderer (if (keywordp renderer) (get-default-renderer renderer) renderer))
              (auto-scheduler
               (when (= (ctx:getenv :AUTO_SCHEDULER) 1)
                 (or (%renderer-get-auto-scheduler renderer)
                     (error "Cannot enable auto-scheduler without the renderer support.~%Use define-auto-scheduler and define-hook-auto-scheduler and compilers will recognise it.~%or, set AUTO_SCHEDULER=0 to supress this error.")))))
  "
```
(jit avm &key (renderer :clang) (dir nil))
```
Runs the JIT compilation for the given AVM."
  (declare (type AVM avm))
  (when (= 2 (ctx:getenv :DOT)) (->dot (avm-graph avm) :title "Base Graph"))
  (run-type-infer avm)
  ;; 2. Applying JIT Specific Graph Rewriting Rules in advance (e.g.: Propagete Views)
  (apply-rewriting-rules avm)
  ;; 3. Running the scheduler
  (let ((base-graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (avm-graph avm)))))
        (schedule-graph (graph-schedule (avm-graph avm)))
        ;; 4. Gathering the dynamic shapes used in the graph.
        (symbolics
          (remove-duplicates
           (loop for node in (graph-nodes (avm-graph avm))
                 if (and (eql (node-type node) :LOAD) (symbolp (getattr node :value)))
                   collect (getattr node :value))))
        (pointer-map (make-hash-table)))
    (declare (type Graph schedule-graph))
    (with-expr-cache (:pointer-map pointer-map) ;; Initialize a cache to treat (EXPR: a*b) as a symbolic and make symbolic collapsed loops as an affine loop.
      ;; 5. Minifying the number of schedules, (reuse kernels)
      (when (not (= 1 (ctx:getenv :NO_SCHEDULE_CACHE)))
        (minify-equivalent-schedule schedule-graph))
      ;; 6. Start JIT Compilation. (Performing group by group)
      (let ((total-kernels (count-if #'(lambda (x) (getattr x :jitable)) (graph-nodes schedule-graph))))
        (when (>= (ctx:getenv :JIT_DEBUG) 2)
          (print-info "JIT Compilation Start (AVM=~a)" (avm-name avm)))
        (with-progress (total-kernels :debug (if (>= (ctx:getenv :JIT_DEBUG) 2) 1 -1) :timeit nil)
          (maybe-pmapc
           #'(lambda (x &aux (start (get-internal-real-time)))
               (with-printing-as-chunk
                   (stream)
                 (when (and (getattr x :jitable) (getattr x :cache-name))
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (print-progress "~a" (getattr x :name))
                     (format stream "=====> (Skipped) redirect to ~a~%" (getattr x :cache-name))))
                 (when (getattr x :jitable)
                   (when (and (>= (ctx:getenv :JIT_DEBUG) 2) (null (getattr x :cache-name)))
                     (print-progress "~a" (getattr x :name))
                     (format stream "=====> Lowering to blueprint~%"))
                   (when (null (getattr x :cache-name))
                     ;; 7. Running Lowerer
                     (lower-schedule-item x (avm-graph avm) schedule-graph)
                     (when (>= (ctx:getenv :JIT_DEBUG) 2)
                       (print-blueprint (getattr x :blueprint) stream))
                     ;; 8. Lower into Polyhedral IR
                     (when (and (>= (ctx:getenv :JIT_DEBUG) 2) (null (getattr x :auto-schedule-p)) (>= (ctx:getenv :AUTO_SCHEDULER) 1))
                       (format stream "=====> Skipping Auto Scheduler (Symbolic incremental or scalar kernel)~%"))
                     (when (and (>= (ctx:getenv :AUTO_SCHEDULER) 1) (getattr x :auto-schedule-p))
                       (when (>= (ctx:getenv :JIT_DEBUG) 2)
                         (format stream "=====> Lowering to Polyhedral IR~%"))
                       (scop x symbolics)
                       (when (>= (ctx:getenv :JIT_DEBUG) 2)
                         (format stream "=====> Auto Scheduler~%"))
                       ;; 9. Optimizing: Tiles, Parallelizing, Vectorizing, Unrolling
                       (auto-schedule auto-scheduler x)
                       (when (>= (ctx:getenv :JIT_DEBUG) 2)
                         (print (getattr x :polyhedral) stream)
                         (fresh-line stream)
                         (format stream "=====> Optimized kernel~%")
                         (print-blueprint (getattr x :blueprint) t)))
                     (when (>= (ctx:getenv :JIT_DEBUG) 2)
                       (format stream "Compilation Time : ~A(sec)" (float (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
                     (schedule-item-write-define-global x)))))
           (graph-nodes schedule-graph)))
        (mapc
         #'(lambda (x)
             (when (getattr x :cache-name)
               (when (>= (ctx:getenv :JIT_DEBUG) 4)
                 (fresh-line)
                 (print-info "Copying cache ~a => ~a" (getattr x :name) (getattr x :cache-name)))
               (lower-cached-schedule-item x schedule-graph)))
         (graph-nodes schedule-graph))
        ;; 10. Running memory-planner, update the storage-id
        (setf schedule-graph (->graph-with-tpsort schedule-graph))
        (verify-graph schedule-graph) ;; Sort the graph for memory planner
        (when (not (= 1 (ctx:getenv :NO_MEMORY_PLANNER)))
          (when (>= (ctx:getenv :JIT_DEBUG) 2)
            (fresh-line)
            (print-info "Running the memory planner..."))
          (run-memory-planner schedule-graph symbolics base-graph))
        (when (>= (ctx:getenv :JIT_DEBUG) 2)
          (fresh-line)
          (print-info "Rendering ..."))
        (dolist (s (graph-nodes schedule-graph))
          (when (and (getattr s :jitable) (getattr s :blueprint))
            (setf (getattr s :rendered-object) (%render-kernel renderer s))))
        ;; 11. Complete (Render by the renderer)
        (when (>= (ctx:getenv :JIT_DEBUG) 2)
          (fresh-line)
          (print-info "Compiling ..."))
        (%compile-kernel renderer (graph-nodes schedule-graph) dir)
        ;; [TODO] Improve schedule-graph->avm-graph
        (let ((new-graph (schedule-graph->avm-graph base-graph schedule-graph)))
          (setf (graph-outputs new-graph) (graph-outputs schedule-graph))
          (when (>= (ctx:getenv :JIT_DEBUG) 4)
            (print-info "Final Scheduling Graph:")
            (pprint-graph schedule-graph)
            (when (= (ctx:getenv :DOT) 2) (->dot schedule-graph :title "Schedule Graph (Final)"))
            (print-info "Final VM Graph:")
            (print new-graph))
          (setf (avm-graph avm) new-graph
                (avm-tape-length avm) (length (graph-nodes new-graph))
                (avm-pc avm) 0
                (avm-variables avm) (make-hash-table)))
        avm))))
