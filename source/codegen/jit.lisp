(defpackage :caten/codegen/jit
  (:use :cl :caten/runtime :caten/air :caten/codegen/iteration :caten/codegen/rewriting-rules :caten/codegen/byoc
        :caten/codegen/scheduler :caten/common.logger :caten/codegen/blueprint :caten/codegen/realize)
  (:import-from :caten/codegen/helpers #:coerce-dtyped-buffer)
  (:import-from :caten/codegen/memory-planner #:run-memory-planner)
  (:export #:codegen #:jit))

(in-package :caten/codegen/jit)
;; ~~ Compiler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun minify-equivalent-schedule (schedule-graph)
  (let ((tgts (loop for node in (graph-nodes schedule-graph)
                    if (eql (getattr node :type) :kernel)
                      collect node))
        (seen))
    (loop for tgt in tgts
          for eql-schedule = (find tgt seen :test #'schedule-item-equal)
          if eql-schedule
            do (setf (getattr tgt :cache-name) (getattr eql-schedule :name))
          else
            do (push tgt seen))
    schedule-graph))

(defun codegen (runtime &key (backend (ctx:getenv :BACKEND)) (get-schedule-p nil))
  "
```
(codegen runtime &key (backend (ctx:getenv :BACKEND)))
```

Creates a JIT-compiled RuntimeGraph from the given runtime-graph.
"
  (declare (type GraphRuntime runtime))
  ;; Get configurations for the backend
  (multiple-value-bind (buffer-type runtime-type renderer-type kernel auto-scheduler is-jit) (apply #'values (get-backend-configs backend))
    (declare (ignore auto-scheduler))
    (when (null is-jit) (setf (runtime-buffer-type runtime) buffer-type) (return-from codegen runtime))
    (when (= 2 (ctx:getenv :DOT)) (->dot (runtime-graph runtime) :title "Base Graph"))
    ;; Running shape inference
    (graph-infer-type-relay (runtime-graph runtime))
    ;; Applying JIT Specific Graph Rewriting Rules in advance (e.g.: Propagete Views)
    (apply-rewriting-rules runtime)
    (let ((renderer (make-instance renderer-type))
          (base-graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (runtime-graph runtime)))))
          (schedule-graph (graph-schedule (runtime-graph runtime))))
      ;; Minifying the number of duplicated items.
      (unless (= 1 (ctx:getenv :NO_SCHEDULE_CACHE)) (minify-equivalent-schedule schedule-graph))
      (let ((total-kernels (count-if #'(lambda (x) (eql :kernel (getattr x :type))) (graph-nodes schedule-graph)))
            (JIT_DEBUG (ctx:getenv :JIT_DEBUG)))
        (when (>= JIT_DEBUG 1)
          (print-info "[CODEGEN] | Captured ~a kernel~a (JIT_DEBUG>=1 BACKEND=~a) "
                      total-kernels (if (= total-kernels 1) "" "s") (ctx:getenv :BACKEND)))
        ;; Running lowerer
        (with-progress (total-kernels :debug (if (>= JIT_DEBUG 2) 1 -1) :timeit nil)
          (mapc
           #'(lambda (x &aux (start (get-internal-real-time)))
               (when (and (eql (getattr x :type) :kernel) (getattr x :cache-name))
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (print-progress "~a" (getattr x :name))
                   (format t "=====> (Skipped) redirect to ~a~%" (getattr x :cache-name))))
               (when (eql :kernel (getattr x :type))
                 (when (and (>= JIT_DEBUG 2) (null (getattr x :cache-name)))
                   (print-progress "~a" (getattr x :name))
                   (format t "=====> Lowering to blueprint~%"))
                 (when (null (getattr x :cache-name))
                   ;; Lowering into the AST
                   (lower-schedule-item x base-graph schedule-graph)
                   (let ((end (get-internal-real-time)))
                     (when (>= JIT_DEBUG 2)
                       (pprint-graph (getattr x :blueprint))
                       (print-blueprint (getattr x :blueprint) t))
                     (when (>= JIT_DEBUG 2)
                       (format t "Lowering Time : ~A(sec)" (float (/ (- end start) internal-time-units-per-second))))))))
           (graph-nodes schedule-graph)))
        (when get-schedule-p (return-from codegen schedule-graph))
        ;; Running Memory Planner
        (when (= 0 (ctx:getenv :NO_MEMORY_PLANNER))
          ;; [TODO] Bring Back Memory Planner!
          ;(run-memory-planner schedule-graph nil base-graph)
          )
        ;; Finalize the realize
        (mapc
         #'(lambda (x) (when (eql (getattr x :type) :kernel) (schedule-item-sync-realize x)))
         (graph-nodes schedule-graph))
        ;; ScheduleGraph -> RuntimeGraph (schedule/memory planning is fixed)
        (let ((runtime-graph (schedule-graph->runtime-graph schedule-graph base-graph kernel)))
          (when (= JIT_DEBUG 1) (print-info "[CODEGEN] | Rendering with ~a" renderer))
          (mapc
           #'(lambda (x) (when (eql (node-type x) :KERNEL) (caten/codegen/byoc:%render-kernel renderer (getattr x :kernel-info))))
           (graph-nodes runtime-graph))
          (when (= (ctx:getenv :BEAM) 0) ;; If BEAM >= 1, the blueprint is further optimized and then compiled.
            (when (= JIT_DEBUG 1) (print-info "[CODEGEN] | Compiling with ~a" renderer))
            (caten/codegen/byoc:%compile-kernel
             renderer
             (loop for node in (graph-nodes runtime-graph) if (eql (node-type node) :KERNEL) collect (getattr node :kernel-info))
             nil))
          ;; Sync ID2Tensor
          (loop for node in (graph-nodes runtime-graph)
                if (eql (node-type node) :SYNCHRONIZE) do
                  (setf (gethash (car (node-writes node)) (runtime-id2tensor runtime))
                        (gethash (car (subseq (node-reads node) (getattr node :n-kernel-args))) (runtime-id2tensor runtime))))
          (when (>= (ctx:getenv :JIT_DEBUG) 1) (print-info "[CODEGEN] | Completed"))
          ;; [TODO] Backward Graph Support
          (make-runtime runtime-graph :fw-outputs (graph-outputs runtime-graph) :bw-outputs (runtime-bw-outputs runtime) :runtime runtime-type :id2tensor (runtime-id2tensor runtime) :buffer-type buffer-type :params (runtime-params runtime) :renderer renderer))))))

(defun jit (runtime &key (backend (ctx:getenv :BACKEND)) (dir nil))
  "
```
(jit runtime &key (backend (ctx:getenv :BACKEND)))
```
"
  (declare (type GraphRuntime runtime))
  (let ((runtime (codegen runtime :backend backend)))
   ;;  (when (>= 1 (ctx:getenv :JIT_DEBUG)) (print-info "Compiling ~a kernels ..." (count-if #'(lambda (x) (eql (node-type x) :JIT_KERNEL)) (graph-nodes graph))))
    ;; [TODO] Use Runtime instead of renderer when doing %compile-kernel
    (when (and (>= (ctx:getenv :BEAM) 1) (get-backend-jit-p (ctx:getenv :BACKEND)))
      (autotune runtime))
    runtime))

(defparameter *autotune-node-callback* nil)
(defun register-autotune-node (node) (push node *autotune-node-callback*))
(defun autotune (runtime)
  ;; An entrypoint for the AutoScheduler
  ;; Optimizing the runtime end-to-end.
  ;; If runtime has an symbolic input, try them all
  ;; Optimize kernel by kernel
  ;; It is possuble to fuse KERNEL and KERNEL Pattern.
  ;; [TODO]
  ;; - Implement HashedGraph
  ;; - 入力のSymbolicに応じて変動する。
  (let ((*autotune-node-callback*))
    (%autotune runtime)
    (when (typep (runtime-graph runtime) 'Graph) ;; TODO: Delete this after everything is DAG refactor
      (setf (graph-nodes (runtime-graph runtime))
            (loop with rewritten = (apply #'append (map 'list #'node-writes *autotune-node-callback*))
                  for node in (graph-nodes (runtime-graph runtime))
                  if (not (intersection (Node-writes node) rewritten)) collect node)))
    (insert-nodes (runtime-graph runtime) *autotune-node-callback*))
  (verify-graph (runtime-graph runtime)))

(defun %autotune (runtime)
  "
```
(%autotune runtime)
```
Automatically optimizes the given runtime graph which is static.
"
  
  ;; [TODO] Schedule Cache as well as BEAM Cache!!
  ;; Replacing realize-node(:KERNEL) -> realize-node-with-autotuning(:KERNEL)
  (let ((caten/codegen/byoc:*autotune-mode-p* t))
    (when (> (ctx:getenv :JIT_DEBUG) 0) (print-info "[SEARCH] | AutoTuning the graph (BEAM=~a)" (ctx:getenv :BEAM)))
    (runtime-forward runtime)))
