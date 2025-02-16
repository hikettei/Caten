(defpackage :caten/codegen/jit
  (:use :cl :caten/runtime :caten/air :caten/codegen/backend :caten/codegen/type-relay :caten/codegen/rewriting-rules
        :caten/codegen/scheduler :caten/common.logger :caten/codegen/blueprint :caten/codegen/realize)
  (:import-from :caten/codegen/search #:get-optimized-ast #:search-optimized-ast)
  (:import-from :caten/codegen/helpers #:coerce-dtyped-buffer)
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
"
  (declare (type GraphRuntime runtime))
  ;; Get configurations for the backend
  (multiple-value-bind (buffer-type runtime-type renderer-type auto-scheduler is-jit) (apply #'values (get-backend-configs backend))
    (when (null is-jit) (setf (runtime-buffer-type runtime) buffer-type) (return-from codegen runtime))
    (when (= 2 (ctx:getenv :DOT)) (->dot (runtime-graph runtime) :title "Base Graph"))
    ;; Running shape inference
    (run-type-infer runtime)
    ;; Applying JIT Specific Graph Rewriting Rules in advance (e.g.: Propagete Views)
    (apply-rewriting-rules runtime)
    (let ((renderer (make-instance renderer-type))
          (auto-scheduler (make-instance auto-scheduler))
          (base-graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (runtime-graph runtime)))))
          (schedule-graph (graph-schedule (runtime-graph runtime))))
      ;; Minifying the number of duplicated items.
      (unless (= 1 (ctx:getenv :NO_SCHEDULE_CACHE)) (minify-equivalent-schedule schedule-graph))
      (let ((total-kernels (count-if #'(lambda (x) (eql :kernel (getattr x :type))) (graph-nodes schedule-graph)))
            (JIT_DEBUG (ctx:getenv :JIT_DEBUG)))
        (when (= JIT_DEBUG 1) (print-info "(JIT_DEBUG=1) Captured ~a kernel~a ..." total-kernels (if (= total-kernels 1) "" "s")))
        (when (>= JIT_DEBUG 2) (print-info "JIT Compilation Start") (print-info "Running lowerer ..."))
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
                       (format t "Compilation Time : ~A(sec)" (float (/ (- end start) internal-time-units-per-second))))))))
           (graph-nodes schedule-graph)))
        (when get-schedule-p (return-from codegen schedule-graph))
        ;; Running AutoScheduler
        ;; [TODO] Implement Advanced Fusion (e.g.: Matmul+Softmax+Matmul Fusion etc)
        (when (>= JIT_DEBUG 2)
          (fresh-line)
          (print-info "Autotuning the kernels ...")
          (ecase (ctx:getenv :OPTIMIZE)
            (0 (print-info "(OPTIMIZE=0) strategy = NOOPT"))
            (1 (print-info "(OPTIMIZE=1) strategy = RULE_BASE"))
            (2 (print-info "(OPTIMIZE=2) strategy = SEARCH"))))
        (with-progress (total-kernels :debug (if (>= JIT_DEBUG 2) 1 -1) :timeit nil)
          (mapc
           #'(lambda (x &aux (start (get-internal-real-time)))
               (when (and (eql (getattr x :type) :kernel) (null (getattr x :cache-name)))
                 (when (>= JIT_DEBUG 2)
                   (print-progress "~a" (getattr x :name))
                   (format t "====> Running Optimizer~%"))
                 (ecase (ctx:getenv :OPTIMIZE)
                   (0)
                   (1 (setf (getattr x :blueprint) (get-optimized-ast auto-scheduler (getattr x :blueprint))))
                   (2 (setf (getattr x :blueprint) (search-optimized-ast auto-scheduler (getattr x :blueprint)))))
                 (let ((end (get-internal-real-time)))
                   (when (>= JIT_DEBUG 2)
                     (format t "Optimized Kernel:~%")
                     (pprint-graph (getattr x :blueprint))
                     (print-blueprint (getattr x :blueprint) t))
                   (when (>= JIT_DEBUG 2)
                     (format t "Optimization Time: ~A(sec)" (float (/ (- end start) internal-time-units-per-second)))))))
           (graph-nodes schedule-graph)))
        ;; Running Memory Planner -> TODO
        ;; Purge unused allocations
        (mapc
         #'(lambda (x) (when (eql (getattr x :type) :kernel) (schedule-item-sync-realize x)))
         (graph-nodes schedule-graph))
        ;; Rendering
        (let ((runtime-graph (schedule-graph->runtime-graph schedule-graph base-graph)))
          (when (= JIT_DEBUG 1) (print-info "(JIT_DEBUG=1) Rendering with ~a" renderer))
          (mapc
           #'(lambda (x) (when (eql (node-type x) :JIT_KERNEL) (caten/codegen/renderer:%render-kernel renderer x)))
           (graph-nodes runtime-graph))
          runtime-graph)))))

(defun jit (runtime &key (backend (ctx:getenv :BACKEND)) (dir nil))
  "
```
(jit runtime &key (backend (ctx:getenv :BACKEND)))
```
"
  (declare (type GraphRuntime runtime))
  (let ((graph (codegen runtime :backend backend)))
    (when (>= 1 (ctx:getenv :JIT_DEBUG)) (print-info "Compiling ~a kernels ..." (count-if #'(lambda (x) (eql (node-type x) :JIT_KERNEL)) (graph-nodes graph))))
    ;; [TODO] Use Runtime instead of renderer when doing %compile-kernel
    (when (>= 1 (ctx:getenv :JIT_DEBUG)) (print-info "Completed"))))
