(defpackage :caten/codegen/jit
  (:use :cl :caten/runtime :caten/air :caten/codegen/backend :caten/codegen/type-relay :caten/codegen/rewriting-rules
        :caten/codegen/scheduler :caten/common.logger :caten/codegen/blueprint)
  (:export #:codegen #:jit))

(in-package :caten/codegen/jit)

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
;; [TODO] Runner here
(defun codegen (runtime &key (backend (ctx:getenv :BACKEND)))
  "
```
(codegen runtime &key (backend (ctx:getenv :BACKEND)))
```
"
  (declare (type GraphRuntime runtime))
  ;; Get configurations for the backend
  (multiple-value-bind (buffer-type runtime-type renderer-type auto-scheduler is-jit) (get-backend-configs backend)
    (when (null is-jit) (setf (runtime-buffer-type runtime) buffer-type) (return-from codegen runtime))
    (when (= 2 (ctx:getenv :DOT)) (->dot (runtime-graph runtime) :title "Base Graph"))
    ;; Running shape inference
    (run-type-infer runtime)
    ;; Applying JIT Specific Graph Rewriting Rules in advance (e.g.: Propagete Views)
    (apply-rewriting-rules runtime)
    (let ((renderer (make-instance renderer-type))
          (base-graph (apply #'make-graph (map 'list #'copy-node (graph-nodes (runtime-graph runtime)))))
          (schedule-graph (graph-schedule (runtime-graph runtime))))
      ;; Minifying the number of duplicated items.
      (unless (= 1 (ctx:getenv :NO_SCHEDULE_CACHE)) (minify-equivalent-schedule schedule-graph))
      (let ((total-kernels (count-if #'(lambda (x) (eql :kernel (getattr x :type))) (graph-nodes schedule-graph)))
            (JIT_DEBUG (ctx:getenv :JIT_DEBUG)))
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
                   (when (>= JIT_DEBUG 2)
                     (print-blueprint (getattr x :blueprint) t))
                   (when (>= JIT_DEBUG 2)
                     (format t "Compilation Time : ~A(sec)" (float (/ (- (get-internal-real-time) start) internal-time-units-per-second)))))))
           (graph-nodes schedule-graph)))
        ;; Running AutoScheduler (Maybe advanced fusion)

        ;; Running Memory Planner (Scalarify is here!)
        
        ))))

(defun jit (runtime &key (backend (ctx:getenv :BACKEND)))
  "
```
(jit runtime &key (backend (ctx:getenv :BACKEND)))
```
"
  (declare (type GraphRuntime runtime))
  (codegen runtime :backend backend))
