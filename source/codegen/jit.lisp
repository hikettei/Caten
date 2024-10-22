(defpackage :caten/codegen/jit
  (:documentation "This is an entry point for JIT. JIT compiles unoptimized AVM into optimized AVM.")
  (:use :cl :caten/air :caten/codegen/shape-inference)
  (:import-from
   :caten/avm
   #:AVM
   #:Buffer
   #:avm-graph
   #:avm-name
   #:buffer-shape
   #:buffer-views
   #:buffer-stride
   #:buffer-dtype
   #:buffer-inferred-permute)
  (:import-from
   :caten/codegen/shape-inference
   #:run-type-infer
   #:graph-infer-iteration-space)
  (:import-from
   :caten/codegen/rewriting-rules
   #:apply-rewriting-rules)
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
  (:export
   #:jit))

(in-package :caten/codegen/jit)

(defun buffer-equal (a b)
  (declare (optimize (speed 3))
           (type buffer a b))
  (and
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
                   ;; i.e.: important attrs are always number/symbol/list/bool
                   (if (and (typep a1 'attr-value-type) (typep a2 'attr-value-type))
                       (equal a1 a2)
                       t ;; isn't it danger?
                       )))
             attrs1 attrs2)))
         (let ((xt (read-type-relay x))
               (yt (read-type-relay y)))
           (and
            (every #'(lambda (a b) (buffer-equal a b)) (relay-reads xt) (relay-reads yt))
            (every #'(lambda (a b) (buffer-equal a b)) (relay-writes xt) (relay-writes yt))))))
    items1 items2)))

(defun minify-equivalent-schedule (schedule-graph)
  ;; CODE_SIZE=0 ()
  ;; CODE_SIZE=1 (Eager to Full Symbolic Compilation)
  (let ((tgts (loop for node in (graph-nodes schedule-graph)
                    if (getattr node :jitable)
                      collect node))
        (seen))
    (loop for tgt in (nreverse tgts)
          for eql-schedule = (find tgt seen :test #'schedule-item-equal)
          if eql-schedule
            do (setf (getattr tgt :cache-name) (getattr eql-schedule :name))
          else
            do (push tgt seen))
    schedule-graph))

;; [Milestone]
;; - [ ] schedule-graph -> Better Graph Splitting Strategy?
;; - [ ] Fuse Permute
;; - [ ] Scop+Polyhedral -> Can ISL find the optimal embedding kernel?
;; - [ ] Support Backward (Higher Order?)
;; - [ ] TODO Purge unnecessary stride computation (in symbolic!)
(defun jit (avm)
  "Runs the JIT compilation (destructive)"
  (declare (type AVM avm))
  ;; 1. Running the type inference
  (run-type-infer avm)
  ;; 2. JIT Specific Simplifier (Loop Collapse is here)
  (apply-rewriting-rules avm)
  ;; 3. Merge dims (e.g. (10 10 10) Tensor -> (10x10x10) Tensor)
  (graph-infer-iteration-space (avm-graph avm))
  ;; 4. Schedule
  (let ((schedule-graph (graph-schedule (avm-graph avm)))
        (symbolics
          (remove-duplicates
           (loop for node in (graph-nodes (avm-graph avm))
                 if (and (eql (node-type node) :LOAD) (symbolp (getattr node :value)))
                   collect (getattr node :value)))))
    (declare (type Graph schedule-graph))
    ;; 5. Loop Bound Inference (i.e.: OP -> Loop For transformation)))
    ;; Remove duplicated schedule compilation
    (minify-equivalent-schedule schedule-graph)
    (let ((total-kernels (count-if #'(lambda (x) (getattr x :jitable)) (graph-nodes schedule-graph))))
      (when (>= (ctx:getenv :JIT_DEBUG) 2)
        (print-info "JIT Compilation Start (AVM=~a)" (avm-name avm)))
      (with-progress (total-kernels :debug (if (>= (ctx:getenv :JIT_DEBUG) 2) 1 -1) :timeit nil)
        (with-expr-cache () ;; Initialize a cache to treat (EXPR: a*b) as a symbolic and make symbolic collapsed loops as an affine loop.
          (mapc
           #'(lambda (x &aux (start (get-internal-real-time)))
               (when (and (getattr x :jitable) (getattr x :cache-name) (>= (ctx:getenv :JIT_DEBUG) 2))
                 ;; [TODO] (skipped) if cached
                 (print-progress "~a" (getattr x :name))
                 (format t "=====> (Skipped) redirect to ~a~%" (getattr x :cache-name)))
               (when (and (getattr x :jitable) (null (getattr x :cache-name)))
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (print-progress "~a" (getattr x :name))
                   (format t "=====> Lowering to blueprint~%"))
                 ;; [TODO] Debug Info (Compilation time, Function Name, etc...)
                 (lower-schedule-item x (avm-graph avm) schedule-graph)
                 ;; 6. Lower into Polyhedral IR
                 (when (and (>= (ctx:getenv :JIT_DEBUG) 2) (null (getattr x :auto-schedule-p)) (>= (ctx:getenv :AUTO_SCHEDULER) 1))
                   (format t "=====> Skipping Auto Scheduler (Symbolic incremental or scalar kernel)~%"))
                 (when (and (>= (ctx:getenv :AUTO_SCHEDULER) 1) (getattr x :auto-schedule-p))
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (format t "=====> Lowering to Polyhedral IR~%"))
                   (scop x symbolics)
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (format t "=====> Auto Scheduler~%"))
                   ;; Optimizing: Tiles, Parallelizing, Vectorizing, Unrolling
                   (auto-schedule x)
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (print (getattr x :polyhedral))
                     (fresh-line)
                     (format t "=====> Optimized kernel~%")
                     (print-blueprint (getattr x :blueprint) t)))
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (format t "Compilation Time : ~A(sec)" (float (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))
           (reverse (graph-nodes schedule-graph))))))
    ;; 7. Running memory-planner, update the storage-id
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Running the memory planner..."))
    ;; (memory-planner)
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Rendering ..."))
    ;; 8. Complete
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Compiling ..."))
    t))

;; Test Case1
;; (with-no-grad (caten/codegen:jit (caten (call (Transformer 64 4 1 1e-5 32) (make-tensor `(10 30)) (iconst 0)))))
;; Test Case2
;; (caten (!randn `(a b)))

;; memo: Transformer Model Initialization is slow.

