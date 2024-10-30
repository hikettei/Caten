(defpackage :caten/codegen/jit
  (:documentation "This is an entry point for JIT. JIT compiles unoptimized AVM into optimized AVM.")
  (:use :cl :caten/air :caten/codegen/shape-inference)
  (:import-from
   :caten/avm
   #:AVM
   #:Buffer
   #:avm-graph
   #:avm-name
   #:avm-tape-length
   #:buffer-nrank
   #:buffer-shape
   #:buffer-views
   #:buffer-stride
   #:buffer-dtype
   #:buffer-inferred-permute
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
   #:graph-schedule
   #:find-item2id-projection
   #:retrieve-blueprint-from-cache)
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
   #:%render-kernel)
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
             (append (node-writes si) (node-reads si))
             :output-buffer-n (length (node-writes si))
             :kernel-info (make-compiled-kernel-from-si si graph)))

(defmethod %impl (device (op (eql :JIT_KERNEL)) graph node args)
  (let ((info (getattr node :kernel-info))
        (out-n (getattr node :output-buffer-n)))
    (assert (functionp (compiled-kernel-caller info)) () "Could not find the function caller for the node ~a" node)
    (apply (compiled-kernel-caller info) args)
    (apply #'values (subseq args 0 out-n))))

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

(defun schedule-graph->vmop (graph)
  (declare (type Graph graph))
  (verify-graph graph)
  (setf graph (->graph graph)) ;; Convert from FastGraph to Graph to sort the order
  (let ((nodes) (allocated))
    (dolist (node (graph-nodes graph))
      (cond
        ((getattr node :allocate-p)
         (push (car (node-writes node)) allocated)
         (dolist (i (getattr node :items))
           (push i nodes)))
        ((null (getattr node :jitable))
         (dolist (w (node-writes node)) (push w allocated))
         (dolist (i (getattr node :items))
           (push i nodes)))
        ((getattr node :jitable)
         (loop for w in (node-writes node)
               for wt in (getattr node :write-types)
               if (null (find w allocated)) do
                 (push
                  (make-node :Buffer :Allocate (list w) (append (buffer-shape wt) (buffer-stride wt))
                             :nrank (buffer-nrank wt) :dtype (buffer-dtype wt))
                  nodes)
                 (push w allocated))
         (push (make-compiled-kernel-node node graph) nodes))
        (T (error "schedule-graph->vmop: dont know how to merge ~a" node))))
    (apply #'make-graph (nreverse nodes))))
;; Priority
;; - [ ] Auto Scheduler for Tiling/Vectorizing
;; - [ ] Caching the kernel+Memory Planner
;; - [ ] Kernel Generation by CLANG
;; - [ ] Running the kernel
;; - [ ] Passing all tests
;; - [ ] Running tinyllama
;; - [ ] Support merging CUSTOM/Foreign/Pre-compiled kernel
(defun jit (avm &key (renderer (or (ctx:getenv :JIT_BACKEND) :clang))
                &aux (renderer (if (keywordp renderer) (get-default-renderer renderer) renderer)))
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
                   collect (getattr node :value))))
        (projection-table (make-hash-table)))
    (declare (type Graph schedule-graph))
    (dolist (n (graph-nodes schedule-graph))
      (setf (gethash (getattr n :name) projection-table) (find-item2id-projection n)))
    ;; 5. Minifying the number of schedules, (reuse kernels)
    (minify-equivalent-schedule schedule-graph)
    ;; 6. Start JIT Compilation. (Performing by group)
    (let ((total-kernels (count-if #'(lambda (x) (getattr x :jitable)) (graph-nodes schedule-graph))))
      (when (>= (ctx:getenv :JIT_DEBUG) 2)
        (print-info "JIT Compilation Start (AVM=~a)" (avm-name avm)))
      (with-progress (total-kernels :debug (if (>= (ctx:getenv :JIT_DEBUG) 2) 1 -1) :timeit nil)
        (with-expr-cache () ;; Initialize a cache to treat (EXPR: a*b) as a symbolic and make symbolic collapsed loops as an affine loop.
          (mapc
           #'(lambda (x &aux (start (get-internal-real-time)))
               (when (and (getattr x :jitable) (getattr x :cache-name) (>= (ctx:getenv :JIT_DEBUG) 2))
                 (print-progress "~a" (getattr x :name))
                 (format t "=====> (Skipped) redirect to ~a~%" (getattr x :cache-name)))
               (when (and (getattr x :jitable) (null (getattr x :cache-name)))
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (print-progress "~a" (getattr x :name))
                   (format t "=====> Lowering to blueprint~%"))
                 ;; 7. Running Lowerer
                 (lower-schedule-item x (avm-graph avm) schedule-graph)
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
                   (auto-schedule x)
                   (when (>= (ctx:getenv :JIT_DEBUG) 2)
                     (print (getattr x :polyhedral))
                     (fresh-line)
                     (format t "=====> Optimized kernel~%")
                     (print-blueprint (getattr x :blueprint) t)))
                 (when (>= (ctx:getenv :JIT_DEBUG) 2)
                   (format t "Compilation Time : ~A(sec)" (float (/ (- (get-internal-real-time) start) internal-time-units-per-second))))))
           (graph-nodes schedule-graph)))))
    (dolist (n (graph-nodes schedule-graph))
      (when (and (getattr n :jitable) (null (getattr n :blueprint)))
        (retrieve-blueprint-from-cache n schedule-graph projection-table)))
    ;; 10. Running memory-planner, update the storage-id
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Running the memory planner..."))
    ;; (run-memory-planner schedule-graph)
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Rendering ..."))
    (dolist (s (graph-nodes schedule-graph))
      (when (getattr s :jitable)
        ;; Lower the input argument buffer
        (schedule-item-write-define-global s)
        (setf (getattr s :rendered-object) (%render-kernel renderer s))))
    ;; 11. Complete (Render by the renderer)
    (when (>= (ctx:getenv :JIT_DEBUG) 2)
      (fresh-line)
      (print-info "Compiling ..."))
    
    (%compile-kernel renderer (graph-nodes schedule-graph))
    
    (let ((new-graph (schedule-graph->vmop schedule-graph)))
      (setf (avm-graph avm) new-graph
            (avm-tape-length avm) (length (graph-nodes new-graph))))
    avm))
;; MemoryPlanner w/ Caching the duplicated kernel?

;; Test Case1
;; (with-no-grad (caten/codegen:jit (caten (call (Transformer 64 4 1 1e-5 32) (make-tensor `(10 30)) (iconst 0)))))
;; Test Case2
;; (caten (!randn `(a b)))

;; memo: Transformer Model Initialization is slow.

