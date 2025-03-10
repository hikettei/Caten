(defpackage :caten/codegen/search
  (:use :cl :caten/ir :caten/air)
  (:export
   #:Auto-Scheduler
   #:Strategy
   #:define-auto-scheduler)
  ;; Schedule
  (:export
   #:Schedule-Node #:make-schedule-node-from-blueprint #:make-schedule-node
   #:schedule-node-schedule #:schedule-node-node
   #:schedule-node-clone #:schedule-node-move
   #:schedule-node-get-type #:schedule-node-get-child #:schedule-node-get-n-children)
  (:export
   #:get-optimized-ast #:search-optimized-ast))

(in-package :caten/codegen/search)
;; ~~ Abstractions ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Auto-Scheduler () ((strategy :type Auto-Scheduler-Strategy)))

(defstruct Strategy
  (use-tile-gpu 0) (global-max) (local-max) (shared-max)
  (use-parallel 0))

(defmacro define-auto-scheduler
  (name &key
          (use-tile-gpu 0) (global-max) (local-max) (shared-max) ;; Configurations for GPU Coincidence
          (use-parallel 0) ;; Configurations for CPU Coincidence
          ;; [TODO] Vectorize, Upcast, TileSize, etc
          )
  "The macro `define-auto-scheduler` will declare an optimization strategy for the Caten Auto Scheduler.

- use-tile-gpu[fixnum] Set > 1 to allow the compiler to tile bands to generate a parallelized gpu kernel. The value will be the maximum rank of tiling.
- global-max[list, optional] restrict the maximum size of the griddim. The value will be a list of fixnums with the same length as use-tile-gpu.
- local-max[list, optional] restrict the maximun size of the thread. The value will be a list of fixnums with the same length as use-tile-gpu.
- shared-max[or null fixnum] If specified, the search can generate `Prefetch` optimization. This parameter restricts the maximum size of the shared memory. 
- use-parallel[fixnum] Set = 1 to allow the compiler to insert @parallel annotations to generate a parallelized cpu kernel. Note that this value is not orthogonal to use-tile-gpu.
"
  (declare (type (integer 0 1) use-parallel)
           (type (integer 0 3) use-tile-gpu)
           (type list global-max local-max)
           (type (or null fixnum) shared-max))
  (when (> use-tile-gpu 0)
    (assert (= use-parallel 0) () "use-tile-gpu is not orthogonal to use-parallel. Please set use-parallel=0"))
  (when (> use-parallel 0)
    (assert (= use-tile-gpu 0) () "use-parallel is not orthogonal to use-tile-gpu. Please set use-tile-gpu=0"))
  (when (= use-tile-gpu 0)
    (assert (and (null global-max) (null local-max)) () "use-parallel does not support global-max and local-max. Please set them to nil"))

  `(progn
     (defclass ,name (Auto-Scheduler) nil)
     (defmethod initialize-instance :after ((auto-scheduler ,name) &key)
       (setf (slot-value auto-scheduler 'strategy)
             (make-strategy
              :use-tile-gpu ,use-tile-gpu :global-max ',global-max :local-max ',local-max :shared-max ,shared-max :use-parallel ,use-parallel)))))
;; [TODO] Implement the callback w/ AutoScheduler for each optimization pharse
;; ~~ Schedule ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Schedule-Node
  (schedule (error "AST must occur") :type FastGraph)
  (node (error "Node must occur") :type Node))

(defun make-schedule-node-from-blueprint (blueprint)
  (declare (type Graph blueprint))
  (assert (= 1 (length (graph-outputs blueprint))) () "make-schedule-node-from-blueprint: AST must start from a single node")
  (make-schedule-node :schedule blueprint :node (id->value blueprint (car (graph-outputs blueprint)))))

(defmethod schedule-node-clone ((sn schedule-node)) ;; copy-graph = shallow copy.
  (make-schedule-node :schedule (copy-graph (schedule-node-schedule sn)) :node (schedule-node-node sn)))

(defmethod schedule-node-move ((sn schedule-node) (new-node node))
  (make-schedule-node :schedule (schedule-node-schedule sn) :node new-node))

(defmethod schedule-node-get-type ((sn schedule-node)) (node-type (schedule-node-node sn)))

(defmethod schedule-node-get-child ((sn schedule-node) (n fixnum))
  (schedule-node-move sn (or (id->value (schedule-node-schedule sn) (nth n (node-reads (schedule-node-node sn))))
                             (error "schedule-node-get-n-child: No such child (N=~a), node=~a" n (schedule-node-node sn)))))

(defmethod schedule-node-get-n-children ((sn schedule-node)) (length (node-reads (schedule-node-node sn))))

(defmethod schedule-node-band-get-depth ((sn schedule-node) &aux (count 1))
  (assert (eql :FOR (schedule-node-get-type sn)) () "schedule-node-band-get-depth: Node must be a band")
  (labels ((explore (node)
             (when (and (= (schedule-node-get-n-children node) 2)
                        (let ((child (schedule-node-get-child node 1)))
                          (and (eql (schedule-node-get-type child) :FOR)
                               (getattr (schedule-node-node node) :band)
                               (eql (getattr (schedule-node-node child) :band) (getattr (schedule-node-node node) :band)))))
               (incf count)
               (explore (schedule-node-get-child node 1)))))
    (explore sn)
    count))
;; ~~ Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~)
(defmethod get-optimized-ast ((auto-scheduler Auto-Scheduler) (ast Graph))
  "Optimizes the given ast using rule-based auto-scheduler."
  (with-slots ((use-tile-gpu use-tile-gpu) (global-max global-max) (local-max local-max) (shared-max shared-max) (use-parallel use-parallel)) (slot-value auto-scheduler 'strategy)
    (let ((schedule (make-schedule-node-from-blueprint ast))
          (seen-bands nil))
      (declare (type list seen-bands))
      (labels ((explore-children (node)
                 (mapc #'(lambda (x &aux (v (id->value ast x))) (when v (explore (make-schedule-node :schedule ast :node v)))) (node-reads (schedule-node-node node))))
               (explore (node)
                 (declare (type Schedule-Node node))
                 (case (schedule-node-get-type node)
                   (:FOR
                    (when (let ((b (getattr (schedule-node-node node) :band))) (or (null b) (null (member b seen-bands))))
                      (push (getattr (schedule-node-node node) :band) seen-bands)
                      (case (getattr (schedule-node-node node) :mark)
                        (:coincident
                         ;(ast-band-tile (schedule-node-schedule node) (schedule-node-node node) `(4))
                         
                         )
                        (:reduction
                         )
                        (:noopt)))
                    (explore-children node))
                   (:EXPR) ;; Filter
                   (otherwise (explore-children node)))))
        (explore schedule))
      (simplify-ast ast)
      ast)))

(defmethod search-optimized-ast ((auto-scheduler Auto-Scheduler) (ast Graph))
  (with-slots ((use-tile-gpu use-tile-gpu) (global-max global-max) (local-max local-max) (shared-max shared-max) (use-parallel use-parallel)) (slot-value auto-scheduler 'strategy)
    
    ast))
;; how to detect the data reuse?
;; k*_gid0 + _gid1 <- val_16[n*_gid0 + _gid2]
;; k*_gid0 + _gid1 <- val_19[_gid1+k*_gid2]

;; _gid0 * 128 + _gid2 <- val_9[_gid0]
;; _gid0 * 128 + _gid2 <- val_14[_gid2+128*_gid1]
