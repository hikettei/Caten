(defpackage :caten/codegen/search
  (:use :cl :caten/ir :caten/air)
  (:export
   #:Auto-Scheduler
   #:Strategy
   #:define-auto-scheduler)
  (:export
   #:get-optimized-ast
   #:search-optimized-ast))

(in-package :caten/codegen/search)

(defclass Auto-Scheduler () ((strategy :type Auto-Scheduler-Strategy)))

(defstruct Strategy
  (use-tile-gpu 0) (global-max) (local-max) (shared-max)
  (use-parallel 0))

(defmacro define-auto-scheduler
  (name &key
          (use-tile-gpu 0) (global-max) (local-max) (shared-max) ;; Configurations for GPU Coincidence
          (use-parallel 0) ;; Configurations for CPU Coincidence
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
;; ~~ BEAM Search ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod get-optimized-ast ((auto-scheduler Auto-Scheduler) (ast Graph))
  (let ((strategy (slot-value auto-scheduler 'strategy)))
    ast))

(defmethod search-optimized-ast ((auto-scheduler Auto-Scheduler) (ast Graph))
  (let ((strategy (slot-value auto-scheduler 'strategy)))
    ast))
