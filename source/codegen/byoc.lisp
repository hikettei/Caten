(defpackage :caten/codegen/byoc
  (:use :cl)
  ;; AbstractKernel
  (:export
   #:AbstractKernel #:kernel-name #:kernel-args #:kernel-flops #:kernel-output-buffers #:kernel-call #:kernel-blueprint
   #:*autotune-mode-p*)
  ;; Renderer
  (:export
   #:Renderer #:renderer-graph #:renderer-index-space
   #:%render-kernel #:%render-const #:%render-node #:%compile-kernel)
  ;; AutoScheduler
  (:export
   #:define-auto-scheduler
   #:auto-scheduler-strategy)
  ;; Backend
  (:export
   #:define-backend
   #:get-backend-buffer
   #:get-backend-runtime
   #:get-backend-renderer
   #:get-backend-auto-scheduler
   #:get-backend-jit-p
   #:jit-mode-p
   #:get-buffer-type
   #:get-runtime-type
   #:get-backend-kernel
   #:get-backend-configs))

(in-package :caten/codegen/byoc)
;; ~~ Kernel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass AbstractKernel ()
  ((name :initarg :name :accessor kernel-name)
   (args :initarg :args :accessor kernel-args) ;; a list of :DEFINE-GLOBAL but sorted by the order
   (blueprint :initarg :blueprint :accessor kernel-blueprint)
   (flops :initarg :flops :accessor kernel-flops)
   (output-buffers :initarg :output-buffers :accessor kernel-output-buffers)))

(defgeneric kernel-call (kernel runtime node args)
  (:documentation "Invokes the kernel, returning the elapsed time."))

(defparameter *autotune-mode-p* nil)
(defmethod caten/runtime:realize-node ((node-id (eql :KERNEL)) runtime node args)
  (when *autotune-mode-p* (return-from caten/runtime:realize-node (uiop:symbol-call :caten/codegen/polyhedral :realize-node-with-autotuning runtime node args)))
  ;; [TODO] coerce-dtyped-buffer and force scalars to be a buffer? or if there's segv we have to add them.
  (let* ((kernel (caten/air:getattr node :kernel-info))
         (prg-time (kernel-call kernel runtime node (subseq args (caten/air:getattr node :n-kernel-args)))))
    (when (= (ctx:getenv :PROFILE) 1)
      (incf caten/runtime/profile::*jit-time* prg-time)
      ;; [TODO] Bring back profile-report
      )
    t))
;; ~~ Renderer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Renderer ()
  ((graph :initarg :graph :accessor renderer-graph)
   (index-space :initarg :index-space :type list :initform nil :accessor renderer-index-space))
  (:documentation "TODO"))

(defgeneric %render-node (renderer node-dispatcher node) (:documentation ""))
(defgeneric %render-const (renderer obj) (:documentation ""))
(defgeneric %render-kernel (renderer abstract-kernel))
(defgeneric %compile-kernel (renderer items dir))
;; ~~ Scheduler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Auto-Scheduler () ((strategy :accessor Auto-Scheduler-Strategy)))

(defstruct Strategy
  (n-profile 1 :type fixnum) (per-band-optrules 1 :type fixnum)
  (ptile-max-rank 0 :type fixnum)
  (tile-search-space nil :type list)
  (ptile-search-space nil :type list)
  (vectorize-search-space nil :type list)
  (global-max) (local-max) (shared-max))

(defmacro define-auto-scheduler
    (name &key
            ;; Sampling Configuration (TODO: User-defined cost function)
            (n-profile 1) (per-band-optrules 2)
            ;; Parallelism Configuration
            (ptile-max-rank 0) ;; 0 = No Parallelism, 1 = CPU, >= 2 is GPU, NPU, etc.
            ;; Search Space Configuration
            (tile-search-space '(2 4 8 16 32 64))
            (ptile-search-space '(2 3 4 8 13 16 29)) ;; Effective for CPU and GPU, for CPU, creates a tile for tile dim instead of splitting non-coincident band. For GPU, it is equivalent to thread-size.
            (vectorize-search-space '(4))
            ;; Constraints Configuration
            (global-max) (local-max) (shared-max) ;; Configurations for GPU Coincidence
            ;; [TODO] Vectorize, Upcast, TileSize, etc
            )
  "The macro `define-auto-scheduler` will declare an optimization strategy for the Caten Auto Scheduler.

- n-profile[fixnum] the number of profiling for cost model
- n-per-band-optrules[fixnum] BEAM Search uses max_iter = 2 + {number_of_bands} * per-band-optrules
- use-tile-gpu[fixnum] Set > 1 to allow the compiler to tile bands to generate a parallelized gpu kernel. The value will be the maximum rank of tiling.
- global-max[or null fixnum] restrict the maximum size of the griddim.
- local-max[or null fixnum] restrict the maximun size of the thread.
- shared-max[or null fixnum] If specified, the search can generate `Prefetch` optimization. This parameter restricts the maximum size of the shared memory. 
- use-parallel[fixnum] Set = 1 to allow the compiler to insert @parallel annotations to generate a parallelized cpu kernel. Note that this value is not orthogonal to use-tile-gpu.
"
  (declare (type (or null fixnum) shared-max))
  `(progn
     (defclass ,name (Auto-Scheduler) nil)
     (defmethod initialize-instance :after ((auto-scheduler ,name) &key)
       (setf (slot-value auto-scheduler 'strategy)
             (make-strategy
              :n-profile ,n-profile :per-band-optrules ,per-band-optrules
              :ptile-max-rank ,ptile-max-rank
              ;; Search Space configuration
              :tile-search-space ',tile-search-space :ptile-search-space ',ptile-search-space :vectorize-search-space ',vectorize-search-space
              :global-max ,global-max :local-max ,local-max :shared-max ,shared-max)))))
;; ~~ Backend ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defgeneric get-backend-buffer (backend))
(defgeneric get-backend-runtime (backend))
(defgeneric get-backend-auto-scheduler (backend))
(defgeneric get-backend-renderer (backend))
(defgeneric get-backend-kernel (backend))
(defgeneric get-backend-jit-p (backend))

(defmacro define-backend (name buffer-class runtime-class renderer-class kernel auto-scheduler-class is-jit-p)
  "
```
(define-backend name buffer-class runtime-class renderer kernel auto-scheduler-class is-jit-p)
```
Registers a new backend.
"
  `(progn
     (defmethod get-backend-buffer ((backend (eql ,name))) ',buffer-class)
     (defmethod get-backend-runtime ((backend (eql ,name))) ',runtime-class)
     (defmethod get-backend-renderer ((backend (eql ,name))) ',renderer-class)
     (defmethod get-backend-kernel ((backend (eql ,name))) ',kernel)
     (defmethod get-backend-auto-scheduler ((backend (eql ,name))) ',auto-scheduler-class)
     (defmethod get-backend-jit-p ((backend (eql ,name))) ,is-jit-p)))

(defun jit-mode-p (&key (backend (ctx:getenv :BACKEND)))
  "Returns T if the current device uses JIT compilation."
  (get-backend-jit-p backend))

(defun get-buffer-type (&key (backend (ctx:getenv :BACKEND)))
  "Returns the buffer type for the current device."
  (get-backend-buffer backend))

(defun get-runtime-type (&key (backend (ctx:getenv :BACKEND)))
  "Returns the runtime type for the current device."
  (get-backend-runtime backend))

(defun get-backend-configs (backend &key (opts (list #'get-backend-buffer #'get-backend-runtime #'get-backend-renderer #'get-backend-kernel #'get-backend-auto-scheduler #'get-backend-jit-p)))
  (flet ((f (x) (funcall x backend)))
    (map 'list #'f opts)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
