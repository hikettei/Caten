(defpackage :caten/test-suite/test-kernel-opt
  (:documentation "Tests all optimization rule defined in the ./source/codegen/search/engine.lisp")
  (:use :cl :rove :caten/api :caten/nn :caten/runtime :caten/codegen/scheduler :caten/air :caten/codegen/packing :caten/codegen/renderer
   :caten/codegen/auto-scheduler :caten/codegen/expr)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler))
(in-package :caten/test-suite/test-kernel-opt)
;; AutoScheduler workload:
;; 1. Assume there's two type of operations:
;;   - MulAcc: e.g.: Gemm, Conv2D
;;   - Normalization: Softmax, LayerNorm, etc
;; 2. Make caten scheduler enough to generate cuBLAS level speed by manually writing schedules.
;; 3. Let BEAM Search to automatically generate schedules for the workload.
(defun get-raw-schedule (tensor)
  (ctx:with-contextvar (:BACKEND "LISP")
    (let ((runtime (caten tensor)))
      (caten/codegen/shape-inference:run-type-infer runtime)
      (caten/codegen/rewriting-rules:apply-rewriting-rules runtime)
      (values (graph-schedule (runtime-graph runtime)) runtime))))

(defun jit-kernel-p (node) (getattr node :jitable))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; There are two type of kernels we assume:
;; - 1. Reduction: Gemm, Conv2D, Embedding, etc.
;; - 2. Normalization: Softmax, LayerNorm, etc.
;; the function get-x-schedule will return the schedule item of each kernel with polyhedral ir.
(defun get-gemm-schedule (m n k)
  (multiple-value-bind (schedule runtime)
      (get-raw-schedule  (!matmul (make-tensor `(,m ,n)) (make-tensor `(,n ,k))))
    (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling gemm = 1 kernel.")
    (dolist (node (graph-nodes schedule))
      (when (jit-kernel-p node)
        (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
        (caten/codegen/scop:scop node)
        (return-from get-gemm-schedule node)))))

(defun get-softmax-schedule ()
  (multiple-value-bind (schedule runtime)
      (get-raw-schedule (!softmax (make-tensor `(128 128))))
    (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling softmax = 1 kernel.")
    (dolist (node (graph-nodes schedule))
      (when (jit-kernel-p node)
        (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
        (caten/codegen/scop:scop node)
        (return-from get-softmax-schedule node)))))

(defun get-layernorm-schedule ()
  (with-inference-mode ()
    (multiple-value-bind (schedule runtime)
        (get-raw-schedule (forward (LayerNorm `(128)) (make-tensor `(128 128))))
      (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling softmax = 1 kernel.")
      (dolist (node (graph-nodes schedule))
        (when (jit-kernel-p node)
          (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
          (caten/codegen/scop:scop node)
          (return-from get-layernorm-schedule node))))))

(defun get-convnd-relu-schedule ()
  (with-inference-mode ()
    (multiple-value-bind (schedule runtime)
        (get-raw-schedule (!relu (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25)))))
      (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling softmax = 1 kernel.")
      (dolist (node (graph-nodes schedule))
        (when (jit-kernel-p node)
          (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
          (caten/codegen/scop:scop node)
          (return-from get-convnd-relu-schedule node))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; TODO:
;; - Tile Optimization Test (band node relocation works? 2d tiling works?)

;; ~~ Hand Optimized Kernel Generation(GEMM) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun print-bp (si) ;; utils for debugging in repl
  (caten/codegen/blueprint:print-blueprint (getattr si :blueprint) t))

(defun print-schedule (si)
  (print (getattr si :polyhedral)))
;; 明日のTODO: multiple node-writes
(define-auto-scheduler
    (Mock-CPU-AutoScheduler ()) :n-global-loop 1
    :vectorizes
    (list
     (Vectorize :gemm4x4 `(4 4)  :applicable-p #'expr-node-wmma-p         :rewriter #'(lambda (x) (expr-rewrite-as-tensorcore x :gemm4x4)))
     (Vectorize :simd-load `(4)  :applicable-p #'expr-node-simd-load-p    :rewriter #'(lambda (x) (expr-rewrite-as-simd-load x :load)))
     (Vectorize :simd-store `(4) :applicable-p #'expr-node-simd-store-p   :rewriter #'(lambda (x) (expr-rewrite-as-simd-store x :store)))
     (Vectorize :simd-upcast `(4) :applicable-p #'expr-node-simd-upcast-p :rewriter #'(lambda (x) (expr-rewrite-as-simd-upcast x :upcast))))
    ;; :cost-functions (:sum (:vectorized-area :profile :coincidence)) (TODO)
    )

(define-auto-scheduler (Mock-GPU-AutoScheduler ()) :n-global-loop 3)
;; To generate an optimized schedule for the cpu gemm kernel, we have to implement the following scheduling commands:
;; [TODO] Vectorizeを適用すると(getattr node :global-unrolled-space)を固定する。で，val_2_x_x_xは:global-unroll-spaceベースで決定する。
;; - これがEXPRごとで共有できないとvectorize失敗になる
;; - 1. Add: :VECTORIZED (or implement: expr-vectorize)
;;  - LOAD, STORE SIMD Rewriting rule...
;;  - Add the concept of pack/unpack
;; - 2. (!!!!) Add: SIMD (float4 mutation, __mm256, etc...)
;;   - ast-parser.lispで，例えばArgsがPackされてなかったらPackするなど。。。
;; - 3. expr-node-wmma-p => Use Simplififer!
;; - 4. TransposedGemm is properly vectorized?
;; - 5. Fix: PACK+TILE
;; [TODO] Interchange: only counts visible bands
;; [TODO] Late unroll won't update the index? -> fix it first
;; [TODO] how to judge the elements are contiguous?
;; [TODO] When TensorCore Optimization is applicable?
;; [TODO] Maybe no need to schedule Unroll in AutoScheduler, Packing is enough.
;; [TODO} Lowerしてから，SIMDに合わせてBlueprintをいじった方が確実
;; - PARALLEL (OpenMP)
;; - Coleasing (Fuse PARALLEL Loop w/ tiled bands)
;; - Loop Tiling (2D)
;; - Loop Unrolling
;; - Implement TensorCore(gemm8x8)
;;   - Feed hand-written kernel in the CLANG backend
;; 90% performance of OpenBLAS in the hand written kernel is enough great!
;; reference: https://salykova.github.io/matmul-cpu
;; Workload:
;; - Unrollの代わりにUpcsatを追加する
;; - Upcsat/PACK/UNPACK/COMPUTEを追加する
;; - Upcast/COMPUTEのRendererを追加する
;; - Memory Planner?
;; - Mixed Precision?
(deftest hand-optimized-cpu-gemm-test
  (let ((raw (get-gemm-schedule 'a 'b 'c)))
    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
      ;; Scheduling Priority:
      ;; Interchange(Memory Layout) -> Packing -> Tile -> Interchange (2D Tile) -> Parallelize
      ;; Apply packing first to use TensorCore MULADD
      (opt (make-instance 'Parallel) 0)
;      (opt (make-instance 'Packing :amount 4) 0) ;; TODO: Ignore AMT=1 Pack/Unroll/Tile
      (opt (make-instance 'Packing :amount 4) 1)
      (opt (make-instance 'Packing :amount 4) 2)
      ;; (opt (make-instance 'Unroll :amount 1) 0)
      ;; (opt (make-instance 'Unroll :amount 4) 1)
      ;; (opt (make-instance 'Unroll :amount 4) 2)
      ;; 2D Tiling (16, 16)
      ;; (opt (make-instance 'TileBand :amount 16) 0)
      ;; (opt (make-instance 'TileBand :amount 16) 1)
      ;; (opt (make-instance 'TileBand :amount 16) 2)
      ;; (opt (make-instance 'Interchange :amount 1) 1)
      ;; Parallelize at innermost dimension
      ;; (opt (make-instance 'Parallel) 5)
      )
    (print-schedule raw)
    (print-bp raw)))
;; To generate an optimized schedule for the gpu gemm kernel, we have to implement the following scheduling commands:
;; - GLOBAL/LOCAL (Remove extra if statements when the reminder part is zero.)
;; - GROUP
;; - Warp Reduction Transformation
;; - Tile
;; - TensorCore
;; - :BARIIER (=> __syncthreads())
;; Goal: 80~90% performance of cuBLAS
(deftest hand-optimized-gpu-gemm-test
  (let ((raw (get-gemm-schedule 512 512 512)))
    (with-manual-scheduler (raw Mock-GPU-AutoScheduler)
      (opt (make-instance 'Global :amount 1) 0)
      (opt (make-instance 'Global :amount 1) 0)
      (opt (make-instance 'Packing :amount 4) 0)
      (opt (make-instance 'Packing :amount 4) 1)
      (opt (make-instance 'Packing :amount 4) 2)
      )
    (print-bp raw)))
;; ~~ Hand Optimized Kernel Generation(Softmax) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftest hand-optimized-cpu-softmax-test
  (let ((raw (get-softmax-schedule)))
    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
      (opt (make-instance 'Parallel) 0)
      (opt (make-instance 'Packing :amount 4) 0)
      (opt (make-instance 'Packing :amount 4) 1))
    (print-bp raw)))
;; ~~ Hand Optimized Kernel Generation(LayerNorm) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftest hand-optimized-cpu-layernorm-test
  (let ((raw (get-layernorm-schedule)))
    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
      (opt (make-instance 'Packing :amount 4) 0)
      (opt (make-instance 'Packing :amount 4) 1)
      (opt (make-instance 'Packing :amount 4) 2)
      (opt (make-instance 'Packing :amount 4) 3)
      )
    (print-bp raw)))
;; ~~ Hand Optimized Kernel Generation(Conv2d) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftest hand-optimized-cpu-conv2d-relu-test
  (let ((raw (get-convnd-relu-schedule)))
    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
      (opt (make-instance 'Packing :amount 4) 6)
      (opt (make-instance 'Packing :amount 4) 7)
      
      )
    (print-bp raw)))
