(defpackage :caten/test-suite/test-kernel-opt
  (:documentation "Tests all optimization rule defined in the ./source/codegen/search/engine.lisp")
  (:use :cl :rove :caten/api :caten/nn :caten/runtime :caten/codegen/scheduler :caten/air
   :caten/codegen/auto-scheduler)
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
(defun get-gemm-schedule ()
  (multiple-value-bind (schedule runtime)
      (get-raw-schedule (!matmul (make-tensor `(512 512)) (make-tensor `(512 512))))
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

(define-auto-scheduler (Mock-CPU-AutoScheduler ()) :n-global-loop 1)
(define-auto-scheduler (Mock-GPU-AutoScheduler ()) :n-global-loop 3)
;; To generate an optimized schedule for the cpu gemm kernel, we have to implement the following scheduling commands:
;; - PARALLEL (OpenMP)
;; - Coleasing (Fuse PARALLEL Loop w/ tiled bands)
;; - Loop Tiling (2D)
;; - Loop Unrolling
;; - Vectorize or TensorCore (8x8 gemm)
;; 90% performance of OpenBLAS in the hand written kernel is enough great!.
(deftest hand-optimized-cpu-gemm-test
  (let ((raw (get-gemm-schedule)))
    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
      (opt (make-instance 'Parallel) 0)
      )
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
  (let ((raw (get-gemm-schedule)))
    (with-manual-scheduler (raw Mock-GPU-AutoScheduler)
      (opt (make-instance 'Global :amount 1) 0)
      )
    
    (print-bp raw)))
;; ~~ Hand Optimized Kernel Generation(Softmax) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~ Hand Optimized Kernel Generation(LayerNorm) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; ~~ Hand Optimized Kernel Generation(Conv2d) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
