(defpackage :caten/test-suite/test-kernel-opt
  (:documentation "Tests all optimization rule defined in the ./source/codegen/search/engine.lisp")
  (:use :cl :rove :caten/api :caten/nn :caten/runtime :caten/codegen/scheduler :caten/air))
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

(defun get-gemm-schedule ()
  (multiple-value-bind (schedule runtime)
      (get-raw-schedule (!matmul (make-tensor `(512 512)) (make-tensor `(512 512))))
    (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling gemm = 1 kernel.")
    (dolist (node (graph-nodes schedule))
      (when (jit-kernel-p node)
        (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
        (caten/codegen/scop:scop node) ;; Lower into the Polyhedral IR
        (return-from get-gemm-schedule node)))))

(defun get-softmax-schedule ()
  (multiple-value-bind (schedule runtime)
      (get-raw-schedule (!softmax (make-tensor `(128 128))))
    (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling softmax = 1 kernel.")
    (dolist (node (graph-nodes schedule))
      (when (jit-kernel-p node)
        (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
        (caten/codegen/scop:scop node) ;; Lower into the Polyhedral IR
        (return-from get-softmax-schedule node)))))

(defun get-layernorm-schedule ()
  (with-inference-mode ()
    (multiple-value-bind (schedule runtime)
        (get-raw-schedule (forward (LayerNorm `(128)) (make-tensor `(128 128))))
      (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling softmax = 1 kernel.")
      (dolist (node (graph-nodes schedule))
        (when (jit-kernel-p node)
          (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
          (caten/codegen/scop:scop node) ;; Lower into the Polyhedral IR
          (return-from get-layernorm-schedule node))))))

(defun get-convnd-relu-schedule ()
  (with-inference-mode ()
    (multiple-value-bind (schedule runtime)
        (get-raw-schedule (!relu (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25)))))
      (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the test without scheduling softmax = 1 kernel.")
      (dolist (node (graph-nodes schedule))
        (when (jit-kernel-p node)
          (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
          (caten/codegen/scop:scop node) ;; Lower into the Polyhedral IR
          (return-from get-convnd-relu-schedule node))))))
