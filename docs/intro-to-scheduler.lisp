(defpackage :intro-to-tutorial
  (:use :cl :caten/api :caten/nn :caten/runtime :caten/codegen/scheduler :caten/air :caten/codegen/packing :caten/codegen/renderer
   :caten/codegen/auto-scheduler :caten/codegen/expr)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler))
(in-package :intro-to-tutorial)
;; Work in Progress!
;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
        (caten/codegen/scop:scop node)
        (return-from get-gemm-schedule node)))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
