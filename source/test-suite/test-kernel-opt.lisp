(defpackage :caten/test-suite/test-kernel-opt
  (:documentation "Tests all optimization rule defined in the ./source/codegen/search/engine.lisp")
  (:use :cl :rove :caten/api :caten/nn :caten/runtime :caten/codegen/scheduler :caten/air :caten/codegen/packing :caten/codegen/renderer
   :caten/codegen/auto-scheduler :caten/codegen/expr :cl-ppcre)
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
(defun get-schedule-from-op (op)
  (multiple-value-bind (schedule runtime)
      (get-raw-schedule op)
    (assert (= 1 (count-if #'jit-kernel-p (graph-nodes schedule))) () "Cannot run the op without scheduling op = 1 kernel.")
    (dolist (node (graph-nodes schedule))
      (when (jit-kernel-p node)
        (caten/codegen/blueprint:lower-schedule-item node (runtime-graph runtime) schedule)
        (caten/codegen/scop:scop node)
        (return-from get-schedule-from-op node)))))

(defun get-gemm-schedule (m n k)
  (get-schedule-from-op (!matmul (make-tensor `(,m ,n)) (make-tensor `(,n ,k)))))

(defun get-softmax-schedule (a b)
  (get-schedule-from-op (!softmax (make-tensor `(,a ,b)))))

(defun get-layernorm-schedule ()
  (with-inference-mode ()
    (get-schedule-from-op (forward (LayerNorm `(128)) (make-tensor `(128 128))))))

(defun get-convnd-relu-schedule ()
  (with-inference-mode ()
    (get-schedule-from-op (!relu (forward (ConvND 3 6 `(5 5)) (make-tensor `(10 3 25 25)))))))

(defun get-embedding-schedule (b s)
  (with-inference-mode ()
    (get-schedule-from-op (call (Embedding 128 256) (make-tensor `(,b ,s))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun print-bp (si) ;; utils for debugging in repl
  (caten/codegen/blueprint:print-blueprint (getattr si :blueprint) t))

(defun print-schedule (si)
  (print (getattr si :polyhedral)))

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
;; ~~ Test Optimizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun string-hide-nid (string)
  "NID12345(...) -> TASK(...)"
  (declare (type string string))
  (regex-replace-all "NID[0-9]+" string "TASK"))

(defun get-schedule (item)
  (declare (type node item))
  (assert (jit-kernel-p item))
  (string-hide-nid
   (caten/codegen/polyhedral:render-schedule-node
    (caten/codegen/polyhedral:poly-schedule
     (getattr item :polyhedral)))))

(defmacro assert-schedule (item expected)
  `(let ((r (get-schedule ,item)))
     (ok (string= r ,expected) (format nil "[Scheduled]~%~a~%[Expected]~%~a" r ,expected))))
