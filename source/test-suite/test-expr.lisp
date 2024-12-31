(defpackage :caten/test-suite/expr
  (:documentation "A test suite for caten/codegen/expr and gflops computation.")
  (:use :cl :rove :caten/codegen/expr :caten/runtime :caten/apis :caten/air :caten/codegen/blueprint :caten/codegen/jit))
(in-package :caten/test-suite/expr)
;; 1. expr-realize
;; 2. gflops test from the matmul schedule
(deftest test-expr-simplification
  ;; [TODO] More Simplification case if exists
  (ok (= 1 (buffer-value (expr-realize (expr-add (expr-const 1 :int64) (expr-const 0 :int64)))))))

(deftest test-gemm-gflops
  (testing "Static Matmul FLOP Computation ..."
    (when (caten/codegen/backend:jit-mode-p)
      (let* ((gemm (caten (!matmul (make-tensor `(512 256)) (make-tensor `(256 1024)))))
             (ops (* 2 512 256 1024))
             (kernel (find :JIT_KERNEL (graph-nodes (runtime-graph gemm)) :key #'node-type)))
        (assert kernel () "Gemm is not found?")
        (let* ((flops (compiled-kernel-flops (getattr kernel :kernel-info)))
               (ops-computed (compute-gflops flops 1.0 nil)))
          (ok (gflops-measurer-succeed-p flops))
          (ok (= ops-computed (/ ops 1e9)))))))
  (testing "Dynamic Matmul FLOP Computation ..."
    (when (caten/codegen/backend:jit-mode-p)
      (let* ((gemm (caten (!matmul (make-tensor `(A B)) (make-tensor `(B C)))))
             (ops (* 2 512 256 1024))
             (kernel (find :JIT_KERNEL (graph-nodes (runtime-graph gemm)) :key #'node-type)))
        (assert kernel () "Gemm is not found?")
        (let* ((flops (compiled-kernel-flops (getattr kernel :kernel-info)))
               (ops-computed (compute-gflops flops 1.0 `(A 512 B 256 C 1024))))
          (ok (gflops-measurer-succeed-p flops))
          (ok (= ops-computed (/ ops 1e9))))))))
