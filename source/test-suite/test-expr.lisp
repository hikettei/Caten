(defpackage :caten/test-suite/expr
  (:documentation "A test suite for caten/codegen/expr and gflops computation.")
  (:use :cl :rove :caten/codegen/expr :caten/runtime :caten/api :caten/air :caten/codegen/blueprint :caten/codegen/jit))
(in-package :caten/test-suite/expr)

(deftest test-expr-simplification
  ;; [TODO] More Simplification case if exists
  (ok (= 1 (buffer-value (expr-realize (expr-add (expr-const 1 :int64) (expr-const 0 :int64)))))))

(defun test-ceil (a)
  (= (ceiling (/ a 3.0)) (buffer-value (expr-realize (expr-ceiling (expr-div (expr-const a :float32) (expr-const 3 :float32)) :int32)))))

(deftest test-integer-arithmetic
  (ok (every #'test-ceil (alexandria:iota 50 :start 2))))

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

(defun rexpr (expr)
  (let ((renderer (make-instance 'caten/codegen/renderer:default-renderer
                                 :graph (expr-graph expr))))
    (caten/codegen/renderer:render-node renderer (car (node-writes (expr-out expr))))))

(deftest test-expr-simplify
  (testing  "(-a)+(a+c) ==> c"
    (let ((op (expr-add (expr-neg (expr-const 'a :int32)) (expr-add (expr-const 'a :int32) (expr-const 'c :int32)))))
      (ok (= 2 (length (graph-nodes (expr-graph op)))))
      (ok (equalp "c" (rexpr op)))))
  (testing  "(-a)+(a+c+z) ==> c+z"
    (let ((op (expr-add (expr-neg (expr-const 'a :int32)) (expr-add (expr-add (expr-const 'a :int32) (expr-const 'c :int32)) (expr-const 1 :int32)))))
      (ok (= 5 (length (graph-nodes (expr-graph op)))))
      (ok (equalp "(c+1)" (rexpr op))))))
