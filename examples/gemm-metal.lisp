;;;; Note: This file is not intended to be loaded directly from Lisp,
;;;; but to be executed expression by expression.
;;;; If you are using Emacs/Lem, you can simply press `C-C C-c` while your cursor is hovering over an expression

;;;; Matmul Benchmark (vs hand optimized metal kernel)

;; Of course you may need Apple Silicon chips to run this code.
(unless (find-package :caten)
  (ql:quickload :caten))

(defpackage :matmul-metal
  (:use :cl :caten)
  (:import-from :caten/codegen/expr :ExprGrid :expr-const))

(in-package :matmul-metal)

(defmacro with-masked (&body body) `(float-features:with-float-traps-masked t ,@body))

(setf
 (ctx:getenv :BACKEND) "METAL"
 (ctx:getenv :JIT_DEBUG) 4
 (ctx:getenv :OPTIMIZE) 2
 (ctx:getenv :PROFILE) 1) ;; This will display GFLOPS on your repl.

(defparameter *N* 2048)
(defparameter *LID* 2)

(caten/byoc/metal::ensure-foreign-library)

(defparameter *device* (with-masked (caten/byoc/metal::MTLCreateSystemDefaultDevice)))
(defparameter *mtl-queue* (with-masked (caten/byoc/metal::msg *device* "newCommandQueueWithMaxCommandBufferCount:" :pointer :int 1024)))

;; (Note) Metal kernel was taken from https://github.com/tinygrad/tinygrad/blob/master/extra/gemm/metal_matmul.py
(defparameter *lib* (caten/byoc/metal:mtl-compile-source (format nil "
#include <metal_stdlib>
#include <metal_simdgroup_matrix>
using namespace metal;

#define N ~a
#define LID ~a

kernel void mtlsgemm(device float *a, device const float *data1, device const float *data2, uint3 gid [[threadgroup_position_in_grid]], uint3 lid [[thread_position_in_threadgroup]]) {{
  a += gid.x * 32 * N + (gid.y * LID + lid.y) * 32;
  data1 += gid.x * 32 * N;
  data2 += (gid.y * LID + lid.y) * 32;

  simdgroup_float8x8 acc[4][4];
  for (uint i = 0; i < 4; i++) {{
    for (uint j = 0; j < 4; j++) {{
      acc[i][j] = simdgroup_float8x8(0);
    }}
  }}

  simdgroup_float8x8 A[4];
  simdgroup_float8x8 B[4];
  for (uint k = 0; k < N; k+=8) {{
    threadgroup_barrier(mem_flags::mem_threadgroup);
    simdgroup_load(A[0], data1+k+0*N, N, ulong2(0, 0));
    simdgroup_load(A[1], data1+k+8*N, N, ulong2(0, 0));
    simdgroup_load(A[2], data1+k+16*N, N, ulong2(0, 0));
    simdgroup_load(A[3], data1+k+24*N, N, ulong2(0, 0));
    simdgroup_load(B[0], data2+0+k*N, N, ulong2(0, 0));
    simdgroup_load(B[1], data2+8+k*N, N, ulong2(0, 0));
    simdgroup_load(B[2], data2+16+k*N, N, ulong2(0, 0));
    simdgroup_load(B[3], data2+24+k*N, N, ulong2(0, 0));

    simdgroup_multiply_accumulate(acc[0][0], A[0], B[0], acc[0][0]);
    simdgroup_multiply_accumulate(acc[0][1], A[1], B[0], acc[0][1]);
    simdgroup_multiply_accumulate(acc[0][2], A[2], B[0], acc[0][2]);
    simdgroup_multiply_accumulate(acc[0][3], A[3], B[0], acc[0][3]);
    simdgroup_multiply_accumulate(acc[1][0], A[0], B[1], acc[1][0]);
    simdgroup_multiply_accumulate(acc[1][1], A[1], B[1], acc[1][1]);
    simdgroup_multiply_accumulate(acc[1][2], A[2], B[1], acc[1][2]);
    simdgroup_multiply_accumulate(acc[1][3], A[3], B[1], acc[1][3]);
    simdgroup_multiply_accumulate(acc[2][0], A[0], B[2], acc[2][0]);
    simdgroup_multiply_accumulate(acc[2][1], A[1], B[2], acc[2][1]);
    simdgroup_multiply_accumulate(acc[2][2], A[2], B[2], acc[2][2]);
    simdgroup_multiply_accumulate(acc[2][3], A[3], B[2], acc[2][3]);
    simdgroup_multiply_accumulate(acc[3][0], A[0], B[3], acc[3][0]);
    simdgroup_multiply_accumulate(acc[3][1], A[1], B[3], acc[3][1]);
    simdgroup_multiply_accumulate(acc[3][2], A[2], B[3], acc[3][2]);
    simdgroup_multiply_accumulate(acc[3][3], A[3], B[3], acc[3][3]);
  }}
  simdgroup_store(acc[0][0], a+0+0*N, N, ulong2(0, 0));
  simdgroup_store(acc[1][0], a+8+0*N, N, ulong2(0, 0));
  simdgroup_store(acc[2][0], a+16+0*N, N, ulong2(0, 0));
  simdgroup_store(acc[3][0], a+24+0*N, N, ulong2(0, 0));
  simdgroup_store(acc[0][1], a+0+8*N, N, ulong2(0, 0));
  simdgroup_store(acc[1][1], a+8+8*N, N, ulong2(0, 0));
  simdgroup_store(acc[2][1], a+16+8*N, N, ulong2(0, 0));
  simdgroup_store(acc[3][1], a+24+8*N, N, ulong2(0, 0));
  simdgroup_store(acc[0][2], a+0+16*N, N, ulong2(0, 0));
  simdgroup_store(acc[1][2], a+8+16*N, N, ulong2(0, 0));
  simdgroup_store(acc[2][2], a+16+16*N, N, ulong2(0, 0));
  simdgroup_store(acc[3][2], a+24+16*N, N, ulong2(0, 0));
  simdgroup_store(acc[0][3], a+0+24*N, N, ulong2(0, 0));
  simdgroup_store(acc[1][3], a+8+24*N, N, ulong2(0, 0));
  simdgroup_store(acc[2][3], a+16+24*N, N, ulong2(0, 0));
  simdgroup_store(acc[3][3], a+24+24*N, N, ulong2(0, 0));
}}" *N* *LID*)))

(defparameter *matmul*
  (make-instance
   'caten/byoc/metal:Metal-Program
   :lib *lib* :name "mtlsgemm" :device *device* :mtl-queue *mtl-queue*
   :argtypes `(:float32 :float32 :float32)
   :grid-size (list
               (make-instance 'ExprGrid :rank 0 :global-size (expr-const (/ *N* 32) :int64) :local-size (expr-const 32 :int64))
               (make-instance 'ExprGrid :rank 1 :global-size (expr-const (/ *N* (* 8 4 *LID*)) :int64) :local-size (expr-const *LID* :int64))
               (make-instance 'ExprGrid :rank 2 :global-size (expr-const 1 :int64) :local-size (expr-const 1 :int64)))))

(defparameter *node* (caten/air:make-node :JIT :JIT_KERNEL nil (list 'a 'b 'c)))

(defparameter *a* (proceed (!rand `(,*N* ,*N*))))
(defparameter *b* (proceed (!rand `(,*N* ,*N*))))
(defparameter *c* (linspace (list *N* *N*) 0.0 0.0))

(defun gflops (sec) (/ (/ (* 2 *N* *N* *N*) sec) 1e9))
(defun invoke-matmul-kernel (a b c)
  (gflops (caten/byoc/metal::invoke *matmul* *node* (tensor-buffer a) (tensor-buffer b) (tensor-buffer c))))

(defparameter *matmul-caten* (caten (!matmul *a* *b*)))

(defun invoke-matmul-caten (a b c)
  ;; manually invoking the caten jit_kernel
  (let* ((runtime *matmul-caten*)
         (fxn (car (last (caten/air:graph-nodes (caten/runtime:runtime-graph runtime)) 2))))
    (assert (eql (caten/air:node-type fxn) :JIT_KERNEL))
    (gflops (caten/codegen/runner::runtime-invoke-jit-kernel runtime (caten/air:getattr fxn :kernel-info) fxn (list (tensor-buffer a) (tensor-buffer b) (tensor-buffer c))))))

;; Usage:
(print (invoke-matmul-kernel *a* *b* *c*))
(print (invoke-matmul-caten *a* *b* *c*))

(defun benchmark (&aux (n-sample 30))
  (let* ((mps (loop for i upfrom 0 below n-sample collect (invoke-matmul-kernel *a* *b* *c*)))
         (ctn (loop for i upfrom 0 below n-sample collect (invoke-matmul-caten *a* *b* *c*)))
         (avg-gflops-mps (/ (reduce #'+ mps) n-sample))
         (avg-gflops-ctn (/ (reduce #'+ ctn) n-sample)))
    (format t "[BENCHMARK N=~a]~%" *N*)
    (format t "Metal Kernel ~A GFLOPS~%" avg-gflops-mps)
    (format t "Caten JIT Kernel ~A GFLOPS~%" avg-gflops-ctn)
    (format t "Metal/Caten=~A%~%" (/ avg-gflops-ctn avg-gflops-mps))))

;; 4000 GFLOPS w/ Metal is doable, we are still working on improving the auto scheduler to reach that level...
(benchmark)
