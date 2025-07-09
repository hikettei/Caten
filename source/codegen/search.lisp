(defpackage :caten/codegen/search
  (:use :cl :caten/aasm :caten/air :caten/codegen/byoc)
  (:export
   #:get-optimized-ast #:search-optimized-ast))

(in-package :caten/codegen/search)
;; Paper: https://arxiv.org/pdf/2410.03210
;; [TODO] Implement Polyhedral-Guided, Customizable AutoScheduler Engine

;; - コストモデル, 測定方法はどうでもいい。
;; ASTに対する
;;   - 探索方法(DFS)
;;   - How to transform the loop?
;; We already have a naive and fused FlashAttention Kernel.
;; We need:
;; - Loop Tiling               (Unroll, Vectorize, )
;; - Evaluate the cacheability (The number of alloc reuse)
;;  - 早い指標でTop Kを決めて, 最後にまとめてCompileしてTop1を決める，はできるかもしれない
;; これかが決定すれば自動スケジューラーが完成する
;; - Load from VRAM -> SRAM -> Register. Polyhedralでどう表現しますか
;; - optimal_kernel = apply(base_kernel, [] in SUBSET)
;;   - What is SUBSET?
;; - Ops.TRANSFER的なのを実装する (n-layered memory caching arch)
;;   - [ ] SIMD/Warp
;;   - [ ] 

;; Optimization is applied into:
;; - 最適なループ形状 + :AREF={CACHE, NOOPT}の空間を探索？
;; - :FOR Level (Everything is ND TILING)
;;   - Loop Tiling
;;   - Loop Unrolling (i.e.: VECTORIZE, PARALLELIZE)
;; - :AREF Level
;;  - (ast-apply-cache的なsomethingが必要) VRAM -> DRAM -> SRAM
;;  -
"
for i=0..10
  for j=0..10
    float acc = 0.0
    for k=0..10
      acc += x[i, j] * y[j, k]
    z[i, k] = acc
=> SMEM =>

"
;; The ft of Tensor Compiler is POLYHEDRAL Compiler.
;; - Believe Polyhedral Compiler, implement something like polyhedral compiler.
;; e.g.: Caten can create both of:
;;   - LU Decomposing
;;   - 

;; TODO
;; - 1. Introduce Ops.FUNCTION
;;   - 2. Hand-writtern Kernelを記述するMacro, 構文を実装する
;; - 2. Thinking the minimal Softmax Transformation
;; - 3. 

(defun get-optimized-ast ()
  
  )

(defun serach-optimized-ast (blueprint)

  )
