(defpackage :caten/codegen/search
  (:use :cl :caten/aasm :caten/air :caten/codegen/byoc)
  (:export
   #:realize-node-with-autotuning))

(in-package :caten/codegen/search)
;; Paper: https://arxiv.org/pdf/2410.03210
;; [TODO] Implement Polyhedral-Guided, Customizable AutoScheduler Engine
;; https://chatgpt.com/c/6870e59d-2970-8005-abda-1b62c5808111?model=o3-pro
;; o3-pro proposed the following:
"
| 名前                             | 意味・対象                          | 主な効果 (GPU 観点)                                     |
| ------------------------------ | ------------------------------ | ------------------------------------------------- |
| **reorder\_ikj**               | ループ順序交換                        | メモリアクセス連続化，依存を壊さずに L/S 帯域↑。                       |
| **tile\_block\_ij{M×N}**       | CTA (block) タイル化               | グローバル→SMEM 転送の削減。CTA 数で並列度を制御。                    |
| **tile\_warp\_ij{m×n}**        | Warp タイル化                      | Tensor Core／simdgroup 演算単位と一致させ，LD/ST を coalesce。 |
| **tile\_lane\_vec{w}**         | Lane 内 SIMD (vec2/4/8)         | ld.v4/st.v4 等のベクトル命令で帯域効率↑。                       |
| **split\_k\_reduce{p}**        | k 軸並列 + 反復還元                   | CTA/SM 並列度拡大。`p` は分割数。                            |
| **cache\_read\_shared\_{A/B}** | 行列タイルの SMEM 読み込み               | L2⇔SMEM 帯域と再利用回数の最適化。                             |
| **cache\_write\_shared**       | 出力タイルを SMEM 経由で書き戻し            | 書き込みコアレッシング・衝突回避。                                 |
| **sw\_pipeline\_stage{n}**     | 二重/三重バッファリング                   | データ転送と演算をオーバラップし隠蔽。                               |
| **tensorcore\_mma**            | 専用 FMA (16×16×16 等)            | FLOPs/clk を劇的に向上。Metal は `simdgroup_mad`.         |
| **vector\_fma**                | 汎用 SIMD FMA                    | Tensor Core が無い GPU で使用。                          |
| **unroll\_k{u}**               | 内部ループ展開                        | ループ制御除去と ILP 向上。                                  |
| **async\_copy**                | 非同期 cp.async / simdgroup\_copy | 転送レイテンシ隠蔽と帯域最大化。                                  |
| **rfactor\_axis{k}**           | 還元因子分割                         | prefix‑sum, attention‐score reduce 等で並列度↑。        |
| **fuse\_{i,j}**                | ループ／演算子融合                      | 中間テンソル排除，DRAM 往復削減。                               |
| **recompute\_small**           | 小演算の再計算でメモリ節約                  | O(N²)→O(N) メモリモデルで有効。                             |
| **auto\_vector\_width**        | 動的 SIMD 幅決定                    | GPU 世代差・データ型差を隠蔽。                                 |


def softmax(input[1280, 1280], output[1280, 1280]):
  allocate temp[1280, 1280]
  for n in parallel(1280):
    val_11 = 0.0
    val_2 = -Inf
    for m in range(1280):
      val_2 = max(val_2, input[1280*n+m]
    for m in range(1280):
      temp[_gid0*n+m] = exp2((input[1280*n+m]-val_2)*1.442695)
      val_11 = val_11 + temp[_gid0*n+m]
    for m in range(1280):
      output[n*1280+m] = val_9[n*1280+m]/val_11
//=============================================================================
// softmax_stored_locally_multi_dim の “warpOnly” 完全インライン化版
//=============================================================================
def softmax_stored_locally_multi_dim(input, output, m, n):
    num_packs = ceil((n/4) / blockDim.x)
    parallel for block_x in range(gridDim.x):
      parallel for thread_y in range(blockDim.y):
        parallel for tid in range(blockDim.x):
          for row in range(block_x*blockDim.y + thread_y, m, gridDim.x*blockDim.y):
            row_offset = row * (n>>2)
            row_x = input  + row_offset
            row_y = output + row_offset

            //── ローカル読み込み＋最大値
            buf = allocate float4[num_packs]
            local_max = -Inf
            for pack_id in range(num_packs):
              col_base = pack_id*blockDim.x + tid
              vectorize(4):
                if col_base + lane < n/4:
                  h4 = row_x[col_base]
                  buf[pack_id][lane] = half2float(h4.component[lane])
                else:
                  buf[pack_id][lane] = -Inf
              local_max = max(local_max,
                              max(max(buf[pack_id].x, buf[pack_id].y),
                                  max(buf[pack_id].z, buf[pack_id].w)))

            //── ワープ内最大値還元
            for mask in [blockDim.x/2, blockDim.x/4, …, 1]:
              local_max = max(local_max,
                              __shfl_xor_sync(0xffffffff,
                                              local_max,
                                              mask,
                                              32))

            //── exp＋ワープ内和還元
            local_sum = 0.0
            for i in range(num_packs):
              vectorize(4):
                buf[i][lane] = exp(buf[i][lane] - local_max)
                local_sum += buf[i][lane]
            for mask in [blockDim.x/2, blockDim.x/4, …, 1]:
              local_sum += __shfl_xor_sync(0xffffffff,
                                           local_sum,
                                           mask,
                                           32)

            //── 書き戻し
            for i in range(num_packs):
              col_base = i*blockDim.x + tid
              vectorize(4):
                if col_base + lane < n/4:
                  row_y[col_base].component[lane] = buf[i][lane] / local_sum

def matmul(X[128, 128], Y[128, 128], OUT[128, 128]):
  for i in range(128):
    for j in range(128):
      acc = 0.0f
      for k in range(128):
          acc = acc + X[128*i+k] * Y[128*k + j]
      OUT[128*i+j] = acc

// CTA タイル = 64×64，Warp タイル = 16×16，lane = vec4
for B_i in blockIdx.y parallel tile_block_ij:64  // 0..1
  for B_j in blockIdx.x parallel tile_block_ij:64
    // 共有メモリキャッシュ
    shared Xs[64][64] @cache_read_shared_X
    shared Ys[64][64] @cache_read_shared_Y

    // Warp グリッド (=4×4 Warps per CTA)
    for W_i in threadIdx.y parallel tile_warp_ij:16  // 0..3
      for W_j in threadIdx.x/32 parallel tile_warp_ij:16  // 0..3
        // レジスタ蓄積
        reg_C[16][16] = 0

        //--------- k 軸分割（split_k_reduce=2）-----------
        for k_outer in range(0, 128, 64):
          // 二重バッファで Xs, Ys を先読み (sw_pipeline_stage2 + async_copy)
          prefetch X, Y tiles → Xs, Ys

          //---------------- 主演算 ------------------------
          for k_inner in unroll_k8 range(0, 64, 8):
            // lane = vec4 で 4 要素ロード
            vectorize(4):
              reg_A = ld4(Xs, W_i*16 + lane, k_inner + s)    // s=0..7
              reg_B = ld4(Ys, k_inner + s, W_j*16 + lane)
            // Tensor Core (16×16×8 FMA) または simdgroup_mad
            tensorcore_mma(reg_C, reg_A, reg_B)
          //------------------------------------------------

        //--------- 出力 (vec4 store) ---------------------
        vectorize(4):
          st4(OUT,
              (B_i*64 + W_i*16 + lane),
              (B_j*64 + W_j*16 + vec4-id),
              reg_C[lane][:])


"

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

(defun serach-optimzied-ast (kernel)

  
  )

;; (defkernel xxx (...) (:policy `(d . 128))) <- specify OptimizationPolicy

;; Policy:
;; - GraphRewriteでRuntimeGraphが常にStaticであることを保証して，進めていく
;; - いずれにせよグラフの形状がわからないと進められない。
;; ScheduleCommands:
;; 1. new_graph, hi, ho = apply_tile(graph, ...)
;;
;; Workload
;; LoopInterchange is actually what we need:
;; - Implement Polyhedral
;; - Allow Loop Fission!
(defun realize-node-with-autotuning (runtime node args &aux (searched))
  (labels ((evaluate-kernel (kernel &key (n 10) &aux (total 0.0))
             (dotimes (i n)
               (incf total (caten/codegen/byoc:kernel-call kernel runtime node args)))
             total)
           (register-kernel-as-candidate (kernel)
             (push (cons (evaluate-kernel kernel) kernel) searched)))
    (register-kernel-as-candidate (caten/air:getattr node :kernel-info))
    (print "AUTOTUNING")
    

    (caten/codegen/polyhedral:make-polyhedral-from-blueprint
     (kernel-blueprint (caten/air:getattr node :kernel-info)))
    (dotimes (i 100))
    (print searched)
    ;; [TODO] Apply BEAM Search
    (setf (caten/air:getattr node :kernel-info) (cdr (sort searched #'< :key #'car)))
    ;; [TODO] Copy the initial results? to avoid overflow? or for sparse optimizations?
    (apply #'values (subseq args 0 (length (caten/air:node-writes node))))))
