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

(defun get-softmax-schedule ()
  (get-schedule-from-op (!softmax (make-tensor `(128 128)))))

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

(deftest test-packing-tileband-combine
  (let ((raw (get-gemm-schedule 'a 'b 'c)))
    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
      (opt (make-instance 'Parallel) 0)
      (opt (make-instance 'TileBand :amount 32) 0)
      (opt (make-instance 'TileBand :amount 32) 2)
      (opt (make-instance 'TileBand :amount 32) 4)
      (opt (make-instance 'Packing :amount 4) 1)
      (opt (make-instance 'Packing :amount 4) 3)
      (opt (make-instance 'Packing :amount 4) 5))
    (assert-schedule raw "// @DIRECTIVE(TYPE=PARALLEL,AMOUNT=0,VISIBLE=T)
for (int c0 = 0; c0 < a; c0 += 32) {
  // @DIRECTIVE(TYPE=PACKED_OUTER,AMOUNT=4,VISIBLE=T)
  for (int c1 = 0; c1 <= min(31, a - c0 - 1); c1 += 4) {
    // @DIRECTIVE(TYPE=PACKED_INNER,AMOUNT=4,VISIBLE=NIL)
    for (int c2 = 0; c2 <= min(3, a - c0 - c1 - 1); c2 += 1)
      for (int c3 = 0; c3 < c; c3 += 32) {
        // @DIRECTIVE(TYPE=PACKED_OUTER,AMOUNT=4,VISIBLE=T)
        for (int c4 = 0; c4 <= min(31, c - c3 - 1); c4 += 4) {
          // @DIRECTIVE(TYPE=PACKED_INNER,AMOUNT=4,VISIBLE=NIL)
          for (int c5 = 0; c5 <= min(3, c - c3 - c4 - 1); c5 += 1) {
            TASK(c0 + c1 + c2, c3 + c4 + c5);
            for (int c6 = 0; c6 < b; c6 += 32) {
              // @DIRECTIVE(TYPE=PACKED_OUTER,AMOUNT=4,VISIBLE=T)
              for (int c7 = 0; c7 <= min(31, b - c6 - 1); c7 += 4) {
                // @DIRECTIVE(TYPE=PACKED_INNER,AMOUNT=4,VISIBLE=NIL)
                for (int c8 = 0; c8 <= min(3, b - c6 - c7 - 1); c8 += 1)
                  TASK(c0 + c1 + c2, c3 + c4 + c5, c6 + c7 + c8);
              }
            }
            TASK(c0 + c1 + c2, c3 + c4 + c5);
          }
        }
      }
  }
}
")))

(deftest test-elementwise-vectorize-parallelize
  ;; Interchange PARALLEL -> Does not effect on the scheduled code?
  (ok
   (string=
    (let ((raw (get-schedule-from-op (!sin (make-tensor `(100 100))))))
      (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
        (opt (make-instance 'Parallel) 0)
        (opt (make-instance 'Packing :amount 4) 0))
      (get-schedule raw))
    (let ((raw (get-schedule-from-op (!sin (make-tensor `(100 100))))))
      (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
        (opt (make-instance 'Packing :amount 4) 0)
        (opt (make-instance 'Parallel) 0))
      (get-schedule raw)))))

(deftest test-global-mutation
  (testing "If LocalSize=1, @DIRECTIVE(LOCAL) should not appeared here."
    (let ((raw (get-gemm-schedule 'm 'n 'k)))
      (with-manual-scheduler (raw Mock-GPU-AutoScheduler)
        (opt (make-instance 'Global :amount 1) 0))
      (assert-schedule raw "// @DIRECTIVE(TYPE=GLOBAL,AMOUNT=1,VISIBLE=NIL)
for (int c0 = 0; c0 < m; c0 += 1) {
  for (int c2 = 0; c2 < k; c2 += 1) {
    TASK(c0, c2);
    for (int c3 = 0; c3 < n; c3 += 1)
      TASK(c0, c2, c3);
    TASK(c0, c2);
  }
}
")))
  (testing "LocalSize>1"
    (let ((raw (get-gemm-schedule 'm 'n 'k)))
      (with-manual-scheduler (raw Mock-GPU-AutoScheduler)
        ;; Gemm is parallelized at the two outermost dimensions
        (opt (make-instance 'Global :amount 16) 0)
        (opt (make-instance 'Global :amount 16) 0))
      (assert-schedule raw "// @DIRECTIVE(TYPE=GLOBAL,AMOUNT=16,VISIBLE=NIL)
for (int c0 = 0; c0 < m; c0 += 16) {
  // @DIRECTIVE(TYPE=LOCAL,AMOUNT=16,VISIBLE=NIL)
  for (int c1 = 0; c1 <= min(15, m - c0 - 1); c1 += 1) {
    // @DIRECTIVE(TYPE=GLOBAL,AMOUNT=16,VISIBLE=NIL)
    for (int c2 = 0; c2 < k; c2 += 16) {
      // @DIRECTIVE(TYPE=LOCAL,AMOUNT=16,VISIBLE=NIL)
      for (int c3 = 0; c3 <= min(15, k - c2 - 1); c3 += 1) {
        TASK(c0 + c1, c2 + c3);
        for (int c4 = 0; c4 < n; c4 += 1)
          TASK(c0 + c1, c2 + c3, c4);
        TASK(c0 + c1, c2 + c3);
      }
    }
  }
}
"))))

;; [TODO]
;; (deftest test-tile2d
;;  (let ((raw (get-gemm-schedule 'm 'n 'k)))
;;    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
;;      (opt (make-instance 'TileBand :amount 16) 0)
;;      (opt (make-instance 'TileBand :amount 16) 2))
;;    (get-schedule raw)))

(deftest test-tile-and-coalesce-cpu
  (let ((raw (get-gemm-schedule 'm 'n 'k)))
    (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
      (opt (make-instance 'Parallel) 0)
      (opt (make-instance 'TileBand :amount 16) 0)
      (opt (make-instance 'TileBand :amount 16) 2)
      )
    (print (get-schedule raw))
    nil))

(deftest test-tile-and-coalesce-gpu
  (let ((raw (get-gemm-schedule 'm 'n 'k)))
    (with-manual-scheduler (raw Mock-GPU-AutoScheduler)
      (opt (make-instance 'Global :amount 16) 0)
      (opt (make-instance 'Global :amount 16) 0)
      )
    (print (get-schedule raw))
    nil))
;; Test Softmax Inner band is not coincident (signal ...)
;; Test Coalesce
;; Test Tile2D
;; Test UNROLL+PACK
;; Test Interchange (Mark are moved)
;; TODO: Markが挿入されたBandはInterchangeできないようにした方が良くない？
;; TODO: BEAM=1 is not required???

;; ~~ Hand Optimized Kernel Generation(GEMM) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; 方針が変わった
;; - ISL上でOptimalなScheduleを得る目的は変わらない
;; - OPTIMIZE=1: ILPを解く, 全部のBANDにTILE+PACKING, OUTERMOST LOOP PARALLELISM, COLLAPSE, COALEASCINGを適用
;;   - Single Kernelにならない場合棄却する
;;   - AnsorでいうTemplate生成
;; - OPTIMIZE=2
;;   - LOCAL_SIZE, TILE_SIZEをGFLOPSベースで最適化する
;;   - ISL Coalescingで条件をSimplifyする

;; ISL Workload
;; - Softmax/LayerNorm/Embedding/Symbolicを常に1 KernelへFusion -> WIN
;; - Mapping Tile Dims -> UNROLL/PACKING/TILE/COALESCE etc

;; To generate an optimized schedule for the cpu gemm kernel, we have to implement the following scheduling commands:
;; [TODO] Vectorizeを適用すると(getattr node :global-unrolled-space)を固定する。で，val_2_x_x_xは:global-unroll-spaceベースで決定する。
;; - これがEXPRごとで共有できないとvectorize失敗になる
;; - 1. Add: :VECTORIZED (or implement: expr-vectorize)
;;  - Specify the idx.
;;  - LOAD, STORE SIMD Rewriting rule...
;;  - Add the concept of pack/unpack
;;  - Softmax: Innermost dims are not coincident...
;;  - Fix: Unroll/Packing new indexed var names are 100% working??????
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
;; - Unrollの代わりにUpcastを追加する
;; - 
;; - Upcsat/PACK/UNPACK/COMPUTEを追加する
;; - Upcast/COMPUTEのRendererを追加する
;; - Memory Planner?
;; - Mixed Precision?
;; [TODO] Workload
;; - TODO: Delete schedule BFS in optimize-bands!
;; - 1. ISL上で以下の操作を実現する:
;;  - 1 band 1 mark?
;;  - TODO: Size=1のBandの処理をテストに書く
;;  - MEMORY COALEASING (TILE2D, bnut threads are merged)
;;    - Tile2Dを実装する！
;;  - Shared Memory Utilization (How to insert the barrier?)
;;  - Implement GLOBAL/LOCAL as tile
;;  - TILE
;;  - PACK (No conflict!)
;; - Interchange: Markも一緒に移動する
;; - kernel-optでテストを書く
;; - 2. Vectorizeを定義する
;;  - PACK/UNPACK
;;  - 27 Opsに対してSIMD Mutationを実装する
;;  - Dtypeが異なる時どうするか考える
;; - Polyhedral Transformationをまず完成させる
;;  - 1. UNROLL/PACKINGを綺麗にする
;;  - 2. Tilingと干渉しない
;;  - 3. Tiling/UNROLL/PACKINGの，val_x_y_zのIndexingをちゃんとやる
;;  - 4. 
;; - Vectorize = PACKED_INNER次元の範囲でSliceすること
;; - PACKED_INNER次元は全てVectorizeされると仮定する。(無理なら等価な普通の演算に書き換えられるように)
;; - ReminderとPACKED?
(deftest hand-optimized-cpu-gemm-test
  (with-expr-cache ()
    (let ((raw (get-gemm-schedule 'a 'b 'c)))
      (schedule-item-maximize-band-depth raw)
      (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
        ;; Scheduling Priority:
        ;; Interchange(Memory Layout) -> Packing -> Tile -> Interchange (2D Tile) -> Parallelize
        ;; Apply packing first to use TensorCore MULADD
        (opt (make-instance 'Parallel) 0)
        (opt (make-instance 'TileBand :amount 32) 0)
        (opt (make-instance 'Unroll :amount 16) 1))
      (print-schedule raw)
      (print-bp raw)
      )))

;; Note: OPTIMIZE=1 Behaviour
;; (with-manual-scheduler (x scheduler)
;;    (dotimes (i (n-bands))
;;      (opt (make-instance 'Packing :amount 4) i)))
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
      ;;(opt (make-instance 'Parallel) 0)
      ;(opt (make-instance 'Packing :amount 4) 0)
      ;(opt (make-instance 'Packing :amount 4) 1)
      )
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
     ; (opt (make-instance 'Packing :amount 4) 6)
     ; (opt (make-instance 'Packing :amount 4) 7)
      
      )
    (print-bp raw)))

(deftest hand-optimized-cpu-embedding-test
  ;; Symbolic kernel needs expr-cache
  (caten/codegen/expr-cache:with-expr-cache ()
    (let ((raw (get-embedding-schedule 128 128)))
      (with-manual-scheduler (raw Mock-CPU-AutoScheduler)
        ;; (opt (make-instance 'Packing :amount 4) 6)
        ;; (opt (make-instance 'Packing :amount 4) 7)
        
        )
      (print-bp raw))))
