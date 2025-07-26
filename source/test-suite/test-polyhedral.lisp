(defpackage :caten/test-suite/polyhedral
  (:use :cl :rove :caten/api :caten/air :caten/aasm :caten/lang :caten/runtime :caten/codegen/byoc
        :caten/codegen/polyhedral
        :caten/codegen/blueprint)
  (:export))

(in-package :caten/test-suite/polyhedral)

(in-caten-toplevel)

(defun psched (poly)
  (format t "~a~%" (caten/codegen/pprinter:pprint-isl-schedule (caten/codegen/polyhedral::poly-schedule poly))))

(defun getband (poly idx)
  (caten/codegen/polyhedral::schedule-node-get-band-from-relative-idx (isl::schedule-get-root (caten/codegen/polyhedral::poly-schedule poly)) idx))

(defun expr-val (expr) (caten/aasm/expr:expr-realize-as-value expr))

(defun get-depth (band) (caten/codegen/polyhedral::schedule-node-get-band-depth band))

(defmacro bp-match-p (graph pattern &aux (match-p (gensym)))
  `(let ((,match-p nil))
     (funcall
      (Simplifier
          ()
          (,pattern -> ((node graph) (setf ,match-p t) nil)))
      ,graph)
     ,match-p))

(defparameter *strategy*
  (caten/codegen/byoc::make-strategy
   :n-profile 1 :per-band-optrules 3 :ptile-max-rank 0 :tile-search-space `(2 4 6 8)
   :ptile-search-space `(2 4 8 12)
   :vectorize-search-space `(2 4 8 12)))

(trivia:defpattern Var (x dtype &key (allow-range nil) (expr t))
  `(or
    ,@(when expr
        `((<Rule> :EXPR ((Var ,x ,dtype :expr nil)))))
    (<Rule> :Load ((:Allocate () :nrank 0 :dtype ,dtype)) :value ,x)
    ,@(when allow-range `((<Rule> :RANGE (_ _) :dtype ,dtype :idx ,x)))
    ,@(when (equal x `(= 0))
        `((<Rule> :Allocate () :nrank 0 :dtype ,dtype)))))

(defmacro with-traced-polyhedral ((name1 name2 strategy) &body body)
  `(progn
     ,@body
     (defun ,name1 (&rest args)
       (ctx:with-contextvar (:BEAM 0 :BACKEND "NATIVE")
         (let ((runtime (caten (apply #',name2 args))))
           (assert (= 1 (count :KERNEL (graph-nodes (runtime-graph runtime)) :key #'node-type))
                   ()
                   "Runtime should only schedule a single kernel!")
           (let ((kernel (find :KERNEL (graph-nodes (runtime-graph runtime)) :key #'node-type)))
             (assert kernel)
             (caten/codegen/polyhedral::make-polyhedral-from-blueprint
              (kernel-blueprint (getattr kernel :kernel-info))
              :strategy ,strategy)))))))

(defmacro with-polyhedral (((bind polyhedral) &rest optimizations) ((bind1 &optional (allocs (gensym))) &body body))
  `(let ((,bind ,polyhedral))
     ,@optimizations
     (multiple-value-bind (,bind1 ,allocs) (caten/codegen/polyhedral::get-blueprint-from-polyhedral ,bind)
       (declare (ignorable ,allocs))
       ,@body)))

(with-traced-polyhedral ($gemm gemm *strategy*)
  @caten.jit () {
  (defun Gemm ((Pointer Z Type (M K)) (Pointer X Type (M N)) (Pointer Y Type (N K)))
    (for i = (Range M 1) do
         (for j = (Range K 1) do
              (with-locals ((acc 0.0))
                (for kk = (Range N 1) do
                     (setf acc (+ acc (* (aref X (+ (* N i) kk)) (aref Y (+ (* K kk) j))))))
                (setf (aref Z (+ (* K i) j)) acc)))))})

(with-traced-polyhedral ($flash_attention flash_attention *strategy*)
  @caten.jit () {
  (defun flash_attention ((Pointer Q Type (Batch Head N D)) (Pointer K Type (Batch Head N D)) (Pointer V Type (Batch Head N D))
                          (Pointer O Type (Batch Head N D))
                          (Pointer L Type (Batch Head N)) (Pointer M Type (Batch Head N)))
    (let ((scale (/ 1.0 (sqrt (scast D :float32)))))
      (for b = (Range BATCH 1) do
           (for h = (Range Head 1) do
                (for i = (Range N 1) do
                     (let ((q-base-idx (* D (+ i (* N (+ (* b head) h)))))
                           (k-base-idx (* D (* N (+ (* b head) h))))
                           (v-base-idx (* D (* N (+ (* b head) h))))
                           (o-base-idx (* D (+ i (* N (+ (* b head) h))))))
                       (with-locals ((row_m (aref M (+ i (* N (+ (* b head) h)))))
                                     (row_l (aref L (+ i (* N (+ (* b head) h))))))
                         (for j = (Range N 1) do
                              (with-locals ((dot 0.0))
                                (for dth = (Range D 1) do
                                     (setf dot (+= dot (* (aref Q (+ q-base-idx dth)) (aref K (+ k-base-idx (* D j) dth))))))
                                (let ((S (* dot scale))
                                      (new-max (max row_m S))
                                      (exp-prev (exp (- row_m new-max)))
                                      (exp-cur (exp (- S new-max)))
                                      (l-new (+ (* exp-prev row_l) exp-cur)))
                                  (for dth1 = (Range D 1) do
                                       (setf (aref O (+ o-base-idx dth1))
                                             (/ (+ (* exp-cur (aref V (+ v-base-idx (* j D) dth1))) (* exp-prev row_l (aref O (+ o-base-idx dth1))))
                                                l-new)))
                                  (setf row_m new-max ;; もしかしたらここarefかも
                                        row_l l-new))))
                         (setf
                          (aref M (+ i (* n (+ (* b head) h)))) row_m
                          (aref L (+ i (* n (+ (* b head) h)))) row_l))))))))})

(with-traced-polyhedral ($softmax softmax *strategy*)
  (defun softmax (tensor) (!softmax tensor)))

(with-traced-polyhedral ($softmax_jit softmax-jit *strategy*)
  @caten.jit () {
  (defun softmax-jit ((Pointer X Type (A B)))
    (for _gid_p0 = (Range A 1) do
         (with-locals ((val_11 0.0) (val_2 -100000.0))
           (for _gid_p1 = (Range B 1) do
                (setf val_2 (max val_2 (aref X (+ (* 512 _gid_p0) _gid_p1)))))
           (for _gid_p1_1 = (Range B 1) do
                (setf (aref X (+ (* 512 _gid_p0) _gid_p1_1)) (exp (- (aref X (+ (* 512 _gid_p0) _gid_p1_1)) val_2)))
                (setf val_11 (+ val_11 (aref X (+ (* 512 _gid_p0) _gid_p1_1)))))
           (for _gid_p1_2 = (range B 1) do
                (setf (aref X (+ (* 512 _gid_p0) _gid_p1_2)) (/ (aref X (+ (* 512 _gid_p0) _gid_p1_2)) val_11))))))})

(deftest test-polyhedral-reschedule
  (testing "Test Reschedule"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 10)) (make-tensor `(10 10)) (make-tensor `(10 10))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         ;; (psched gemm)
         )
        ((new-kernels extra-allocs) (ok (= 1 (length new-kernels)))))
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 10)) (make-tensor `(10 10)) (make-tensor `(10 10))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :serialize-sccs 1))) ;; Loop Fission
         ;; (psched gemm)
         )
        ((new-kernels extra-allocs)
          (ok (= 1 (length extra-allocs))) ;; Accumlator is mutated as allocation
          (ok (= 3 (length new-kernels)))))))

(deftest test-polyhedral-interchange
  "Interchange can change the order of coincidence bands"
  (testing "Test Interchange 2D (ij -> ij)"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 2 (get-depth (getband gemm 0)))) ;; I, J should be coincidence, they are interchangeable
         (let ((ij-band (getband gemm 0)))
           (setf gemm (apply-optimization gemm (make-instance 'Interchange :order `(0 1) :band ij-band :axis 0)))))
        ((new-kernels extra-allocs)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          (let ((gemm (car new-kernels)))
            (ok (bp-match-p gemm (:FOR ((:RANGE ((Var 10 _) (Var 1 _))) (:FOR ((:RANGE ((Var 30 _) (Var 1 _))) _))))))))))
  (testing "Test Interchange 2D (ij -> ji)"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 2 (get-depth (getband gemm 0)))) ;; I, J should be coincidence, they are interchangeable
         (let ((ij-band (getband gemm 0)))
           (setf gemm (apply-optimization gemm (make-instance 'Interchange :order `(1 0) :band ij-band :axis 0)))))
        ((new-kernels extra-allocs)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          (let ((gemm (car new-kernels)))
            (ok (bp-match-p gemm (:FOR ((:RANGE ((Var 30 _) (Var 1 _))) (:FOR ((:RANGE ((Var 10 _) (Var 1 _))) _))))))))))
  (testing "Test Interchange 3D"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :serialize-sccs 1)))
         (ok (= 3 (get-depth (getband gemm 1)))) ;; I, J should be coincidence, they are interchangeable
         (let ((ijk-band (getband gemm 1)))
           (setf gemm (apply-optimization gemm (make-instance 'Interchange :order `(2 1 0) :band ijk-band :axis 1)))))
        ((new-kernels extra-allocs)
          (ok (= 3 (length new-kernels)))
          (ok (= 1 (length extra-allocs)))
          (let ((gemm (second new-kernels)))
            (ok (bp-match-p gemm
                            (:FOR ((:RANGE ((Var 20 _) (Var 1 _)))
                                   (:FOR ((:RANGE ((Var 30 _) (Var 1 _)))
                                          (:FOR ((:RANGE ((Var 10 _) (Var 1 _))) _)))))))))))))
;; [TODO] Test Loop Tile Here
(deftest test-polyhedral-tile-gpu
  (testing "TileGPU for 2D"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 2 (get-depth (getband gemm 0))))
         (let ((ij-band (getband gemm 0)))
           (setf gemm (apply-optimization gemm (make-instance 'TileGPU :local-size 4 :band ij-band :axis 0)))))
        ((new-kernels extra-allocs)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          (let ((gemm (car new-kernels)))
            (let ((ls (caten/codegen/blueprint:blueprint-gather-grids gemm)))
              ;; (grid_size, thread_size)
              (ok (equal (print (map 'list #'expr-val (nth 0 ls))) `(3 16)))
              (ok (equal (map 'list #'expr-val (nth 1 ls)) `(8 1)))
              (ok (equal (map 'list #'expr-val (nth 2 ls)) `(1 1))))))))
  (testing "TileGPU for 3D"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :serialize-sccs 1)))
         (ok (= 3 (get-depth (getband gemm 1))))
         (let ((ijk-band (getband gemm 1)))
           ;; Memo: K is not coincident=1 btw
           (setf gemm (apply-optimization gemm (make-instance 'TileGPU :local-size 4 :band ijk-band :axis 1)))))
        ((new-kernels extra-allocs)
          (ok (= 3 (length new-kernels)))
          (ok (= 1 (length extra-allocs)))
          (let ((gemm (second new-kernels)))
            ;; [TODO] Add a "decent" match case
            (let ((ls (caten/codegen/blueprint:blueprint-gather-grids gemm)))
              ;; (grid_size, thread_size)
              (ok (equal (map 'list #'expr-val (nth 0 ls)) `(3 64)))
              (ok (equal (map 'list #'expr-val (nth 1 ls)) `(8 1)))
              (ok (equal (map 'list #'expr-val (nth 2 ls)) `(5 1))))))))
  (testing "TileGPU but local_size is smaller than loop size") ;; TODO
  (testing "TileGPU+Tile"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 2 (get-depth (getband gemm 0))))
         (let ((ij-band (getband gemm 0)))
           (setf gemm (apply-optimization gemm (make-instance 'TileGPU :local-size 4 :band ij-band :axis 0)))
           (setf gemm (apply-optimization gemm (make-instance 'Tile :size 4 :band (getband gemm 0) :axis 0)))))
        ((new-kernels extra-allocs)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          (let ((gemm (car new-kernels)))
            ;; Local Size should not be changed
            (let ((ls (caten/codegen/blueprint:blueprint-gather-grids gemm)))
              ;; (grid_size, thread_size)
              (ok (equal (map 'list #'expr-val (nth 0 ls)) `(3 16)))
              (ok (equal (map 'list #'expr-val (nth 1 ls)) `(8 1)))
              (ok (equal (map 'list #'expr-val (nth 2 ls)) `(1 1)))))))))

(deftest test-polyhedral-parallel
  (testing "Parallelize outermost loop w/ collapse(2)"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 2 (get-depth (getband gemm 0))))
         (let ((ij-band (getband gemm 0)))
           (setf gemm (apply-optimization gemm (make-instance 'Parallel :depth 2 :band ij-band :axis 1)))))
        ((new-kernels extra-allocs)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          (let ((gemm (car new-kernels)))
            (ok
             (bp-match-p
              gemm
              (:FOR
               ((:RANGE ((Var 300 _) (Var 1 _)))
                (:PROGN
                  ((:EXPR (_)) ;; CSE0
                   (:EXPR ((Var 0.0 _))) ;; Accumlation Loader
                   (:EXPR (_)) ;; CSE1
                   (:FOR ((:RANGE ((Var 20 _) (Var 1 _))) _)) ;; WMMA
                   (:Expr (_)) ;; Store Function
                   )))
               :parallel (= 1))))))))
  (testing "Parallelize+Tile"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 2 (get-depth (getband gemm 0))))
         (let ((ij-band (getband gemm 0)))
           (setf gemm (apply-optimization gemm (make-instance 'Parallel :depth 2 :band ij-band :axis 1)))
           (setf gemm (apply-optimization gemm (make-instance 'Tile :size 4 :band (getband gemm 0) :axis 1)))
           (print gemm)
           ))
        ((new-kernels extra-allocs)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          (let ((gemm (car new-kernels)))
            (print-blueprint gemm t)
            ;; [TODO] 後でテストちゃんと書く，どう検証すべきかわかんね
            (ok
             (bp-match-p
              gemm
              (:FOR
               ((:RANGE ((Var 300 _) (Var 1 _)))
                (:PROGN
                  ((:EXPR ((Var 0.0 _))) ;; Accumlation Loader
                   (:FOR ((:RANGE ((Var 20 _) (Var 1 _))) _)) ;; WMMA
                   (:Expr (_)) ;; Store Function
                   )))
               :parallel (= 1)))))))))

(deftest test-polyhedral-unroll
  ;; Two Tests:
  ;; - Outermost Unroll (i.e.: i, j)
  ;; - Innermost Unroll (i.e.: k)
  ;; and ..
  ;; - Reminder Creation
  ;; This thing should be applied into Vectorize, And finally TensorCore
  )

(deftest test-polyhedral-vectorize
  ;; Two Tests:
  ;; - Outermost Unroll (i.e.: i, j)
  ;; - Innermost Unroll (i.e.: k)
  ;; and ..
  ;; - Reminder Creation
  ;; [TODO]
  ;; - まず考える，InsertMarkが一位に定まる方法
  ;; - 
  ;; [TODO] これが終わったら，ConvNDでもVectorizeを適用することを考える
  ;; [TODO] Vectorize ==> BANDをInnermostへSinkしたい...
  ;; Workload
  ;; - DirectiveをちゃんとFORに適用させる or DirectiveMarkにSequenceを挿入したい？
  ;; - ReminderはPaddingで表現する
  (testing "Vectorize at K"
    (with-polyhedral
        ;; TODO: If the loop was smaller than width?
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 1 (get-depth (getband gemm 1))))
         (setf gemm (apply-optimization gemm (make-instance 'Vectorize :width 3 :band (getband gemm 0) :axis 0)))
         (setf gemm (apply-optimization gemm (make-instance 'Vectorize :width 3 :band (getband gemm 1) :axis 1)))
         (print gemm))
        ((new-kernels extra-allocs)
          (print-blueprint (car new-kernels) t)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))

          )))
  ;; [TODO] Softmax Vectorize
  )

(deftest test-warp/block-reduction
  (with-polyhedral ((softmax ($softmax (make-tensor `(512 512))))
                    (setf softmax (apply-optimization softmax (make-instance 'Reschedule :outer-coincidence 1)))
                    (setf softmax (apply-optimization softmax (make-instance 'SplitReduce :mode :warp :size 4 :band (getband softmax 2) :axis 2)))
                    
                    (print softmax)
                    )
      ((softmax-kernels extra-allocs)
        (assert (= 1 (length softmax-kernels)))
        (let ((sftmx (car softmax-kernels)))
          (print-blueprint sftmx t)))))
;; [TODO] Write
(deftest test-polyhedral-flash-attention
  (testing "Scheduling"
    (with-polyhedral ((attn ($flash_attention (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5)) (make-tensor `(10 8 5)))))
        ((attn-kernels extra-allocs)
          (assert (= 1 (length attn-kernels)))
          (let ((kernel (car attn-kernels)))
            (print-blueprint kernel t)))))
  (testing "Search"
    (with-polyhedral ((attn ($flash_attention (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5)) (make-tensor `(10 8 5))))
                      (setf attn (apply-optimization attn (make-instance 'Reschedule :outer-coincidence 1))) ;; Loop Fusion
                      (setf attn (apply-optimization attn (make-instance 'TileGPU :local-size 2 :band (getband attn 0) :axis 0)))
                      ;; [TODO]
                      ;; Apply:
                      ;;   Vectorize, TensorCore, SplitReduce
                      (psched attn)
                      (print attn)
                      nil)
        ((attn-kernels extra-allocs)
          (assert (= 1 (length attn-kernels)))
          (let ((kernel (car attn-kernels)))
            (print-blueprint kernel t))))))
;; [TODO]
(deftest test-polyhedral-cse
  "val_9[i] = ...
   val_10 = BIND(SETF_VAL_9, value=val_9)[i]"
  (testing "Softmax(Tensor)"
  (with-polyhedral ((softmax ($softmax (make-tensor `(512 512))))
                    )
      ((softmax-kernels extra-allocs)
        (assert (= 1 (length softmax-kernels)))
        (let ((sftmx (car softmax-kernels)))
          (print-blueprint sftmx t)))))
  (testing "Softmax(JIT)"
  (with-polyhedral ((softmax ($softmax_jit (make-tensor `(512 512))))
                    )
      ((softmax-kernels extra-allocs)
        (assert (= 1 (length softmax-kernels)))
        (let ((sftmx (car softmax-kernels)))
          (print-blueprint sftmx t))))))
;; [TODO] Polyhedral TODO
;; - [ ] Parallel+TILE is breaked
;; - [x] Finalize CSE
;;  - [x] val_9，というか(setf X)にBIND生成を矯正させる
;;  - [x] Fix FlashAttention CSE
;;  - [ ] Fix null stashed problem (?) CSE+Tile?
;; - [ ] 次にVectorize/TensorCore, これは今日やる
;; - [ ] TileGPUを実装し直す。バンドの深さに制約をかけない (全部band-coalsceしてから置き換えるだけ)
;; - [ ] 最後にGROUP/GROUPTOP
;; - [ ] 全てにAccuarcy Test実装する
;; - [ ] TODO: CIでFlashAttentionを回す(CI BEAM)
;; - [ ] Loop FissionしてからのTileGPU動いたっけ？ ==> OK
;; - [ ] Fix RANDN
;;  - [ ] Matmul, Randnした後のTensorViewを正しく修正する。
;; - [ ] Things to fix: (1.) FlashAttention Schedule is too slow, (2.) Softmax is not working?
;; - [ ] After that, proceed to Vectorize/TensorCore/GROUP (1day)
;; - [ ] Setup CI, Benchmark, Poster
;; - [ ] SearchSpace, Rescheduleさえ先頭ならあとはどうでもいい
;; - [ ] *search-space* Tree no Parallel Assertion is not valid? and interchange isn't working?
;; - Vectorizeをどうやって実装するべきか，InnerLoopのみを切り出すというのはできない？
;; - Interchange/TileGPUが本当にすべての空間をCoverしてるか考える。一回のInterchangeで複数のToplevel Sequenceを最適化する
  ;; [TODO]
;; Needed for finding an optimal kernel FINISH by (07/27)
;; - [x] Reschedule
;; - [x] Interchange
;; - [x] Tile
;; - [x] TileGPU
;; - [x] Parallel
;; - [ ] Vectorize   (--> :SEPARATEでInnerBandのみで実装できるか。)
;;   - [ ] Upcastなので，float4, GPUのvar.x var.y(SIMD), CPUのSIMDへ応用する必要がある。
;; - [ ] TensorCore  (--> Vectorize2D for prerequisite)
;; - [ ] SplitReduce (--> Need Some Improvements on RenderOps)
;; - [ ] 必要か微妙: Collapse
;; - [ ] 全部探索空間に入れてBEAM
;; - [ ] TileしてParentにMarkしてはいけない。

;; - [ ] Unroll      (--> :SEPARATEでInnerBandのみの実装できるか。)
;;  - [ ] これは自動スケジューリングで実施するのではなく，自動でやる。Nothing to tuning!

;; - [ ] BEAM Searchの初期でTILE_SIZE=256が選択されないという問題がある。
;; - [ ] TODO: Insert Markする時は先にTileしてから！
;; - [ ] We Want To Have:
;;   - [ ] TileGPU as ISL Tile (Insert IF!!) TileGPU+Unrollができるようにして，スレッド数を削減したい。
;;     - [ ] Isolate Option? 
;;   - [ ] Unroll
;; - [ ] Collapse
;; - [ ] SearchSpaceについて, 256 -> 128 -> 32みたいに綺麗にMappingができるだろうか？
;; - [ ] Clang Parallel. (Error Handlingがあるので，OMPない環境でも実装できる)
;; - [ ] OpenCL Backend

;; [TODO]
;; Also add tests for
;; - Softmax
;; - FlashAttention
;; - randn failing case
;; - BandGPUはReminderをIfで生成したい。
;; - Loop Size=1 --> ここにMarkしたら壊れない？
;; (run-suite *package*)
