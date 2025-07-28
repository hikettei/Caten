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

(with-traced-polyhedral ($sin sin-graph *strategy*)
  (defun sin-graph (tensor) (!sin tensor)))

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
         (setf gemm (apply-optimization gemm (make-instance 'Vectorize :width 4 :band (getband gemm 0) :axis 0)))
         (setf gemm (apply-optimization gemm (make-instance 'Vectorize :width 4 :band (getband gemm 1) :axis 1)))
         (print gemm))
        ((new-kernels extra-allocs)
          (print-blueprint (car new-kernels) t)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          ))))
;; [TODO]
;; 戻ったら
;; Softmax, FlashAttentionでVECTORIZE
;; CI書く準備して実験回す！
(deftest test-vectorize-softmax
  (testing "Softmax Vectorize"
    (with-polyhedral ((softmax ($softmax (make-tensor `(512 512))))
                      (setf softmax (apply-optimization softmax (make-instance 'Reschedule :outer-coincidence 1)))
                      (setf softmax (apply-optimization softmax (make-instance 'Vectorize :width 4 :band (getband softmax 0) :axis 0)))
                      (print softmax)
                      )
        ((softmax-kernels extra-allocs)
        (assert (= 1 (length softmax-kernels)))
        (let ((sftmx (car softmax-kernels)))
          (print-blueprint sftmx t)))))
  ;; [TODO] Softmax Vectorize
  )

(deftest test-vectorize-flash-attention
  (testing "Scheduling"
    (with-polyhedral ((attn ($flash_attention (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5)) (make-tensor `(10 8 5))))
                      (setf attn (apply-optimization attn (make-instance 'Reschedule :outer-coincidence 1)))
                      ;;(setf attn (apply-optimization attn (make-instance 'Vectorize :width 4 :band (getband attn 0) :axis 0)))
                      (print attn))
        ((attn-kernels extra-allocs)
          (assert (= 1 (length attn-kernels)))
          (let ((kernel (car attn-kernels)))
            (print-blueprint kernel t)
            )))))

(deftest test-polyhedral-vectorize-1
  (testing "Vectorize at K"
    (with-polyhedral
        ;; TODO: If the loop was smaller than width?
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :serialize-sccs 1)))
         (setf gemm (apply-optimization gemm (make-instance 'Vectorize :width 4 :band (getband gemm 1) :axis 0)))
;;         (setf gemm (apply-optimization gemm (make-instance 'Vectorize :width 4 :band (getband gemm 1) :axis 1)))
         (print gemm))
        ((new-kernels extra-allocs)
          (print-blueprint (nth 1 new-kernels) t)
          ))))

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

(deftest test-polyhedral-vec
  (with-polyhedral ((sin ($sin (make-tensor `(10 10))))
                    (setf sin (apply-optimization sin (make-instance 'Vectorize :width 4 :band (getband sin 0) :axis 0))))
      ((sin-kernels extra-allocs)
        (print-blueprint (car sin-kernels) t))))
;; [TODO] Write
(deftest test-polyhedral-flash-attention
  (testing "Scheduling"
    (with-polyhedral ((attn ($flash_attention (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5 10)) (make-tensor `(10 8 5)) (make-tensor `(10 8 5)))))
        ((attn-kernels extra-allocs)
          (assert (= 1 (length attn-kernels)))
          (let ((kernel (car attn-kernels)))
            (print-blueprint kernel t)
            ))))
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
;; - 1. Interchangeをすべて探索OKにする (OK)
;; - 2. ParallelをCPUで実行可能にする   (x) CLANG Segv?
;; - 3. TileGPUをすべて探索OKにする
;;     - 次元数で分割をやめてすべてCoalesceにする No Need To Split Band (?)
;;     - Metal TileGPU Segv
;; - 4. FUZZ_BEAM Context, 探索中常にMSE Errorを確認
;; - Loop Fissionが不安定？
;; - ある程度Beneficialな最適化というのは決まってる(特に並列化)
;; - STEP=1では，CPUならParallelは各カーネルの最もOutermostなBand, GPUならサイズ固定せずにParallelを並列化する。w/o Execution
;; 目下のやること
;; - Run BEAM=1 JIT_DEBUG=3 NATIVE=1 qlot exec ros run --load ./examples/flash-attention.lisp in CI
;; - [ ] Softmax -> Vectorize, DEFINE-LOCALの場所
;; - [ ] Run BEAM Search in CI, Fix Segv
;; - [ ] Gemm >= 1000 GFLOPs
;; - [ ] TileGPUを実装し直す。バンドの深さに制約をかけない (全部band-coalsceしてから置き換えるだけ)
;; - [ ] Finish Poster
;; [TODO] Polyhedral TODO
;; - [ ] Parallel+TILE is breaked
;; - [x] Finalize CSE
;;  - [x] val_9，というか(setf X)にBIND生成を矯正させる
;;  - [x] Fix FlashAttention CSE
;;  - [ ] Fix null stashed problem (?) CSE+Tile?
;; - [ ] 次にVectorize/TensorCore, これは今日やる
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

;;  - [ ] !sum :axis t looks slow ... they canot use tilegpu? 
;;; Paper: https://arxiv.org/pdf/2410.03210
;; [TODO] Implement Polyhedral-Guided, Customizable AutoScheduler Engine
;; https://chatgpt.com/c/6870e59d-2970-8005-abda-1b62c5808111?model=o3-pro
;; o3-pro proposed the following:
;; 1. reduce dependencies are not handled
;; 2. scalar is not a scalar
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

;; One week is enough to run FlashAttention for me ...
;; [Workload]
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

;; [SearchSpace] => {Action} => [SearchSpace]

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
;; Schedule + RaW Accessing, 同じフォーマットである必要？
;; - なぜループが0から始まらないのか？
