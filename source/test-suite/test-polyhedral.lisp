(defpackage :caten/test-suite/polyhedral
  (:use :cl :rove :caten/api :caten/air :caten/aasm :caten/lang :caten/runtime :caten/codegen/byoc
        :caten/codegen/polyhedral
        :caten/codegen/blueprint)
  (:export))

(in-package :caten/test-suite/polyhedral)

(in-caten-toplevel)

(defun psched (poly)
  (format t "~a~%" (caten/common.pprinter:pprint-isl-schedule (caten/codegen/polyhedral::poly-schedule poly))))

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
       (ctx:with-contextvar (:BEAM 0)
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
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :serialize-sccs 1)))
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
;; [TODO] TileTest

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
              (ok (equal (map 'list #'expr-val (nth 2 ls)) `(1 1))))
            ;; [TODO] Add a "decent" match case
            (ok
             (bp-match-p
              gemm
              (:PROGN
                ((:EXPR (a))
                 (:EXPR (b))
                 (:EXPR (c))
                 (:EXPR (d))
                 (:IF ((:EXPR (e)) body))))))))))
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
              (ok (equal (map 'list #'expr-val (nth 2 ls)) `(5 1))))
            (ok
             (bp-match-p
              gemm
              (:PROGN
                ((:EXPR (a))
                 (:EXPR (b))
                 (:EXPR (c))
                 (:EXPR (d))
                 (:EXPR (e))
                 (:EXPR (f))
                 (:IF ((:EXPR (l)) body))))))))))
  (testing "TileGPU but the loop size is smaller than local-size") ;; TODO
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
              (ok (equal (map 'list #'expr-val (nth 2 ls)) `(1 1))))
            (ok
             (bp-match-p
              gemm
              (:PROGN
                ((:EXPR (a))
                 (:EXPR (b))
                 (:EXPR (c))
                 (:EXPR (d))
                 (:IF ((:EXPR (e)) body)))))))))))

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
                  ((:EXPR ((Var 0.0 _))) ;; Accumlation Loader
                   (:FOR ((:RANGE ((Var 20 _) (Var 1 _))) _)) ;; WMMA
                   (:Expr (_)) ;; Store Function
                   )))
               :parallel (= 1))))))))
  ;; TODO test w/ tile, vectorize
  )
;; - Vectorizeをどうやって実装するべきか，InnerLoopのみを切り出すというのはできない？

;; Needed for finding an optimal kernel FINISH by (07/27)
;; - [x] Reschedule
;; - [x] Interchange
;; - [x] Tile
;; - [x] TileGPU
;; - [ ] Parallel
;; - [ ] Vectorize
;; - [ ] Unroll
;; - [ ] TensorCore
;; - [ ] SplitReduce
;; - [ ] 必要か微妙: Collapse
;; - [ ] 全部探索空間に入れてBEAM

;; - [ ] TODO: Insert Markする時は先にTileしてから！
;; - [ ] We Want To Have:
;;   - [ ] TileGPU as ISL Tile (Insert IF!!) TileGPU+Unrollができるようにして，スレッド数を削減したい。
;;     - [ ] Isolate Option? 
;;   - [ ] Unroll
;; - [ ] Collapse
;; - [ ] SearchSpaceについて, 256 -> 128 -> 32みたいに綺麗にMappingができるだろうか？

;; [TODO]
;; Also add tests for
;; - Softmax
;; - FlashAttention
;; - randn failing case
;; - BandGPUはReminderをIfで生成したい。
;; - Loop Size=1 --> ここにMarkしたら壊れない？
(run-suite *package*)
