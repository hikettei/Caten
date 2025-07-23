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
    ;; [TODO] Marked Band Shuffle?
    ))

(deftest test-polyhedral-tile-gpu
  (testing "Test Interchange 2D (ij -> ij)"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 30)) (make-tensor `(10 20)) (make-tensor `(20 30))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (ok (= 2 (get-depth (getband gemm 0))))
         (let ((ij-band (getband gemm 0)))
           ;; [TODO] [Important] ループのサイズで振る舞いを変える！！
           (setf gemm (apply-optimization gemm (make-instance 'TileGPU :local-size 4 :band ij-band :axis 0)))))
        ((new-kernels extra-allocs)
          (ok (= 1 (length new-kernels)))
          (ok (= 0 (length extra-allocs)))
          (let ((gemm (car new-kernels)))
            (print-blueprint gemm t)
            )))))

;; Needed for finding an optimal kernel FINISH by (07/27)
;; - [x] Reschedule
;; - [x] Interchange
;; - [ ] Tile
;; - [ ] TileGPU
;; - [ ] Vectorize
;; - [ ] TensorCore
;; - [ ] Collapse
;; - [ ] SplitReduce

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
