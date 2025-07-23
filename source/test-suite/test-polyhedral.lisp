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

(defparameter *strategy*
  (caten/codegen/byoc::make-strategy
   :n-profile 1 :per-band-optrules 3 :ptile-max-rank 0 :tile-search-space `(2 4 6 8)
   :ptile-search-space `(2 4 8 12)
   :vectorize-search-space `(2 4 8 12)))

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
  (testing "Test Interchange"
    (with-polyhedral
        ((gemm ($gemm (make-tensor `(10 10)) (make-tensor `(10 10)) (make-tensor `(10 10))))
         (setf gemm (apply-optimization gemm (make-instance 'Reschedule :maximize-coincidence 1)))
         (print (getband gemm 0))
         ;; (psched gemm)
         )
        ((new-kernels extra-allocs)
;;          (print new-kernels)
          ))))

;; [TODO]
;; Also add tests for
;; - Softmax
;; - FlashAttention
;; - randn failing case
(run-suite *package*)
