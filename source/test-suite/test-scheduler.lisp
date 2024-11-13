(in-package :caten/test-suite)
;; Tests caten/codegen/scheduler.lisp
(defun schedule-with-vars (&rest tensors)
  (ctx:with-contextvar (:JIT 0)
    (let ((avm (apply #'caten tensors)))
      (caten/codegen/shape-inference:run-type-infer avm)
      (caten/codegen/rewriting-rules:apply-rewriting-rules avm)
      (graph-schedule (avm-graph avm)))))

(defun gather-kernels (schedule-graph)
  (loop for node in (graph-nodes schedule-graph)
        if (getattr node :jitable)
          collect node))

(defun check-kernel (schedule-graph n-count)
  (ok (= n-count (length (gather-kernels schedule-graph))) (format nil "Scheduled ~a, expected ~a" (length (gather-kernels schedule-graph)) n-count)))
;; Adding more failing case!, the more the better
;; Can't we add more general tests for it?
;; [GOAL] Eliminate bugs from the scheduler
;; Failing ?
(deftest test-double-reduction-separated
  (testing "Reduction with different axes are not fused into a single kernel"
    (let ((schedule (schedule-with-vars (!sum (!sum (make-tensor `(10 10)) :axis 0 :keepdims t) :axis 1 :keepdims t))))
      (check-kernel schedule 2))))

(deftest test-chunk-schedule
  (let ((schedule (schedule-with-vars (apply #'!matmul (multiple-value-list (!chunk (make-tensor `(2 64 64)) 2))))))
    (check-kernel schedule 1)
    (let ((items (getattr (car (gather-kernels schedule)) :items)))
      (print items))))

(deftest test-view-merge-failing-case
  (let ((schedule (schedule-with-vars (caten (!gelu (!matmul (make-tensor `(1 64 64)) (!t (make-tensor `(1 64 64)))))))))
    (check-kernel schedule 1)))
