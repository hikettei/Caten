(in-package :caten/test-suite)
;; Tests caten/codegen/scheduler.lisp
(defun schedule-with-vars (&rest tensors)
  (ctx:with-contextvar (:JIT 0)
    (let ((avm (apply #'caten tensors)))
      (caten/codegen/shape-inference:run-type-infer avm)
      (caten/codegen/rewriting-rules:apply-rewriting-rules avm)
      (values (graph-schedule (avm-graph avm)) avm))))

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
;; ~~ Testing reduction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftest test-double-reduction-separated
  (testing "Reduction with different axes are not fused into a single kernel"
    (let ((schedule (schedule-with-vars (!sum (!sum (make-tensor `(10 10)) :axis 0 :keepdims t) :axis 1 :keepdims t))))
      (check-kernel schedule 2))))

(deftest test-double-reduction-separated-mean
  (testing "Reduction with different axes are not fused into a single kernel"
    (let ((schedule (schedule-with-vars (!sum (!sum (make-tensor `(10 10)) :axis 0 :keepdims t) :axis 1 :keepdims t))))
      (check-kernel schedule 2))))

(deftest test-no-extra-loop-after-out-complex
  (multiple-value-bind (schedule avm)
      (schedule-with-vars (!matmul (make-tensor `(64 64)) (!gelu (!matmul (make-tensor `(64 64)) (make-tensor `(64 64))))))
    (check-kernel schedule 2)
    (caten/codegen/expr-cache:with-expr-cache ()
      (dolist (item (gather-kernels schedule))
        (let ((bp (caten/codegen/blueprint:lower-schedule-item item (avm-graph avm) schedule)))
          (ok (= 3 (count :FOR bp :key #'node-type)) (format nil "Expected 3 loops, got ~a" (count :FOR bp :key #'node-type))))))))

(deftest test-no-extra-loop-matmul-ln-matmul
  (multiple-value-bind (schedule avm)
      (schedule-with-vars (!matmul (make-tensor `(64 64)) (!layer-norm (!matmul (make-tensor `(64 64)) (make-tensor `(64 64))) `(64 64))))
    (check-kernel schedule 4)
    (caten/codegen/expr-cache:with-expr-cache ()
      (loop for item in (gather-kernels schedule)
            for count in `(3 2 2 3) do
              (let ((bp (caten/codegen/blueprint:lower-schedule-item item (avm-graph avm) schedule)))
                (ok (= count (count :FOR bp :key #'node-type)) (format nil "Expected ~a loops, got ~a" count (count :FOR bp :key #'node-type))))))))

(deftest test-serialize-reduction-loop
  (testing "Serialized Reductions should belong to the same loop. (not creating a new inner loop!)"
    (testing "Add(Matmul, Matmul)"
      (multiple-value-bind (schedule avm)
          (schedule-with-vars (!add (!matmul (make-tensor `(64 64)) (make-tensor `(64 64)))
                                    (!matmul (make-tensor `(64 64)) (make-tensor `(64 64)))))
        (check-kernel schedule 1)
        (caten/codegen/expr-cache:with-expr-cache ()
          (loop for item in (gather-kernels schedule)
                for count in `(3) do
                  (let ((bp (caten/codegen/blueprint:lower-schedule-item item (avm-graph avm) schedule)))
                    (ok (= count (count :FOR bp :key #'node-type)) (format nil "Expected ~a loops, got ~a" count (count :FOR bp :key #'node-type))))))))
    (testing "Add(Embedding, Embedding)"
      (multiple-value-bind (schedule avm)
          (schedule-with-vars (!add (forward (Embedding 10 10) (make-tensor `(10 10))) (forward (Embedding 10 10) (make-tensor `(10 10)))))
        (check-kernel schedule 1)
        (caten/codegen/expr-cache:with-expr-cache ()
          (loop for item in (gather-kernels schedule)
                for count in `(3) do
                  (let ((bp (caten/codegen/blueprint:lower-schedule-item item (avm-graph avm) schedule)))
                    (ok (= count (count :FOR bp :key #'node-type)) (format nil "Expected ~a loops, got ~a" count (count :FOR bp :key #'node-type))))))))))
    
(deftest test-chunk-schedule
  (multiple-value-bind (schedule avm)
      (schedule-with-vars (apply #'!matmul (multiple-value-list (!chunk (make-tensor `(2 64 64)) 2))))
    (check-kernel schedule 2)
    ))

(deftest test-view-merge-failing-case
  (let ((schedule (schedule-with-vars (caten (!gelu (!matmul (make-tensor `(1 64 64)) (!t (make-tensor `(1 64 64)))))))))
    (check-kernel schedule 1)))
