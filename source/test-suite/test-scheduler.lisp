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
;; ~~ Testing reduction ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftest test-double-reduction-separated
  (testing "Reduction with different axes are not fused into a single kernel"
    (let ((schedule (schedule-with-vars (!sum (!sum (make-tensor `(10 10)) :axis 0 :keepdims t) :axis 1 :keepdims t))))
      (check-kernel schedule 2)))
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
  (with-no-grad
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
                      (ok (= count (count :FOR bp :key #'node-type)) (format nil "Expected ~a loops, got ~a" count (count :FOR bp :key #'node-type)))))))))))
;; ~~ Testing view merge ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftest test-chunk-schedule
  (multiple-value-bind (schedule avm)
      (schedule-with-vars (apply #'!matmul (multiple-value-list (!chunk (make-tensor `(2 64 64)) 2))))
    (check-kernel schedule 1)
    (caten/codegen/expr-cache:with-expr-cache ()
      (loop for item in (gather-kernels schedule)
            for count in `(4) do
              (let ((bp (caten/codegen/blueprint:lower-schedule-item item (avm-graph avm) schedule)))
                (ok (= count (count :FOR bp :key #'node-type)) (format nil "Expected ~a loops, got ~a" count (count :FOR bp :key #'node-type)))
                (testing "Valid load is val_2 = val_1[(((4096*(1+_gid0))+_gid2)+64*_gid3)]"
                  (let ((loader (find-if #'(lambda (x) (and (eql (node-type x) :EXPR) (equalp (symbol-name (car (node-writes x))) "val_2"))) bp))
                        (wmma   (find-if #'(lambda (x)
                                             (and (eql (node-type x) :EXPR)
                                                  (getattr x :reduction :allow-undefined t)))
                                         bp))
                        (renderer 'caten/codegen/renderer:CStyle-Renderer)
                        (index-space (map 'list #'(lambda (x) (expr-const x :int64)) '(a b c d))))
                    (flet ((r (n)
                             (caten/codegen/renderer:render-expr renderer (getattr n :EXPR) :index-space index-space)))
                      (ok (equalp (r wmma) "(val_9+(val_1[(((4096*a)+(64*b))+d)]*val_2))") (format nil "WMMA Part is rendered as ~a" (r wmma)))
                      (ok (equalp (r loader) "val_1[(((4096*(1+a))+c)+(64*d))]") (format nil "Loader Part is rendered as ~a" (r loader)))))))))))

(deftest test-view-merge-failing-case
  (let ((schedule (schedule-with-vars (caten (!gelu (!matmul (make-tensor `(1 64 64)) (!t (make-tensor `(1 64 64)))))))))
    (check-kernel schedule 1)))

;; Testing Expr Merging (e.g.: Count the number of expr in the group)
;; もうちょっとMinimalに考えたい
;;  (%view xxx)と(%view yyy)のMergeを考えるだけにしたい。
;; Adding more failing case!, the more the better
;; Can't we add more general tests for it?
;; [GOAL] Eliminate bugs from the scheduler
;; Failing ?
