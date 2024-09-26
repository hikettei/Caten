(in-package :caten/test-suite)

(deftest nested-class-lower-failing-case
  (let ((model (caten (forward (FeedForward 10 10) (make-tensor `(10 10))))))
    (if (= 0 (ctx:getenv :JIT))
	(ok (>= (length (graph-nodes (avm-graph model))) 20))
        ;; If (!matmul x (!transpose y)) is implemented, this will be 1 or 2 kernels.
	(ok (= (n-kernels model) 4)))))
