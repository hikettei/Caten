(in-package :caten/test-suite)

(deftest nested-class-lower-failing-case
  (let ((model (caten (forward (FeedForward 10 10) (make-tensor `(10 10))))))
    (if (= 0 (ctx:getenv :JIT))
	(ok (>= (length (graph-nodes (avm-graph model))) 20))
	(ok (= (n-kernels model) 2)))))
