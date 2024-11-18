(in-package :caten/test-suite)

(deftest check-in-place-mutation
  (with-no-grad
    (with-protect-jit
      (check-args 1 `(3 3) (caten (!tan (make-tensor `(3 3)))))
      (check-args 1 `(3 3) (caten (!tan (!tan (!tan (make-tensor `(3 3)))))))
      (check-args 1 `(3 3) (caten (!softmax (make-tensor `(3 3)))))
      (check-args 1 `(3 3) (caten (!softmax (ax+b `(3 3) 1 1))))
      (check-args 1 `(3 3) (caten (!softmax (!softmax (make-tensor `(3 3))))))
      (check-args 1 `(t t) (caten (!softmax (!softmax (make-tensor `(a b))))))
      (check-args 1 :tensor (caten (!tril (make-tensor `(5 5) :initial-element 1.0))))
      (check-args 1 :tensor (caten (!tril (make-tensor `(5 5 5) :initial-element 1.0))))
      (check-args 1 `(9) (caten (!rand `(3 3)))))))
;; [TODO] Testing the following things
;; - Schedule Cache for the big graph construction
;; - Memory Planner for the big graph
;; - Memory Planner for the big and cached schedule construction
;; - Tested by doing (!matmul (!matmul (!matmul (!matmul ...)))) x 10
;; - Tested by running the transformer with an dummy input
(deftest memory-planner-recursive-call-regression-test
  (testing "Testing: Infinite Recursion is not happening?"
    (caten (!where (make-tensor `(s s s s)) (make-tensor `(s s s s)) (make-tensor `(s s s s))))))
