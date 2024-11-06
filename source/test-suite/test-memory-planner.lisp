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
      (check-args 1 :tensor (caten (!tril (make-tensor `(5 5 5) :initial-element 1.0)))))))
