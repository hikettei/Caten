(in-package :caten/test-suite)

(import-function "torch.var")
(import-function "torch.std")

;; [TODO] Fuse in a single kernel (var/std)
(deftest test-variance
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(30 30))))
      (assert-equal
          (:atol 1e-5 :rtol 1e-6)
          (with-torch (x) (->caten (torch.var x :axis -1 :keepdims t)))
          (proceed (!variance x :axis -1))))))

(deftest test-std
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(30 30))))
      (assert-equal
          (:atol 1e-5 :rtol 1e-6)
          (with-torch (x) (->caten (torch.std x :axis -1 :keepdims t)))
          (proceed (!std x :axis -1))))))
