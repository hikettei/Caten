(in-package :caten/test-suite)

(import-function "torch.nn.parameter.Parameter" :as "torch_as_param")
(import-function "torch.sum")
(python-exec "
def torch_backward(tensor): tensor.backward()
def torch_grad(tensor): return tensor.grad")
(import-function "torch_backward")
(import-function "torch_grad")

(defmacro with-torch-params ((&rest params) &body body)
  `(with-torch (,@params)
     (let (,@(loop for p in params collect `(,p (torch_as_param ,p))))
       ,@body)))

(defun check-bw-schedule (model expected)
  (when (= 1 (ctx:getenv :JIT))
    (let ((count (count :JIT_KERNEL (graph-nodes (avm-graph model)) :key #'node-type)))
      (ok (= count expected) (format nil "Expected ~a, scheduled ~a" expected count)))))
;; ReLU/SigMoid should be tested independently
(deftest test-relu-backward
  (testing "Testing backward for ReLU(MaxOp)"
    (let* ((x (randn `(10 10) :requires-grad t)))
      (assert-equal
          (:rtol 1e-5 :atol 1e-5)
          (with-torch-params (x)
            (let ((y (torch.sum (f:relu x))))
              (torch_backward y)
              (->caten (torch_grad x))))
          (let ((m (caten (!sum (!relu x)))))
            (check-bw-schedule m 2) ;; 1 for forward, 1 for backward
            (forward m)
            (backward m)
            (grad x))))))
;; (deftest test-digmoid
;; (deftest test-matmul
;; (deftest test-convnd
;; (deftest test-maxpool

