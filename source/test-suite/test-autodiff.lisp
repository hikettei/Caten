(in-package :caten/test-suite)

(import-function "torch.nn.parameter.Parameter" :as "torch_as_param")
(import-function "torch.sum")
(python-exec "
def torch_backward(tensor): tensor.backward()
def torch_grad(tensor): return tensor.grad")
(import-function "torch_backward")
(import-function "torch_grad")
;; unary ops
(import-function "torch.neg")
(import-function "torch.abs")
(import-function "torch.exp")
(import-function "torch.log")
(import-function "torch.sin")

(defmacro with-torch-params ((&rest params) &body body)
  `(with-torch (,@params)
     (let (,@(loop for p in params collect `(,p (torch_as_param ,p))))
       ,@body)))

(defun check-bw-schedule (model expected)
  (when (= 1 (ctx:getenv :JIT))
    (let ((count (count :JIT_KERNEL (graph-nodes (avm-graph model)) :key #'node-type)))
      (ok (= count expected) (format nil "Expected ~a, scheduled ~a" expected count)))))
;; ReLU/SigMoid should be tested independently
;; Activation Backwards
(macrolet ((def (opname lisp-name torch-name schedule)
             ;; e.g.: !max autodiff test is defined as !max-backward
             `(deftest ,(intern (string-upcase (format nil "~a-backward" opname)))
                (testing ,(format nil "Testing ~a" opname)
                  (let* ((x (randn `(10 10) :requires-grad t)))
                    (assert-equal
                        (:rtol 1e-4 :atol 1e-5)
                        (with-torch-params (x)
                          (let ((y (torch.sum (funcall #',torch-name x))))
                            (torch_backward y)
                            (->caten (torch_grad x))))
                        (let ((m (caten (!sum (funcall #',lisp-name x)))))
                          (check-bw-schedule m ,schedule) ;; 1 for forward, 1 for backward
                          (forward m)
                          (backward m)
                          (grad x))))))))
  ;; mathematical functions
  (def neg !neg torch.neg 2)
  (def sin !sin torch.sin 2)
  (def abs !abs torch.abs 2)
  (def exp !exp torch.exp 2) ;; wrong
  ;; (def log !log torch.log 2) todo
  ;; Unary activations
  (def sigmoid !sigmoid f:sigmoid 2)
  (def hardsigmoid !hard-sigmoid f:hardsigmoid 2) ;; fail
  (def relu !relu f:relu 2)
  (def leaky-relu (lambda (x) (!leaky-relu x :neg-slope 1e-2)) f:leaky_relu 2)
  (def logsoftmax !log-softmax f:log_softmax 2) ;; fail
  (def elu !elu f:elu 2) ;; fail
  (def relu6 !relu6 f:relu6 2)
  (def softmax !softmax f:softmax 2) ;; fail
  (def softplus !softplus f:softplus 2) ;; fail
  (def softsign !softsign f:softsign 2)
  (def softshrink !softshrink f:softshrink 2)
  (def celu !celu f:celu 2) ;; fail
  (def silu !silu f:silu 2)
  (def logsigmoid !logsigmoid f:logsigmoid 2) ;; fail
  (def gelu !gelu f:gelu 2) ;; fail
  (def selu !selu f:selu 2) ;; fail
  (def mish !mish f:mish 2) ;; fail
  (def hardswish !hardswish f:hardswish 2)
  (def hardtanh !hardtanh f:hardtanh 2)
  (def softmin !softmin f:softmin 2)) ;; fail
;; (deftest test-matmul
;; (deftest test-convnd
;; (deftest test-maxpool

