(in-package :caten/test-suite)

(import-function "torch.nn.parameter.Parameter" :as "torch_as_param")
(import-function "torch.sum")
(python-exec "
def torch_backward(tensor): tensor.backward()
def torch_grad(tensor): return tensor.grad")
(import-function "torch_backward")
(import-function "torch_grad")
;; Ops to compare
(import-function "torch.neg")
(import-function "torch.abs")
(import-function "torch.exp")
(import-function "torch.exp2")
(import-function "torch.log")
(import-function "torch.log2")
(import-function "torch.sin")
(import-function "torch.cos")
(import-function "torch.tan")
(import-function "torch.sqrt")
(import-function "torch.reciprocal")

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
(macrolet ((def (opname lisp-name torch-name schedule &key (upfrom) (below))
             ;; e.g.: !max autodiff test is defined as !max-backward
             `(deftest ,(intern (string-upcase (format nil "~a-backward" opname)))
                (testing ,(format nil "Testing ~a" opname)
                  (let* ((x (if ,(and upfrom below)
                                (uniform `(10 10) :requires-grad t :low ,upfrom :high ,below)
                                (randn `(10 10) :requires-grad t))))
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
  (def recip !recip torch.reciprocal 2 :upfrom 1e-3 :below 3.0)
  (def neg !neg torch.neg 2)
  (def sin !sin torch.sin 2)
  (def cos !cos torch.cos 2 :upfrom 1e-5 :below (/ 2 pi))
  (def tan !tan torch.tan 2 :upfrom 1e-5 :below (/ 2 pi))
  (def tanh !tanh f:tanh 2)
  (def sqrt !sqrt torch.sqrt 2 :upfrom 1e-3 :below 3.0)
  
  (def abs !abs torch.abs 2)
  (def exp !exp torch.exp 2)
  (def log !log torch.log 2 :upfrom 1e-3 :below 3.0)
  (def exp2 !exp2 torch.exp2 2) ;; wrong
  (def log2 !log2 torch.log2 2 :upfrom 1e-3 :below 3.0)
  ;; Unary activations
  (def sigmoid !sigmoid f:sigmoid 2)
  (def hardsigmoid !hard-sigmoid f:hardsigmoid 2) ;; fail
  (def relu !relu f:relu 2)
  (def leaky-relu (lambda (x) (!leaky-relu x :neg-slope 1e-2)) f:leaky_relu 2)
  (def logsoftmax !log-softmax f:log_softmax 2) ;; fail
  (def elu !elu f:elu 2)
  (def relu6 !relu6 f:relu6 2)
  (def softmax !softmax f:softmax 2) ;; fail
  (def softplus !softplus f:softplus 2)
  (def softsign !softsign f:softsign 2)
  (def softshrink !softshrink f:softshrink 2)
  (def celu !celu f:celu 2)
  (def silu !silu f:silu 2)
  (def logsigmoid !logsigmoid f:logsigmoid 2)
  (def gelu !gelu f:gelu 2) ;; fail
  (def selu !selu f:selu 2)
  (def mish !mish f:mish 2)
  (def hardswish !hardswish f:hardswish 2)
  (def hardtanh !hardtanh f:hardtanh 2)
  (def softmin !softmin f:softmin 2)) ;; fail
;; !where, !expt,

;; Testing reduction
;; !sum !mean !max !min !prod !std !var

;; Testing !view backward
;; !matmul, concatenate, split, convnd, padding, RoPE uses many kind of !view so good to test
;; Embedding, Normalization, 

;; (deftest test-matmul
;; (deftest test-convnd
;; (deftest test-maxpool
;; If all of above things are tested -> Caten autodiff have an enough coverage

;; Training MNIST?
