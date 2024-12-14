(in-package :caten/test-suite)

(import-function "torch.nn.parameter.Parameter" :as "torch_as_param")
(import-function "torch.sum")
(import-function "torch.mean")
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
;; Binary Ops
(import-function "torch.add")
(import-function "torch.sub")
(import-function "torch.mul")
(import-function "torch.div")
(import-function "torch.maximum")
(import-function "torch.minimum")
(import-function "torch.transpose")
(python-exec "def torch_gelu(x): return torch.nn.functional.gelu(x, approximate='tanh')")
(import-function "torch_gelu")

(defmacro with-torch-params ((&rest params) &body body)
  `(with-torch (,@params)
     (let (,@(loop for p in params collect `(,p (torch_as_param ,p))))
       ,@body)))

(defun check-bw-schedule (model expected)
  (when (= 1 (ctx:getenv :JIT))
    (let ((count (count :JIT_KERNEL (graph-nodes (avm-graph model)) :key #'node-type)))
      (ok (= count expected) (format nil "Expected ~a, scheduled ~a" expected count)))))
;; Testing Unary Backwards
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
                          ;; (check-bw-schedule m ,schedule) ;; 1 for forward, 1 for backward
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
  (def exp2 !exp2 torch.exp2 2)
  (def log2 !log2 torch.log2 2 :upfrom 1e-3 :below 3.0)
  ;; Unary activations
  (def sigmoid !sigmoid f:sigmoid 2)
  ;; (def hardsigmoid (lambda (x) (!hard-sigmoid x :alpha 3.0 :beta -3.0)) f:hardsigmoid 2) TODO: Definitions do not match with pytorch?
  (def relu !relu f:relu 2)
  (def leaky-relu (lambda (x) (!leaky-relu x :neg-slope 1e-2)) f:leaky_relu 2)
  (def logsoftmax !log-softmax f:log_softmax 2) ;; fail due to softmax
  (def elu !elu f:elu 2)
  (def relu6 !relu6 f:relu6 2)
  (def softmax !softmax f:softmax 2) ;; almost working but unstable due to x*y.recip(), should work with JIT?
  (def softplus !softplus f:softplus 2)
  (def softsign !softsign f:softsign 2)
  (def softshrink !softshrink f:softshrink 2)
  (def celu !celu f:celu 2)
  (def silu !silu f:silu 2)
  (def logsigmoid !logsigmoid f:logsigmoid 2)
  (def gelu !gelu torch_gelu 2)
  (def selu !selu f:selu 2)
  (def mish !mish f:mish 2)
  (def hardswish !hardswish f:hardswish 2)
  (def hardtanh !hardtanh f:hardtanh 2)
  (def softmin !softmin f:softmin 2)) ;; fail due to the same reason for softmax
(python-exec "def torch_max(x, dim=None): return torch.max(x, dim=dim)[0]")
(import-function "torch_max")
;; Test reductions
(macrolet ((def (opname lisp-name torch-name schedule shape &key axis use-linspace)
             `(deftest ,(intern (string-upcase (format nil "~a-backward" opname)))
                (testing ,(format nil "Testing ~a" opname)
                  (let* ((x (if ,use-linspace
                                (linspace ',shape 0 1 :requires-grad t)
                                (randn ',shape :requires-grad t))))
                    (assert-equal
                        (:rtol 1e-4 :atol 1e-5)
                        (with-torch-params (x)
                          (let ((y (torch.sum (,torch-name x :dim ,axis))))
                            (torch_backward y)
                            (print (->caten (torch_grad x)))))
                        (let ((m (caten (!sum (,lisp-name x :axis (or ,axis t))))))
                          (check-bw-schedule m ,schedule) ;; 1 for forward, 1 for backward
                          (forward m)
                          (backward m)
                          (print (grad x)))))))))
  (def sum1d !sum torch.sum 2 (10))
  (def mean1d !mean torch.mean 2 (10))
  (def sum2d !sum torch.sum 2 (10 10))
  (def mean2d !mean torch.mean 2 (10 10))
  (def sum3d !sum torch.sum 2 (10 10 10))
  (def mean3d !mean torch.mean 2 (10 10 10))
  
  (def sum2d-axis !sum torch.sum 2 (10 10) :axis 1)
  (def mean2d-axis !mean torch.mean 2 (10 10) :axis 1)
  (def sum3d-axis !sum torch.sum 2 (10 10 10) :axis 1)
  (def mean3d-axis !mean torch.mean 2 (10 10 10) :axis 1)
  (def max2d !max torch_max 2 (10 10) :axis -1)
  ;; MAX(1, 1) -> only the first location can be chosen
  (def max2d-same-points !max torch_max 2 (10 10) :axis -1 :use-linspace t))
;; Test Binaries
(macrolet ((def (opname lisp-name torch-name shape1 shape2 &key (rhs-positive) (transpose-lhs nil) (transpose-rhs nil) (same-point nil))
             `(deftest ,(intern (string-upcase (format nil "~a-backward" opname)))
                (testing
                    ,(format nil "Testing ~a" opname)
                  (let ((x (if ,same-point
                               (linspace ',shape1 0 1 :requires-grad t)
                               (randn ',shape1 :requires-grad t)))
                        (y (if ,rhs-positive
                               (uniform ',shape2 :low 2.0 :high 3.0 :requires-grad t)
                               (if ,same-point
                                   (linspace ',shape2 0 1 :requires-grad t)
                                   (randn ',shape2 :requires-grad t)))))
                    (assert-equals
                        (:rtol 1e-4 :atol 1e-3)
                        (with-torch-params (x y)
                          (let ((out (torch.sum (,torch-name (if ,transpose-lhs (torch.transpose x -1 -2) x) (if ,transpose-rhs (torch.transpose y -1 -2) y)))))
                            (torch_backward out)
                            (values (->caten (torch_grad x)) (->caten (torch_grad y)))))
                        (let ((m (caten (!sum (,lisp-name (if ,transpose-lhs (!t x) x) (if ,transpose-rhs (!t y) y))))))
                          ;; (->dot (avm-graph m))
                          (forward m)
                          (backward m)
                          (values (grad x) (grad y)))))))))
  (def add !add torch.add (10 10) (10 10))
  (def add-broadcast-1 !add torch.add (10 10) (10 10 10))
  (def add-broadcast-2 !add torch.add (10 10 10) (10 10))
  
  (def sub !sub torch.sub (10 10) (10 10))
  (def sub-broadcast-1 !sub torch.sub (10 10) (10 10 10))
  (def sub-broadcast-2 !sub torch.sub (10 10 10) (10 10))
  
  (def mul !mul torch.mul (10 10) (10 10))
  (def mul-broadcast-1 !mul torch.mul (10 10) (10 10 10))
  (def mul-broadcast-2 !mul torch.mul (10 10 10) (10 10))
  
  (def div !div torch.div (10 10) (10 10) :rhs-positive t)
  (def div-broadcast-1 !div torch.div (10 10) (10 10 10) :rhs-positive t)
  (def div-broadcast-2 !div torch.div (10 10 10) (10 10) :rhs-positive t)
  
  (def maximum !maximum torch.maximum (10 10) (10 10))
  (def maximum-same-point !maximum torch.maximum (10 10) (10 10) :same-point t)
  (def minimum !minimum torch.minimum (10 10) (10 10))
  
  (def matmul1 !matmul torch.matmul (10 10) (10 10))
  (def matmul2 !matmul torch.matmul (10 20) (20 30))
  (def matmul3 !matmul torch.matmul (10 20) (10 20 30))
  (def matmul4 !matmul torch.matmul (10 10 20) (20 30))

  (def matmul1-transpose-1 !matmul torch.matmul (10 10) (10 10) :transpose-lhs t)
  (def matmul2-transpose-1 !matmul torch.matmul (30 10) (30 20) :transpose-lhs t)
  (def matmul3-transpose-1 !matmul torch.matmul (20 10) (30 20 10) :transpose-lhs t)
  (def matmul4-transpose-1 !matmul torch.matmul (10 30 10) (30 20) :transpose-lhs t)
  
  (def matmul1-transpose-2 !matmul torch.matmul (10 10) (10 10) :transpose-rhs t)
  (def matmul2-transpose-2 !matmul torch.matmul (30 10) (20 10) :transpose-rhs t)
  (def matmul3-transpose-2 !matmul torch.matmul (20 10) (30 20 10) :transpose-rhs t)
  (def matmul4-transpose-2 !matmul torch.matmul (10 30 10) (30 10) :transpose-rhs t))
;; [TODO]
;; 1. Slice/Padding
;; 2. ConvND (TODO)
;; 3. Having Better Schedule for Embedding
;; 4. Fix for JIT
;; 5. Enable Kernel Fusion
