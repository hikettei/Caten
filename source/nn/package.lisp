(cl:in-package :cl-user)
(defpackage :caten/nn
  (:documentation "Caten Neural Network Frontends
Policy:
  - We only provide the official implementation to established and well used nn modules.
  - You have to ensure that each files works standalone. (Use 1 file, 1 package coding rule at caten/nn)
  - Each module should be tested well (we expected to have a ton of regression tests here); 1 file 1 test-suite.
  - TODO: Add test-helpers.lisp")
  (:local-nicknames (:docs :caten/common.documentation))
  (:use :cl :caten :caten/air :alexandria :caten/ir)
  ;; from activations.lisp
  (:export
   #:Sigmoid
   #:!sigmoid

   #:HardSigmoid
   #:!hard-sigmoid

   #:ReLU
   #:!relu

   #:LeakyReLU
   #:!leaky-relu

   #:LogSoftmax
   #:!log-softmax

   #:ELU
   #:!elu

   #:ReLU6
   #:!relu6

   #:Softmax
   #:!softmax

   #:Softplus
   #:!softplus

   #:Softsign
   #:!softsign

   #:SoftShrink
   #:!softshrink

   #:GeLU
   #:!gelu

   #:SeLU
   #:!selu

   #:CeLU
   #:!celu

   #:LogSigmoid
   #:!logsigmoid

   #:SiLU
   #:!silu

   #:HardSwish
   #:!hardswish

   #:Mish
   #:!mish

   #:HardTanh
   #:!hardtanh

   #:Softmin
   #:!softmin
   )
  ;; from normalization.lisp
  (:export
   #:LayerNorm
   #:!layer-norm
   #:BatchNorm
   #:!batch-norm
   #:RMSNorm
   #:!rms-norm)
  ;; from embedding.lisp
  (:export
   #:Embedding)
  ;; from positional-encoding.lisp
  (:export
   #:RoPE
   #:!rope)
  ;; from conv.lisp
  (:export
   #:ConvND
   #:!convnd
   #:convnd-weight
   #:convnd-bias)
  ;; from criterion.lisp
  (:export
   #:L1NormLoss
   #:!l1norm
   #:MSELoss
   #:!mse
   #:CrossEntropyLoss
   #:!cross-entropy)
  ;; from padding.lisp
  (:export
   #:Padding
   #:!padding
   #:!padding2d)
  ;; from pool.lisp
  (:export
   #:AvgPool
   #:!avgpool
   #:MaxPool
   #:!maxpool)
  ;; from optimizers.lisp
  (:export
   #:AbstractOptimizer #:optimizer-param #:param
   #:hook-optimizers
   #:step-optimizer
   #:zero-grad
   #:SGD)
  ;; from linear.lisp
  (:export
   #:Linear)
  ;; from unfold.lisp
  (:export
   #:Unfold
   #:!unfold))

(in-package :caten/nn)

(defmacro slice (list upfrom &optional (below `(length ,list)) (by 1))
  (with-gensyms (upfrom1 below1)
    `(let* ((redirect (signum ,by))
	    (,upfrom1 (if (>= ,upfrom 0) ,upfrom (+ (length ,list) ,upfrom)))
	    (,below1  (when ,below (if (>= ,below 0) ,below (+ (length ,list) ,below))))
	    (out
	      (loop for i upfrom ,upfrom1 below (or ,below1 (length ,list)) by (abs ,by) collect (nth i ,list))))
       (if (= 1 redirect)
	   out
	   (reverse out)))))

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))
