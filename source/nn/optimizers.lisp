(in-package :caten/nn)

(defclass AbstractOptimizer ()
  ((param :initarg :param :type Tensor :reader optimizer-param))
  (:documentation "
`AbstractOptimizer` is the base class for all optimizers. User defined optimizers can be defined by using `(defoptimizer ...)` macro.

One `AbstractOptimizer` corresponds to one `Tensor` class with `:requires-grad=T`. `(optimizer-param optimizer)` to get the corresponding tensor.
"))

(defgeneric step-optimize (optimizer) (:documentation "
```
(step-optimize optimizer)
```
A trigger to update the parameters of the optimizer. It is not recommended to compile a new lazy function in this method because it will be called multiple times in the training loop. Use cacheable function instead (e.g.: `caten/defun`)
"))

(defclass SGD (AbstractOptimizer)
  ((lr :initarg :lr))
  (:documentation "
Implements SGD Optimizer:

```math
Param_{new}\\gets{Param - Param_{grad}\\times{lr}}
```

where the initarg `:lr` is the learning rate.
"))

(caten/defun[float] (sgd-impl "sgd-impl") (n param grad lr)
  (!sub (make-tensor `(n) :from param) (!mul (make-tensor `(n) :from grad) (fconst lr)) :reduce t))

(defmethod step-optimize ((optimizer SGD))
  (with-slots ((param param) (lr lr)) optimizer
    (sgd-impl (dtype-of param) (apply #'* (shape param)) param (grad param) lr)))

;; [TODO] Adam
