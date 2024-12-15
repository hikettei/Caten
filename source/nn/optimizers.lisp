(in-package :caten/nn)

(defclass AbstractOptimizer ()
  ((param :initarg :param :type Tensor :reader optimizer-param))
  (:documentation "
`AbstractOptimizer` is the base class for all optimizers. User defined optimizers can be defined by using `(defoptimizer ...)` macro.

One `AbstractOptimizer` corresponds to one `Tensor` class with `:requires-grad=T`. `(optimizer-param optimizer)` to get the corresponding tensor.
"))

(defgeneric step-optimizer (optimizer) (:documentation "
```
(step-optimizer optimizer)
```

A trigger to update the parameters of the optimizer. It is not recommended to compile a new lazy function in this method because it will be called multiple times in the training loop. Use cacheable function instead (e.g.: `caten/defun`)
"))

(defun hook-optimizers (avm hooker)
  "
```
(hook-optimizers avm hooker)
```

This function searches for all Allocate node in the avm-graph, and if there is a tensor that requires-grad=T, it will call the function `hooker` with the tensor as the argument.

A list of created optimizers are returned.

hooker is an function that takes one argument, which is the tensor that requires-grad=T, returns the AbstractOptimizer.
"
  (declare (type caten/avm:avm avm) (type function hooker))
  (map
   'list
   #'(lambda (x) (assert (subtypep x 'AbstractOptimizer) () "hook-optimizers: ~a is not an AbstractOptimizer!" x) x)
   (loop for node in (graph-nodes (caten/avm:avm-graph avm))
         if (and (eql (node-type node) :Allocate) (getattr node :from) (tensor-requires-grad (getattr node :from)))
           collect (funcall hooker (getattr node :from)))))

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

(defmethod step-optimizer ((optimizer SGD))
  (with-slots ((param param) (lr lr)) optimizer
    (sgd-impl (dtype-of param) (apply #'* (shape param)) param (grad param) lr)))
;; [TODO] Adam
;; [TODO] End-to-end training example
