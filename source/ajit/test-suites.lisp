(in-package :cl-user)

(defpackage :caten/ajit.test
  (:use :cl :rove :caten/air :trivia :caten/ajit))

(in-package :caten/ajit.test)

;; TODO: Pooling2D, Conv2D, Gemm, Composed Gemm (count the number of kernels)
;; TODO: Tensor-Shaped-Tesnor Operation
;; TODO: Tensor-Shaped-Tensor Iteration Rendering (The scalar result should be passed via arguments)
;; TODO: Mixed use of dynamic shape and scalar values (e.g.: Adding Float[n, 10] += n)
;; (jit (caten (!add (make-tensor `(a 10)) (!cast (fconst 'a) :float32))) :debug 4)

