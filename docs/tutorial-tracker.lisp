;; The objective of this tutorial is to understand how the tensor-tracking feature works, what are views, reshapes...

(ql:quickload :caten)

(defpackage :getting-started
  (:use :cl :caten/air :caten/aasm :caten/apis :caten/avm))

;;; First, let's define a tensor with arange.

(defun %arange (shape a b &key (dtype :float32) (order :row))
  "Creates a tensor where each element is generated using alpha * i + beta."
  (with-context
      (m (%make-tensor shape :dtype dtype :order order))
    (i (%index-components m (%shape shape)))
    (alpha (%load (%salloc :dtype dtype) a))
    (beta  (%load (%salloc :dtype dtype) b))
    (t1 (%mul i alpha))
    (t2 (%add t1 beta))
    (c  (%store m t2 :id 'out))))
)
(defparameter aranged-tensor
  (%arange '(6) 1 0 :dtype :float32 :order :row))

(defparameter realized-tensor (%realize aranged-tensor))

(defparameter tensor-with-buffer
  (make-tensor '(8) :dtype :float32 :order :row :from realized-tensor))



;; In Caten, a tensor is more than just data. It's also a symbolic node in a computational graph. When you perform an operation (like reshape), you're not inmediately computing the result. Instead you create a new symbolic tensor that represents how to compute the result once the final program is lowered an executed.


;; The forward method in the classes of caten/apis isfor constructing and returning a new symbolic tensor WITHOUT BEING COMPUTED. This includes its shape, dependencies...

;; THe lower method takes these symbolic descriptions and translates them into an intermediate representation that can be used to produce the result.


;; What does forward do?
;; Forward takes the input tensor(s)
;; Calculates what the new shape of the output should be.
;; Creates a 