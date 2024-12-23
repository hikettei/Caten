(in-package :caten/test-suite)

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

(defparameter aranged-tensor
  (%arange '(8) 1 0 :dtype :float32 :order :row))

(defparameter realized-tensor (%realize aranged-tensor))

(defparameter tensor-with-buffer
  (make-tensor '(8) :dtype :float32 :order :row :from realized-tensor))



(defparameter proceeded-tensor (proceed tensor-with-buffer))
(print proceeded-tensor)

(print )

(defparameter t3 (make-tensor '(8) :initial-element 1.0))
(print (caten t3))
(print (!unfold proceeded-tensor 1 1))




(print (caten (!unfold tensor 3 1)))


(defparameter tensor (make-tensor `(8) :initial-element 1.0))
(defparameter reshaped-tensor (!reshape tensor '(4 2)))

(print (proceed reshaped-tensor))

(defparameter tensor1 (make-tensor '(1) :initial-element 1))

(print (caten tensor1))
(untrace caten proceed forward lower)

(defparameter contiguous-tensor (!contiguous (make-tensor `(8) :initial-element 1.0)))

(print (type-of contiguous-tensor))



(defparameter unfolded-tensor (!unfold contiguous-tensor 3 1))


(print unfolded-tensor)
(defparameter proceeded-unfolded (proceed unfolded-tensor))

(print proceeded-unfolded)



