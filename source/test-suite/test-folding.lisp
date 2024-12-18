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
  (%arange '(8) 1 0 :dtype :float32 :order :row)) ;; Shape: (4)

(defparameter realized-tensor (%realize aranged-tensor))

(defparameter tensor-with-buffer
  (make-tensor '(8) :dtype :float32 :order :row :from realized-tensor))

(defparameter reshaped-tensor 
  (!reshape tensor '(2 4)))



;; Step 3: Print the reshaped tensor
(format t "Reshaped Tensor (2x4):~%")
(print (proceed reshaped-tensor))

;; Step 4: Use !view to extract specific slices
;; Subscript for a 2D Tensor: First dimension (rows), second dimension (columns)
(defparameter viewed-tensor
  (!view reshaped-tensor '(0 2 1) '(0 4 1))) ;; Full view with no strides

;; Step 5: Print the viewed tensor
(format t "Viewed Tensor (2 Windows):~%")
(print (proceed viewed-tensor))


(print (tr-broadcast (tensor-tr (!view (make-tensor `(1 1) :initial-element 1.0) `(:~ 3) `(:~ 3)))))



(print (proceed (!view (make-tensor `(1 1) :initial-element 1.0) `(:~ 3) `(:~ 3))))



(print (proceed tensor-with-buffer))


(defun unfold (tensor window-size stride)
  "unfolds a 1d tensor into overlapping windows."
  (let* ((rows (/ (car (shape tensor)) window-size)) ;; calculate the columns
         (reshaped (!reshape tensor `( ,rows ,window-size)))) ;; reshape tensor
    (!view reshaped `(0 ,rows ,stride) t)))



(print (proceed (unfold tensor-with-buffer 4 1)))

(print (proceed (!unfold tensor-with-buffer 4 1)))

