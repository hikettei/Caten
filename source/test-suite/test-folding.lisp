(in-package :caten/test-suite)

(defun %arange (shape a b &key (dtype :float32) (order :row))
  "creates a tensor where each element is generated using alpha * i + beta."
  (with-context
      (m (%make-tensor shape :dtype dtype :order order))
    (i (%index-components m (%shape shape)))
    (alpha (%load (%salloc :dtype dtype) a))
    (beta  (%load (%salloc :dtype dtype) b))
    (t1 (%mul i alpha))
    (t2 (%add t1 beta))
    (c  (%store m t2 :id 'out))))

(defparameter aranged-tensor
  (%arange '(1 1 3 3) 1 0 :dtype :float32 :order :row))


(defparameter realized-tensor (realize-graph aranged-tensor :buffer-type 'caten/byoc/lisp:LispBuffer))

(defparameter tensor-with-buffer
  (make-tensor '(1 1 3 3) :dtype :float32 :order :row :from realized-tensor))

(defparameter input-tensor (proceed tensor-with-buffer))

;; (1 2 3
;; 4 5 6
;; 6 8 9)

(print input-tensor)

(defparameter unfolded-tensor  (!copy (!unfold input-tensor '(2 2) '(1 1)))))

(print unfolded-tensor)

(print (caten unfolded-tensor))
(print (proceed unfolded-tensor))


