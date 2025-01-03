(in-package :caten/nn)
;; ~~~~~ Movements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; - [ ] Rolling -> Unfold
;;   - [ ] Make it differentiable
;;   - [ ] Padding
;;   - [ ] Group
;;   - [ ] Dilation
;;   - [ ] Stride
;; - [ ] Export
;; - [ ] (with-no-grad (caten (!copy (caten/apis::!window (!padding (ax+b `(10 3 21 21) 1 0) `((0 0) (0 0) (2 2) (2 2))) `(1 1 5 5)))))
;; - [ ] Move to caten/nn and implement as fold/unfold?
(defclass Unfold (Func)
  ((kernel-size :initarg :kernel-size :accessor unfold-kernel-size)
   (args :accessor unfold-args)))

(defmethod unfold-shape ((op Unfold) shape window-shape)
  (assert (= (length shape) (length window-shape)))
  (append
   (loop for s in shape for w in window-shape
         collect (!add (!- (->iconst s) (->iconst w)) (iconst 1)))
   (map 'list #'->iconst window-shape)))
(defmethod unfold-stride ((op Unfold) stride) (map 'list #'->iconst (append stride stride)))

(defmethod forward ((op Unfold) &rest tensors)
  (assert (= 1 (length tensors)))
  (assert (tr-contiguous (tensor-tr (car tensors))))
  (let* ((shape (unfold-shape op (shape (car tensors)) (unfold-kernel-size op)))
         (stride (unfold-stride op (tr-stride (tensor-tr (car tensors)))))
         (out (make-tensor shape :dtype (dtype-of (car tensors)) :order (order (car tensors)))))
    (setf (tr-stride (tensor-tr out)) stride
          (tr-contiguous (tensor-tr out)) nil
          (unfold-args op) (append tensors shape stride))
    out))

(defmethod backward ((op Unfold) &optional prev-grad) (declare (ignore prev-grad)) (error "WIP: Unfold Backward"))

(defmethod lower ((op Unfold) &rest inputs)
  ;; Inputs: (tensor *shape *stride)
  (let ((n (length (unfold-kernel-size op))))
    (multiple-value-bind (op shape stride)
        (values (car inputs) (subseq inputs 1 (1+ (* 2 n))) (subseq inputs (1+ (* 2 n))))
      (assert (= (length shape) (length stride) (* 2 n)))
      (with-context
          (out (%view
                op shape
                (loop repeat (* 2 n) collect (%iconst 0))
                shape (loop repeat (* 2 n) collect (%iconst 1)) (loop repeat (* 2 n) collect nil) stride
                :id (gensym "ROLL")))))))

(defun !unfold (x kernel-size)
  (declare (type tensor x) (type list kernel-size))
  (assert (= (length (shape x)) (length kernel-size)))
  (let ((out (forward (make-instance 'Unfold :kernel-size kernel-size) (!contiguous x))))
    (setf (tensor-variables out) (unfold-args (tensor-op out))
          (func-variables (tensor-op out)) (unfold-args (tensor-op out)))
    out))
