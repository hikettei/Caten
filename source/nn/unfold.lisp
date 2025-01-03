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
   (strides :initarg :strides :accessor unfold-strides)
   (padding :initarg :padding :accessor unfold-padding)
   (dilation :initarg :dilation :accessor unfold-dilation)
   (args :accessor unfold-args)))

(defmethod unfold-shape ((op Unfold) shape window-shape)
  (append
   (map 'list #'->iconst (slice shape 0 (- (length window-shape))))
   (loop for s in (slice shape (- (length window-shape))) for w in window-shape
         collect (!add (!- (->iconst s) (->iconst w)) (iconst 1)))
   (map 'list #'->iconst window-shape)))

(defmethod unfold-stride ((op Unfold) stride)
  (let ((n (length (unfold-kernel-size op))))
    (append
     (map 'list #'->iconst (slice stride 0 (- n)))
     (map 'list #'->iconst (slice stride (- n)))
     (map 'list #'->iconst (slice stride (- n))))))

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

(defmethod backward ((op Unfold) &optional prev-grad)
  ;; [TODO] Differentiable Unfold
  (warn "Unfold is not differentiable. Please use alternative implementation for now."))

(defmethod lower ((op Unfold) &rest inputs)
  ;; Inputs: (tensor *shape *stride)
  (let ((n (length (unfold-kernel-size op))))
    (with-slots ((kernel-size kernel-size) (strides strides) (padding padding) (dilation dilation) (args args)) op
      (multiple-value-bind (op shape stride)
          (values (car inputs) (subseq inputs 1 (1+ (* 2 n))) (subseq inputs (1+ (* 2 n))))
        (assert (= (length shape) (length stride) (* 2 n)))
        (with-context
            (out
             (%view op shape
                    (loop repeat (* 2 n) collect (%iconst 0))
                    shape (loop repeat (* 2 n) collect (%iconst 1))
                    (loop repeat (* 2 n) collect nil) stride :id (gensym "UNFOLD"))))))))

(defun !unfold (x kernel-size &key (dilation 1) (padding 0) (stride 1))
  (declare (type tensor x) (type list kernel-size))
  (let ((out (forward
              (make-instance
               'Unfold
               :kernel-size kernel-size
               :dilation (maybe-list dilation kernel-size)
               :padding (maybe-list padding kernel-size)
               :strides (maybe-list stride kernel-size))
              (!contiguous x))))
    (setf (tensor-variables out) (unfold-args (tensor-op out))
          (func-variables (tensor-op out)) (unfold-args (tensor-op out)))
    out))
