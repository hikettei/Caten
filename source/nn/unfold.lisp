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
   (dilation :initarg :dilation :accessor unfold-dilation)
   (ceiling :initarg :ceiling :accessor unfold-ceiling)
   (args :accessor unfold-args)))

(defmethod unfold-shape ((op Unfold) shape window-shape)
  (with-slots ((ceiling ceiling) (strides strides) (dilation dilation)) op
    (append
     (map 'list #'->iconst (slice shape 0 (- (length window-shape))))
     (loop for s in (slice shape (- (length window-shape)))
           for w in window-shape
           for d in dilation
           for stride in strides
           collect (->iconst (conv-out-size s 0 d w stride :ceiling ceiling)))
     (map 'list #'->iconst window-shape))))

(defmethod unfold-stride ((op Unfold) stride)
  (with-slots ((ceiling ceiling) (strides strides) (dilation dilation)) op
    (let ((n (length (unfold-kernel-size op))))
      (append
       (map 'list #'->iconst (slice stride 0 (- n)))
       (loop for s1 in (slice stride (- n)) for s2 in strides
             collect (!mul (->iconst s1) (->iconst s2)))
       (loop for s1 in (slice stride (- n)) for s2 in dilation
             collect (!mul (->iconst s1) (->iconst s2)))))))

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
  (warn "Unfold is not differentiable. Please use _pool if you need gradients."))

(defmethod lower ((op Unfold) &rest inputs)
  ;; Inputs: (tensor *shape *stride)
  (let ((n (+ (ndim (car (func-variables op))) (length (unfold-kernel-size op)))))
    (multiple-value-bind (op shape stride)
        (values (car inputs) (subseq inputs 1 (1+ n)) (subseq inputs (1+ n)))
      (assert (= (length shape) (length stride) n))
      (with-context
          (out
           (%view op shape
                  (loop repeat n collect (%iconst 0))
                  shape (loop repeat n collect (%iconst 1))
                  (loop repeat n collect nil) stride :id (gensym "UNFOLD")))))))
;; kernel-size stride dilationがあればOK
;; [TODO] Fuse with Padding2D,
;; Padded ConvND=1Kernel
(defun !unfold (x kernel-size &key (dilation 1) (stride 1) (ceiling #'ceiling))
  "Unfold
```
(!unfold x kernel-size &key (dilation 1) (stride 1) (ceiling #'ceiling))
```
Note: this function has two implementation ...
"
  (declare (type tensor x) (type list kernel-size))
  (let ((dilation (maybe-list dilation kernel-size))
        (stride (maybe-list stride kernel-size)))
    ;; [todo] assert for c h w is static
    ;; Note: !unfold is not differentiable so we are going to use an alternative implementation.
    (when (null caten/apis::*no-grad*)
      (return-from !unfold (_pool x kernel-size stride dilation :ceiling ceiling)))
    (let ((out (forward (make-instance 'UnFold :kernel-size kernel-size :dilation dilation :strides stride :ceiling ceiling) (!contiguous x))))
      (setf (tensor-variables out) (unfold-args (tensor-op out))
            (func-variables (tensor-op out)) (unfold-args (tensor-op out)))
      out)))
;; [todo] clean up
(defun pool-out ()
  (caten (!copy (_pool (ax+b `(10) 1 0) `(5) 1 1))))
;; (with-no-grad
;;   (caten (caten/nn::!unfold (!padding2d (make-tensor `(10 3 23 23)) `(1 1 1 1)) `(5 5))))
(defun unfold-out ()
  (caten (!copy (!unfold (ax+b `(10) 1 0) `(5)))))
