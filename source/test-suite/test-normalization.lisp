(in-package :caten/test-suite)

(import-function "torch.var")
(import-function "torch.std")
(import-function "torch.ones")

(python-exec
 "
# Taken from https://pytorch.org/torchtune/0.2/_modules/torchtune/modules/rms_norm.html
def torch_rms_norm(x):
  # computation is in fp32
  x_fp32 = x.float()
  x_normed = (x_fp32 * torch.rsqrt(x_fp32.pow(2).mean(-1, keepdim=True) + 1e-5)).type_as(x)
  return x_normed
")
(import-function "torch_rms_norm")
;; [TODO] Fuse in a single kernel (var/std)
(deftest test-variance
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(30 30))))
      (assert-equal
          (:atol 1e-5 :rtol 1e-6)
          (with-torch (x) (->caten (torch.var x :axis -1 :keepdims t :correction 1)))
          (proceed (!variance x :axis -1 :correction 1))))))

(deftest test-std
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(30 30))))
      (assert-equal
          (:atol 1e-5 :rtol 1e-6)
          (with-torch (x) (->caten (torch.std x :axis -1 :keepdims t :correction 1)))
          (proceed (!std x :axis -1 :correction 1))))))

(deftest test-batch-norm
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(40 40))))
      (assert-equal
          (:atol 1e-3 :rtol 1e-4)
          (with-torch (x) (->caten (f:batch_norm x (torch.ones `(40)) (torch.ones `(40)))))
          (proceed (!batch-norm x nil nil (linspace `(40 40) 0 1) (linspace `(40 40) 0 1)))))))

(deftest test-batch-norm-1
  (when (= 1 (ctx:getenv :JIT))
    (let* ((jit (proceed (!batch-norm (ax+b `(10 10) 0.01 0.01))))
           (vm  (ctx:with-contextvar (:JIT 0) (proceed (!batch-norm (ax+b `(10 10) 0.01 0.01))))))
      (assert-equal (:atol 1e-3 :rtol 1e-3) jit vm))))

(deftest test-layer-norm
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (randn `(30 40))))
      (assert-equal
          (:atol 1e-4 :rtol 1e-6)
          (with-torch (x) (->caten (f:layer_norm x `(40) :eps 1e-5)))
          (proceed (!layer-norm x `(40) :eps 1e-5))))))

(deftest test-rms-norm
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(30 40))))
      (assert-equal
          (:atol 1e-3 :rtol 1e-6)
          (with-torch (x) (->caten (torch_rms_norm x)))
          (proceed (!rms-norm x `(40) :eps 1e-5))))))

(deftest test-layer-norm-affine-bias
  (with-given-dtype ((:float32 . "float32"))
    (let* ((x (make-tensor `(10 s 32) :from 'x))
           (affine (rand `(32)))
           (bias   (rand `(32)))
           (model (caten (!layer-norm x `(32) :weight affine :bias bias))))
      (loop for s upfrom 3 below 7
            for x = (randn `(10 ,s 32)) do
              (assert-equal
                  (:atol 1e-4 :rtol 1e-5)
                  (with-torch (x affine bias)
                    (->caten (f:layer_norm x `(32) :weight affine :bias bias)))
                  (let ((x (forward model `(x . ,x) `(s . ,s))))
                    (setf (nth 1 (tensor-shape x)) s)
                    x))))))
