(in-package :caten/nn)

; from https://github.com/ml-explore/mlx/blob/main/python/mlx/nn/layers/positional_encoding.py
;; RoPE
;; PositionalEncoding

(defmodel (RoPE (dims &key (traditional NIL) (base 10000) (scale 1.0) (offset 1.0)))
    ((dims dims)
     (traditional traditional)
     (base base)
     (scale scale)
     (offset offset)))

(defmethod call ((op RoPE) &rest inputs)
  (let* ((x (car inputs))
         (shape (shape x))
         (last-two (last shape 2))
         (n (first last-two))
         (d (second last-two))
         (b (reduce #'* (butlast shape 2)))
         (x (!reshape x (list b n d)))
         (positions (!index-components (list n)))
         ; TODO: handle potential divisions by 0
         (freqs (!exp (!div (!index-components (list (floor d 2)))
                            (!const x (log (- (floor 10000 d) 1))))))
         (positions-reshaped (!reshape positions (list n 1)))  ; (N,1)
         (freqs-reshaped (!reshape freqs (list 1 (floor d 2))))  ; (1,D/2)
         (theta (!mul positions-reshaped freqs-reshaped))  ; (N,D/2)
         (costheta (!cos theta))
         (sintheta (!sin theta))
         (x1 (!view x t t `(0 ,d 2)))
         (x2 (!view x t t `(1 ,d 2)))
         (rx1 (!sub (!mul x1 costheta) (!mul x2 sintheta)))
         (rx2 (!add (!mul x1 sintheta) (!mul x2 costheta)))
         (rx1-expanded (!reshape rx1 (append (shape rx1) (list 1))))
         (rx2-expanded (!reshape rx2 (append (shape rx2) (list 1))))
         (result (!concatenate -1 rx1-expanded rx2-expanded))
         (final-result (!reshape result (list b n d))))
    (proceed final-result))))


(defun !rope (x)
(declare (type tensor x))
(forward (RoPE 1) x))

(in-package :caten/test-suite)

(python-exec
"
#from: https://pytorch.org/torchtune/0.2/_modules/torchtune/modules/position_embeddings.html
def torch_rope(x):
    base = 10000
    dim = x.shape[-1]
    theta = 1.0 / (base ** (torch.arange(0, dim, 2, device=x.device).float() / dim))
    seq_len = x.size(1)
    seq_idx = torch.arange(seq_len, dtype=theta.dtype, device=theta.device)
    idx_theta = torch.einsum('i,j->ij', seq_idx, theta)
    cache = torch.stack([torch.cos(idx_theta), torch.sin(idx_theta)], dim=-1)
    x_shaped = x.float().reshape(*x.shape[:-1], -1, 2)
    rope_cache = cache.view(1, seq_len, 1, x_shaped.size(-2), 2)
    x_out = torch.stack([
        x_shaped[..., 0] * rope_cache[..., 0] - x_shaped[..., 1] * rope_cache[..., 1],
        x_shaped[..., 1] * rope_cache[..., 0] + x_shaped[..., 0] * rope_cache[..., 1],
    ], dim=-1)
    x_out = x_out.flatten(-2)
    return x_out.type_as(x)")


(deftest test-rope
(with-given-dtype ((:float32 . "float32"))
  (let ((x (rand `(30 30))))
    (assert-equal
     (:atol 1e-5 :rtol 1e-6)
     (with-torch (x) (->caten (torch_rope x)))
     (proceed (!rope x))))))


