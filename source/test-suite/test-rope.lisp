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

(import-function "torch_rope")

(deftest test-rope
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (with-manual-seed (0) (rand `(1 10 10 10)))))
      (assert-equal
          (:atol 1e-5 :rtol 1e-5) ;; fixme atol looks unstable?
          (with-torch (x) (->caten (torch_rope x)))
          (proceed (!rope x 5))))))
