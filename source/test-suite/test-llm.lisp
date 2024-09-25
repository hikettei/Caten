(in-package :caten/test-suite)

;; Official implementation of scaled_dot_product_attention
;; taken from https://pytorch.org/docs/stable/generated/torch.nn.functional.scaled_dot_product_attention.html
;; Make sure that not to use FlashAttention2
(python-exec
 "
# Efficient implementation equivalent to the following:
import math
def test_scaled_dot_product_attention(query, key, value) -> torch.Tensor:
    qk = torch.matmul(query, key.transpose(-2, -1))
    scale_factor = 1 / math.sqrt(query.size(-1))
    qk = qk * scale_factor
    attn_weight = torch.softmax(qk, dim=-1)
    return torch.matmul(attn_weight, value)
")

(import-function "test_scaled_dot_product_attention")

;; Neural Ops used in implementing LLM
;; [TODO] If batched+JIT, wont work
(deftest test-scaled-dot-product-attention
  (with-given-dtype ((:float32 . "float32"))
    (let ((q (rand `(4 8 8)))
	  (k (rand `(4 8 8)))
	  (v (rand `(4 8 8))))
      (assert-equal
	  (:atol 1e-5 :rtol 1e-5)
	  (with-torch (q k v)
	    (->caten (test_scaled_dot_product_attention q k v)))
	  (proceed (scaled-dot-product-attention q k v))))))

(deftest test-scaled-dot-product-attention-batched
  (with-given-dtype ((:float32 . "float32"))
    (let ((q (rand `(4 4 8 8)))
	  (k (rand `(4 4 8 8)))
	  (v (rand `(4 4 8 8))))
      (assert-equal
	  (:atol 1e-5 :rtol 1e-5)
	  (with-torch (q k v)
	    (->caten (test_scaled_dot_product_attention q k v)))
	  (proceed (scaled-dot-product-attention q k v))))))

(deftest test-softmax-pytorch
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(32 32))))
      (assert-equal
	  (:atol 1e-5 :rtol 1e-3)
	  (with-torch (x)
	    (->caten (f:softmax x)))
	  (proceed (!softmax x))))))
