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

(deftest test-scaled-dot-product-attention
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let ((q (rand `(4 8 8)))
	    (k (rand `(4 8 8)))
	    (v (rand `(4 8 8))))
        (assert-equal
	    (:atol 1e-5 :rtol 1e-5)
	    (with-torch (q k v)
	      (->caten (test_scaled_dot_product_attention q k v)))
	    (proceed (scaled-dot-product-attention q k v)))))))

(deftest test-scaled-dot-product-attention-batched
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let ((q (rand `(4 4 8 8)))
	    (k (rand `(4 4 8 8)))
	    (v (rand `(4 4 8 8))))
        (assert-equal
	    (:atol 1e-5 :rtol 1e-5)
	    (with-torch (q k v)
	      (->caten (test_scaled_dot_product_attention q k v)))
	    (proceed (scaled-dot-product-attention q k v)))))))

(deftest test-softmax-pytorch
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (rand `(32 32))))
      (assert-equal
	  (:atol 1e-5 :rtol 1e-3)
	  (with-torch (x)
	    (->caten (f:softmax x)))
	  (proceed (!softmax x))))))

(deftest softmax-matmul
  (let ((a (rand `(128 128)))
        (b (rand `(128 128)))
        (c (rand `(128 128))))
    (with-no-grad
      (assert-equal
          (:atol 1e-5 :rtol 1e-5)
          (with-torch (a b c)
            (->caten (f:softmax (torch.matmul (f:softmax c) (f:softmax (torch.matmul (f:softmax a) (f:softmax b)))))))
          (proceed (!softmax (!matmul (!softmax c) (!softmax (!matmul (!softmax a) (!softmax b))))))))))

(deftest reshape-permute
  (let ((x (rand `(4 8 8))))
    (with-no-grad
      (assert-equal
          (:atol 1e-5 :rtol 1e-5)
          (with-torch (x)
            (->caten (!permute (!reshape x `(4 8 8)) 1 0 2)))
          (proceed (!permute (!reshape x `(4 8 8)) 1 0 2))))))

(defun mha-parameters (dim)
  (let ((c-attn-weight (rand `(,(* 3 dim) ,dim)))
        (c-attn-bias   (rand `(,(* 3 dim))))
        (c-proj-weight (rand `(,dim ,dim)))
        (c-proj-bias   (rand `(,dim))))
    (values c-attn-weight c-attn-bias c-proj-weight c-proj-bias)))

(python-exec "
def mha_scaled_dot_product_attention(query, key, value, mask) -> torch.Tensor:
    # Compute scaled dot-product attention
    qk = torch.matmul(query, key.transpose(-2, -1))
    scale_factor = 1 / math.sqrt(query.size(-1))
    qk = qk * scale_factor
    qk = qk + mask
    attn_weights = torch.softmax(qk, dim=-1)
    return torch.matmul(attn_weights, value)
    
def torch_mha_impl(n, dim, n_heads, input, c_attn_weight, c_attn_bias, c_proj_weight, c_proj_bias):
    # Combine queries, keys, and values into one matrix multiplication for efficiency
    xqkv = torch.matmul(input, c_attn_weight.T) + c_attn_bias
    batch_size, seq_len, _ = input.size()
    head_dim = dim // n_heads
    mask = torch.triu(torch.full((1, 1, seq_len, seq_len), float('-inf')), diagonal=1+n)
    # Reshape and split the combined QKV tensor
    xqkv = xqkv.view(batch_size, seq_len, n_heads, 3, head_dim)
    xqkv = xqkv.permute(3, 0, 2, 1, 4)  # Rearrange dimensions for splitting
    xq, xk, xv = xqkv.chunk(3)
    # Compute attention output
    attn_output = mha_scaled_dot_product_attention(xq, xk, xv, mask)
    # Concatenate and reshape the attention output
    attn_output = attn_output.transpose(1, 2).contiguous().view(batch_size, seq_len, dim)
    # Apply the final linear projection
    return torch.matmul(attn_output, c_proj_weight.T) + c_proj_bias
")
(import-function "torch_mha_impl")
;; Permute does n o t h i n g w t f
(defun mha-impl (n dim n-heads input c-attn-weight c-attn-bias c-proj-weight c-proj-bias)
  (let* ((xqkv (!add (!matmul input (!t c-attn-weight)) c-attn-bias))
         (batch-size (car (shape input)))
         (head-dim (/ dim n-heads))
         (seq-len (second (shape input)))
         (mask (!triu (!full `(1 1 ,seq-len ,seq-len) (-inf)) :diagonal (!+ (iconst 1) (iconst n))))
         (xqkv (!reshape xqkv `(,batch-size ,seq-len ,n-heads 3 ,head-dim)))
         (xqkv (!permute xqkv 3 0 2 1 4)))
    (multiple-value-bind (xq xk xv) (!chunk xqkv 3 :dim 0)
      (let* ((attn-output (scaled-dot-product-attention xq xk xv mask))
             (attn-output (!reshape (!transpose attn-output 1 2) `(,batch-size ,seq-len ,dim))))
        (!add (!matmul attn-output (!t c-proj-weight)) c-proj-bias)))))
;; PERMUTE DOES N O T H I N G T _ T
(deftest test-multihead-attention
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let* ((dim 128) (n-heads 8) (batch-size 4) (seq-len 64) (n 1)
             (x (rand `(,batch-size ,seq-len ,dim))))
        (multiple-value-bind (c-attn-weight c-attn-bias c-proj-weight c-proj-bias) (mha-parameters dim)
          (assert-equal
              (:atol 1e-5 :rtol 1e-4)
              (with-torch (x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)
                (->caten (torch_mha_impl n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)))
              (proceed (mha-impl n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias))))))))

(deftest test-mha-1
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let* ((dim 8) (n-heads 1) (batch-size 1) (seq-len 3) (n 1)
             (x (rand `(,batch-size ,seq-len ,dim))))
        (multiple-value-bind (c-attn-weight c-attn-bias c-proj-weight c-proj-bias) (mha-parameters dim)
          (assert-equal
              (:atol 1e-5 :rtol 1e-4)
              (with-torch (x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)
                (->caten (torch_mha_impl n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)))
              (proceed (mha-impl n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias))))))))
;; Complete High Level ASM Level Problem
;; Failing Case
(python-exec
 "
def chunk_fail_case_1(n, dim, n_heads, input, c_attn_weight, c_attn_bias, c_proj_weight, c_proj_bias):
    # Combine queries, keys, and values into one matrix multiplication for efficiency
    xqkv = torch.matmul(input, c_attn_weight.T) + c_attn_bias
    batch_size, seq_len, _ = input.size()
    head_dim = dim // n_heads
    mask = torch.triu(torch.full((1, 1, seq_len, seq_len), float('-inf')), diagonal=1+n)
    # Reshape and split the combined QKV tensor
    xqkv = xqkv.view(batch_size, seq_len, n_heads, 3, head_dim)
    xqkv = xqkv.permute(3, 0, 2, 1, 4)  # Rearrange dimensions for splitting
    xq, xk, xv = xqkv.chunk(3)
    return xq+xk+xv
")

(import-function "chunk_fail_case_1")

(deftest chunk-fail-case-1
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let* ((dim 8) (n-heads 1) (batch-size 1) (seq-len 3) (n 1)
             (x (rand `(,batch-size ,seq-len ,dim))))
        (multiple-value-bind (c-attn-weight c-attn-bias c-proj-weight c-proj-bias) (mha-parameters dim)
          (assert-equal
              (:atol 1e-5 :rtol 1e-5)
              (with-torch (x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)
                (print (->caten (mha_fail_1 n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias))))
              (let* ((xqkv (!add (!matmul x (!t c-attn-weight)) c-attn-bias))
                     (batch-size (car (shape x)))
                     (head-dim (/ dim n-heads))
                     (seq-len (second (shape x)))
                     ;; (mask (!triu (!full `(1 1 ,seq-len ,seq-len) (-inf)) :diagonal (!+ (iconst 1) (iconst n))))
                     (xqkv (!reshape xqkv `(,batch-size ,seq-len ,n-heads 3 ,head-dim)))
                     (xqkv (!permute xqkv 3 0 2 1 4)))
                (multiple-value-bind (xq xk xv) (!chunk xqkv 3 :dim 0)
                  (proceed (!contiguous (!+ xq xk xv)))))))))))
