(in-package :caten/test-suite)

(python-exec
 "
def torch_attn_fail_1(x, n_heads, c_attn_weight, c_attn_bias, c_proj_weight, c_proj_bias):
  xqkv = torch.matmul(x, c_attn_weight.T) + c_attn_bias
  batch_size, seq_len, _ = x.size()
  new_x_shape = (batch_size, seq_len, n_heads, x.size(-1) // n_heads)
  xq, xk, xv = xqkv.chunk(3, dim=2)
  xq, xk, xv = xq.reshape(new_x_shape), xk.reshape(new_x_shape), xv.reshape(new_x_shape)
  xq, xk, xv = xq.transpose(1, 2), xk.transpose(1, 2), xv.transpose(1, 2)
  attn_output = test_scaled_dot_product_attention(xq, xk, xv)
  attn_output = attn_output.permute(0, 2, 1, 3)
  attn_output = attn_output.reshape(batch_size, seq_len, -1)
  attn_output = torch.matmul(attn_output, c_proj_weight.T) + c_proj_bias
  return attn_output
")

(import-function "torch_attn_fail_1")

(defun attn-fail-1 (x n-heads c_attn.weight c_attn.bias c_proj.weight c_proj.bias)
  (let ((xqkv (!add (!matmul x (!t c_attn.weight)) c_attn.bias)))
    (multiple-value-bind (xq xk xv) (!chunk xqkv 3 :dim 2)
      (let* ((new-x-shape (append (butlast (shape xq)) (list n-heads (/ (car (last (shape xq))) n-heads))))
             (xq (!reshape xq new-x-shape))
             (xk (!reshape xk new-x-shape))
             (xv (!reshape xv new-x-shape))
             ;; No KV_Cache
             (xq (!transpose xq 1 2))
             (xk (!transpose xk 1 2))
             (xv (!transpose xv 1 2))
             (attn-output (scaled-dot-product-attention xq xk xv))
             (attn-output (!permute attn-output 0 2 1 3))
             (attn-output (!reshape attn-output (append (butlast (shape attn-output) 2) (list (apply #'* (last (shape attn-output) 2)))))))
        (!add (!matmul attn-output (!t c_proj.weight)) c_proj.bias)))))

(deftest test-attn-fail-1
  (let* ((dim 32)
         (n-heads 8)
         (batch-size 10)
         (seq-len 32)
         (x (randn `(,batch-size ,seq-len ,dim)))
         (c_attn.weight  (normal `(,(* 3 dim) ,dim) :mean 0.1 :std 1.1))
         (c_attn.bias    (normal`(,(* 3 dim))  :mean 0.1 :std 1.1))
         (c_procj.weight (normal `(,dim ,dim) :mean 0.1 :std 1.1))
         (c_procj.bias   (normal `(,dim) :mean 0.1 :std 1.1)))
    (assert-equal
        (:rtol 1e-5 :atol 1e-5) ;; TODO: Rtol in 1e-5
        (with-torch (x c_attn.weight c_attn.bias c_procj.weight c_procj.bias)
          (->caten (attn_impl_torch x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))
        (proceed (attn-impl x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))))
