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
      ;; nedd to add chunk test
      (let* ((new-x-shape (append (butlast (shape xq)) (list n-heads (/ (car (last (shape xq))) n-heads))))
             (xq (!reshape xq new-x-shape))
             (xk (!reshape xk new-x-shape))
             (xv (!reshape xv new-x-shape))
             ;; (_ (return-from attn-fail-1 (!+ xq xk xv))) failing point
             ;; No KV_Cache
             (xq (!transpose xq 1 2))
             (xk (!transpose xk 1 2))
             (xv (!transpose xv 1 2))
             (attn-output (scaled-dot-product-attention xq xk xv))
             (attn-output (!permute attn-output 0 2 1 3))
             (attn-output (!reshape attn-output (append (butlast (shape attn-output) 2) (list (apply #'* (last (shape attn-output) 2)))))))
        (!add (!matmul attn-output (!t c_proj.weight)) c_proj.bias)))))
;; [TODO] Using linspace!!!!
(deftest test-attn-fail-1
  (let* ((dim 32)
         (n-heads 8)
         (batch-size 10)
         (seq-len 32)
         (x (linspace `(,batch-size ,seq-len ,dim) 0.1 0.0))
         (c_attn.weight  (linspace `(,(* 3 dim) ,dim) 0.01 0.0))
         (c_attn.bias    (linspace `(,(* 3 dim)) 0.01 0.0))
         (c_procj.weight (linspace `(,dim ,dim) 0.01 0.0))
         (c_procj.bias   (linspace  `(,dim) 0.01 0.0)))
    (assert-equal
        (:rtol 1e-5 :atol 1e-5) ;; TODO: Rtol in 1e-5
        (with-torch (x c_attn.weight c_attn.bias c_procj.weight c_procj.bias)
          (->caten (torch_attn_fail_1 x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))
        (proceed (attn-fail-1 x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))))

(python-exec
 "
def torch_chunk_fail(x, w, b):
  x = (x @ w.T) + b
  x1, x2, x3 = x.chunk(3, dim=2)
  return x2")
(import-function "torch_chunk_fail")

(deftest chunk-fail-test
  (let ((x (linspace `(10 10 25) 1 0))
        (w (linspace `(36 25) 1 0))
        (b (linspace `(36) 1 0)))
    (assert-equal
        (:rtol 1e-5 :atol 1e-5)
        (with-torch (x w b)
          (print (->caten (torch_chunk_fail x w b))))
        (multiple-value-bind (x1 x2 x3) (!chunk (!add (!matmul x (!t w)) b) 3 :dim 2)
          (print (proceed (!contiguous (!+ x2))))))))
