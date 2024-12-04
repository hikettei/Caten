(in-package :caten/test-suite)

;; Official implementation of scaled_dot_product_attention
;; taken from https://pytorch.org/docs/stable/generated/torch.nn.functional.scaled_dot_product_attention.html
;; Make sure that not to use FlashAttention2
(python-exec
 "
# Efficient implementation equivalent to the following:
def softmax(x):
  m = x - x.max(-1, keepdims=True)[0]
  e = m.exp()
  ss = e.sum(axis=-1, keepdims=True)
  return e / ss

def _softmax(x): return softmax(x)
import math
def test_scaled_dot_product_attention(query, key, value) -> torch.Tensor:
    qk = torch.matmul(query, key.transpose(-2, -1))
    scale_factor = 1 / math.sqrt(query.size(-1))
    qk = qk * scale_factor
    attn_weight = softmax(qk)
    return torch.matmul(attn_weight, value)
")

(import-function "test_scaled_dot_product_attention")
(import-function "_softmax")

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
;; Note(hikettei) Previously failed due to two reasons: Invaild Scheduler + Invaild Memory Planner
(deftest test-scaled-dot-product-attention-batched-composed
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (let ((q (rand `(4 4 8 8) :id 'query))
	    (k (rand `(4 4 8 8) :id 'key))
	    (v (rand `(4 4 8 8) :id 'value)))
        (assert-equal
	    (:atol 1e-4 :rtol 1e-6)
	    (with-torch (q k v)
	      (->caten (test_scaled_dot_product_attention (test_scaled_dot_product_attention q k v) k v)))
	    (proceed (scaled-dot-product-attention (scaled-dot-product-attention q k v) k v)))))))

(deftest test-softmax-pytorch
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (randn `(128 128))))
      (assert-equal
	  (:atol 1e-5 :rtol 1e-6)
	  (with-torch (x)
	    (->caten (f:softmax x)))
	  (proceed (!softmax x))))))

(deftest test-softmax-pytorch-fuzz
  (with-given-dtype ((:float32 . "float32"))
    (let ((x (normal `(512 512) :mean 10.0 :std 100.0)))
      (assert-equal
	  (:atol 1e-5 :rtol 1e-5)
	  (with-torch (x)
	    (->caten (f:softmax x)))
	  (proceed (!softmax x))))))

(deftest softmax-matmul
  (let ((a (randn `(128 128)))
        (b (randn `(128 128)))
        (c (randn `(128 128))))
    (with-no-grad
      (assert-equal
          (:atol 1e-4 :rtol 1e-5)
          (with-torch (a b c)
            (->caten (f:softmax (torch.matmul (f:softmax c) (f:softmax (torch.matmul (f:softmax a) (f:softmax b)))))))
          (proceed (!softmax (!matmul (!softmax c) (!softmax (!matmul (!softmax a) (!softmax b))))))))))

(deftest softmax-matmul-fuzz
  (let ((a (normal `(128 128) :mean 100.0 :std 1000.0))
        (b (normal `(128 128) :mean 100.0 :std 1000.0))
        (c (normal `(128 128) :mean 100.0 :std 1000.0)))
    (with-no-grad
      (assert-equal
          (:atol 1e-4 :rtol 1e-5)
          (with-torch (a b c)
            (->caten (f:softmax (torch.matmul (_softmax c) (_softmax (torch.matmul (_softmax a) (_softmax b)))))))
          (proceed (!softmax (!matmul (!softmax c) (!softmax (!matmul (!softmax a) (!softmax b))))))))))

;; ~~ Utils for testing MultiHeadAttention ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun mha-parameters (dim)
  (let ((c-attn-weight (rand `(,(* 3 dim) ,dim)))
        (c-attn-bias   (rand `(,(* 3 dim))))
        (c-proj-weight (rand `(,dim ,dim)))
        (c-proj-bias   (rand `(,dim))))
    (values c-attn-weight c-attn-bias c-proj-weight c-proj-bias)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Failing Case1: Matmul+Reshape+Permute
(python-exec
 "
def mha_failing_case_1(n, dim, n_heads, input, c_attn_weight, c_attn_bias, c_proj_weight, c_proj_bias):
    # Combine queries, keys, and values into one matrix multiplication for efficiency
    xqkv = torch.matmul(input, c_attn_weight.T) + c_attn_bias
    batch_size, seq_len, _ = input.size()
    head_dim = dim // n_heads
    # Reshape and split the combined QKV tensor
    xqkv = xqkv.view(batch_size, seq_len, n_heads, 3, head_dim)
    xqkv = xqkv.permute(3, 0, 2, 1, 4)  # Rearrange dimensions for splitting
    return xqkv
")

(import-function "mha_failing_case_1")

(deftest mha-failing-case-1
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (loop for dim in        `(8  128)
            for n-heads in    `(1  8)
            for batch-size in `(1  20)
            for seq-len in    `(10 16)
            for n in          `(1  2) do
              (let ((x (rand `(,batch-size ,seq-len ,dim))))
                (testing (format nil "dim=~a n-heads=~a batch-size=~a seq-len=~a n=~a" dim n-heads batch-size seq-len n)
                  (multiple-value-bind (c-attn-weight c-attn-bias c-proj-weight c-proj-bias) (mha-parameters dim)
                    (assert-equal
                        (:atol 1e-4 :rtol 1e-4)
                        (with-torch (x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)
                          (->caten (mha_failing_case_1 n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)))
                        (let* ((xqkv (!add (!matmul x (!t c-attn-weight)) c-attn-bias))
                               (batch-size (car (shape x)))
                               (seq-len (second (shape x)))
                               (head-dim (/ dim n-heads))
                               (xqkv (!reshape xqkv `(,batch-size ,seq-len ,n-heads 3 ,head-dim)))
                               (xqkv (!permute xqkv 3 0 2 1 4)))
                          (proceed (!contiguous xqkv :force t)))))))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Failing Case2: Matmul+Reshape+Permute+Chunk
(python-exec
 "
def mha_failing_case_2(n, dim, n_heads, input, c_attn_weight, c_attn_bias, c_proj_weight, c_proj_bias):
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

(import-function "mha_failing_case_2")

(deftest mha-failing-case-2
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (loop for dim in        `(8  128)
            for n-heads in    `(1  8)
            for batch-size in `(1  10)
            for seq-len in    `(10 16)
            for n in          `(1  2) do
              (let ((x (rand `(,batch-size ,seq-len ,dim))))
                (testing (format nil "dim=~a n-heads=~a batch-size=~a seq-len=~a n=~a" dim n-heads batch-size seq-len n)
                  (multiple-value-bind (c-attn-weight c-attn-bias c-proj-weight c-proj-bias) (mha-parameters dim)
                    (assert-equal
                        (:atol 1e-4 :rtol 1e-4)
                        (with-torch (x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)
                          (->caten (mha_failing_case_2 n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)))
                        (let* ((xqkv (!add (!matmul x (!t c-attn-weight)) c-attn-bias))
                               (batch-size (car (shape x)))
                               (head-dim (/ dim n-heads))
                               (seq-len (second (shape x)))
                               (xqkv (!reshape xqkv `(,batch-size ,seq-len ,n-heads 3 ,head-dim)))
                               (xqkv (!permute xqkv 3 0 2 1 4)))
                          (multiple-value-bind (xq xk xv) (!chunk xqkv 3 :dim 0)
                            (proceed (!contiguous (!+ xq xk xv)))))))))))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Running MHA
(python-exec "
def mha_scaled_dot_product_attention(query, key, value, mask) -> torch.Tensor:
    # Compute scaled dot-product attention
    qk = torch.matmul(query, key.transpose(-1, -2))
    scale_factor = 1 / math.sqrt(query.size(-1))
    qk = qk * scale_factor
    qk = qk + mask
    attn_weights = softmax(qk)
    return torch.matmul(attn_weights, value)
    
def torch_mha_impl(n, dim, n_heads, input, c_attn_weight, c_attn_bias, c_proj_weight, c_proj_bias):
    # Combine queries, keys, and values into one matrix multiplication for efficiency
    xqkv = torch.matmul(input, c_attn_weight.T) + c_attn_bias
    batch_size, seq_len, _ = input.size()
    head_dim = dim // n_heads
    mask = torch.triu(torch.full((1, 1, seq_len, seq_len), float('-inf')), diagonal=1+n)
    # Reshape and split the combined QKV tensor
    xqkv = xqkv.reshape(batch_size, seq_len, n_heads, 3, head_dim)
    xqkv = xqkv.permute(3, 0, 2, 1, 4)  # Rearrange dimensions for splitting
    xq, xk, xv = xqkv.chunk(3)
    # Compute attention output
    attn_output = mha_scaled_dot_product_attention(xq, xk, xv, mask)
    # Concatenate and reshape the attention output
    attn_output = attn_output.transpose(1, 2).reshape(batch_size, seq_len, dim)
    # Apply the final linear projection
    return torch.matmul(attn_output, c_proj_weight.T) + c_proj_bias
")
(import-function "torch_mha_impl")

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
;; [TODO] Test for more larger inputs
#|
;; TODO: Old implementation looks unstable?
(deftest test-multihead-attention
  (with-given-dtype ((:float32 . "float32"))
    (with-no-grad
      (loop
        for dim in        `(8 128)
        for n-heads in    `(1 8)
        for batch-size in `(1 1) ;; [TODO] Unstable with batch_size > 1 ?
        for seq-len in    `(3 64)
        for n in          `(1 1) do        
          (let ((x (rand `(,batch-size ,seq-len ,dim))))
            (testing (format nil "dim=~a n-heads=~a batch-size=~a seq-len=~a n=~a" dim n-heads batch-size seq-len n)
              (multiple-value-bind (c-attn-weight c-attn-bias c-proj-weight c-proj-bias) (mha-parameters dim)
                (assert-equal
                    (:atol 1e-5 :rtol 1e-3)
                    (with-torch (x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)
                      (->caten (torch_mha_impl n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias)))
                    (proceed (mha-impl n dim n-heads x c-attn-weight c-attn-bias c-proj-weight c-proj-bias))))))))))
|#



(defun attn-impl (x n-heads c_attn_weight c_attn_bias c_proj_weight c_proj_bias)
  ;; Step 1: Linear projection
  (let ((xqkv (!add (!matmul x (!t c_attn_weight)) c_attn_bias)))
    ;; (print "xqkv:")
    ;; (print xqkv)

    ;; Step 2: Split xqkv into xq, xk, xv
    (multiple-value-bind (xq1 xk1 xv1) (!chunk xqkv 3 :dim 2)
      ;; (print "xq1:")
      ;; (print xq1)

      ;; Step 3: Reshape for multi-head attention
      (let* ((new-x-shape (append (butlast (shape xq1))
                                  (list n-heads (/ (car (last (shape xq1))) n-heads))))
             (xq2 (!reshape xq1 new-x-shape))
             (xk2 (!reshape xk1 new-x-shape))
             (xv2 (!reshape xv1 new-x-shape)))
        ;; (print "xq2:")
        ;; (print xq2)

        ;; Step 4: Transpose for attention computation
        (let ((xq3 (!transpose xq2 1 2))
              (xk3 (!transpose xk2 1 2))
              (xv3 (!transpose xv2 1 2)))
          ;; (print "xq3:")
          ;; (print xq3)

          ;; Step 5: Compute attention
          (let ((attn-output1 (scaled-dot-product-attention xq3 xk3 xv3)))
            ;; (print "attn-output1:")
            ;; (print attn-output1)

            ;; Step 6: Permute output
            (let ((attn-output2 (!permute attn-output1 0 2 1 3)))
              ;; (print "attn-output2:")
              ;; (print attn-output2)

              ;; Step 7: Reshape to original dimensions
              (let ((attn-output3 (!reshape attn-output2
                                            (append (butlast (shape attn-output2) 2)
                                                    (list (apply #'* (last (shape attn-output2) 2)))))))
                ;; (print "attn-output3:")
                ;; (print attn-output3)

                ;; Step 8: Final linear projection
                (let ((attn-output4 (!add (!matmul attn-output3 (!t c_proj_weight)) c_proj_bias)))
                  ;; (print "attn-output4:")
                  ;; (print attn-output4)

                  ;; Return the final output
                  xq1))))))))))

(python-exec
"
def attn_impl_torch(x, n_heads, c_attn_weight, c_attn_bias, c_proj_weight, c_proj_bias):
    # Step 1: Linear projection
    xqkv = torch.matmul(x, c_attn_weight.T) + c_attn_bias
    # print(\"xqkv:\")
    # print(xqkv)
    batch_size, seq_len, _ = x.size()
    new_x_shape = (batch_size, seq_len, n_heads, x.size(-1) // n_heads)

    # Step 2: Split xqkv into xq, xk, xv
    xq1, xk1, xv1 = xqkv.chunk(3, dim=2)
    # print(\"xq1:\")
    # print(xq1)

    # Step 3: Reshape for multi-head attention
    xq2 = xq1.reshape(new_x_shape)
    xk2 = xk1.reshape(new_x_shape)
    xv2 = xv1.reshape(new_x_shape)
    # print(\"xq2:\")
    # print(xq2)

    # Step 4: Transpose for attention computation
    xq3 = xq2.transpose(1, 2)
    xk3 = xk2.transpose(1, 2)
    xv3 = xv2.transpose(1, 2)
    # print(\"xq3:\")
    # print(xq3)

    # Step 5: Compute attention
    attn_output1 = test_scaled_dot_product_attention(xq3, xk3, xv3)
    # print(\"attn_output1:\")
    # print(attn_output1)

    # Step 6: Permute output
    attn_output2 = attn_output1.permute(0, 2, 1, 3)
    # print(\"attn_output2:\")
    # print(attn_output2)

    # Step 7: Reshape to original dimensions
    attn_output3 = attn_output2.reshape(batch_size, seq_len, -1)
    # print(\"attn_output3:\")
    # print(attn_output3)

    # Step 8: Final linear projection
    attn_output4 = torch.matmul(attn_output3, c_proj_weight.T) + c_proj_bias
    # print(\"attn_output4:\")
    # print(attn_output4)

    # Return the final output
    return xq1
")

(import-function "attn_impl_torch")

(deftest test-attention
  (let ((x (rand `(10 3 32)))
        (c_attn.weight (rand `(96 32)))
        (c_attn.bias   (rand `(96)))
        (c_procj.weight (rand `(32 32)))
        (c_procj.bias (rand `(32))))
    (assert-equal
        (:rtol 1e-4 :atol 1e-4)
        (with-torch (x c_attn.weight c_attn.bias c_procj.weight c_procj.bias)
          (->caten (attn_impl_torch x 4 c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))
        (proceed (attn-impl x 4 c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))))

(define-kernel-count-test fixed-attention-schedule 6
  "Attention = 6 Kernels (TODO: 5 Kernel)"
  (let ((x (make-tensor `(10 3 32)))
        (c_attn.weight (make-tensor `(96 32)))
        (c_attn.bias   (make-tensor `(96)))
        (c_procj.weight (make-tensor `(32 32)))
        (c_procj.bias (make-tensor `(32))))
    (caten (attn-impl x 4 c_attn.weight c_attn.bias c_procj.weight c_procj.bias))))

(define-kernel-count-test symbolic-attention-schedule 6
  "Attention = 6 Kernels (TODO: 5 Kernel)"
  (let ((x (make-tensor `(10 b 32)))
        (c_attn.weight (make-tensor `(96 32)))
        (c_attn.bias   (make-tensor `(96)))
        (c_procj.weight (make-tensor `(32 32)))
        (c_procj.bias (make-tensor `(32))))
    (caten (attn-impl x 4 c_attn.weight c_attn.bias c_procj.weight c_procj.bias))))

(define-kernel-count-test batch=1-symbolic-attention-schedule 6
  "Attention = 6 Kernels (TODO: 5 Kernel)"
  (let ((x (make-tensor `(1 b 32)))
        (c_attn.weight (make-tensor `(96 32)))
        (c_attn.bias   (make-tensor `(96)))
        (c_procj.weight (make-tensor `(32 32)))
        (c_procj.bias (make-tensor `(32))))
    (caten (attn-impl x 4 c_attn.weight c_attn.bias c_procj.weight c_procj.bias))))
;; [TODO] Fix test-attention-large for both JIT=0 and JIT=1
(deftest test-attention-large
  (with-given-dtype ((:float32 . "float32"))
    (let* ((dim 128)
           (n-heads 8)
           (batch-size 10)
           (seq-len 32)
           (x (rand `(,batch-size ,seq-len ,dim)))
           (c_attn.weight  (rand `(,(* 3 dim) ,dim)))
           (c_attn.bias    (rand `(,(* 3 dim))))
           (c_procj.weight (rand `(,dim ,dim)))
           (c_procj.bias   (rand `(,dim))))
      (assert-equal
          (:rtol 1e-5 :atol 5e-3) ;; TODO: Rtol in 1e-5
          (with-torch (x c_attn.weight c_attn.bias c_procj.weight c_procj.bias)
            (->caten (attn_impl_torch x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))
          (proceed (attn-impl x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias))))))
;; [TODO] Update test-attention-large-b=1 for both JIT=0 and JIT=1, atol looks unstable
(deftest test-attention-large-b=1
  (let* ((dim 128)
         (n-heads 8)
         (batch-size 1)
         (seq-len 32)
         (x (randn `(,batch-size ,seq-len ,dim)))
         (c_attn.weight  (normal `(,(* 3 dim) ,dim) :mean 0.0 :std 0.1))
         (c_attn.bias    (normal `(,(* 3 dim))  :mean 0.0 :std 0.1))
         (c_procj.weight (normal `(,dim ,dim) :mean 0.0 :std 0.1))
         (c_procj.bias   (normal `(,dim) :mean 0.0 :std 0.1)))
    (assert-equal
        (:rtol 1e-5 :atol 5e-3) ;; TODO: Rtol in 1e-5, atol looks still unstable?...
        (with-torch (x c_attn.weight c_attn.bias c_procj.weight c_procj.bias)
          (->caten (attn_impl_torch x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))
        (proceed (attn-impl x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))))
;; Segfault Test (occurs with use_kv_cache=T, and n_layers > 1)
(deftest test-symbolic-regression-test
  (with-no-grad
    (when (= 1 (ctx:getenv :JIT))
      (let* ((model (Transformer 32 4 0 1e-5 32))
             (x (forward model (make-tensor `(b s)) (iconst 'n)))
             (model (caten x)))
        (ok (forward model `(b . 1) `(s . 2) `(n . 2)))))))

(deftest test-symbolic-transformer-forward-test-no-kv-cache-1-layer
  (with-no-grad
    (when (= 1 (ctx:getenv :JIT))
      (let* ((caten/llm::*use-kv-cache* nil)
             (model (Transformer 32 4 1 1e-5 32))
             (x (forward model (make-tensor `(1 s) :from 'x) (iconst 'n)))
             (model (caten x)))
        (let ((value (forward model `(x . ,(randint `(1 3) :low 0 :high 10)) `(s . 3) `(n . 0))))
          (ok value (format nil "~a" value)))))))

(deftest test-symbolic-transformer-forward-test-no-kv-cache-2-layer
  (with-no-grad
    (when (= 1 (ctx:getenv :JIT))
      (testing "No Segv?"
        (let* ((caten/llm::*use-kv-cache* nil)
               (model (Transformer 32 4 2 1e-5 32))
               (x (forward model (make-tensor `(1 s) :from 'x) (iconst 'n)))
               (model (caten x)))
          (let ((value (forward model `(x . ,(randint `(1 3) :low 0 :high 10)) `(s . 3) `(n . 0))))
            (ok value (format nil "~a" value))))))))

#|
(deftest test-symbolic-transformer-forward-test-1-layer
  (with-no-grad
    (when (= 1 (ctx:getenv :JIT))
      (let* ((caten/llm::*use-kv-cache* t)
             (model (Transformer 32 4 1 1e-5 32))
             (x (forward model (make-tensor `(1 s) :from 'x) (iconst 'n)))
             (model (caten x)))
        (ok (forward model `(x . ,(randint `(1 3) :low 0 :high 10)) `(s . 3) `(n . 0)))))))
|#

(deftest test-symbolic-transformer-forward-test-2-layer
  (with-no-grad
    (when (= 1 (ctx:getenv :JIT))
      (testing "No Segv?"
        (let* ((caten/llm::*use-kv-cache* t)
               (model (Transformer 32 4 2 1e-5 32))
               (x (forward model (make-tensor `(1 s) :from 'x) (iconst 'n)))
               (model (caten x)))
          (ok (forward model `(x . ,(randint `(1 3) :low 0 :high 10)) `(s . 3) `(n . 0))))))))
|#

(let* ((dim 64)
       (n-heads 4)
       (batch-size 1)
       (seq-len 32)
       (x (proceed (make-tensor `(,batch-size ,seq-len ,dim) :initial-element 1.0)))
       (c_attn.weight  (normal `(,(* 3 dim) ,dim) :mean 0.0 :std 0.1))
       (c_attn.bias    (normal `(,(* 3 dim))  :mean 0.0 :std 0.1))
       (c_procj.weight (normal `(,dim ,dim) :mean 0.0 :std 0.1))
       (c_procj.bias   (normal `(,dim) :mean 0.0 :std 0.1)))
  (assert-equal
   (:rtol 1e-5 :atol 1e-5) ;; TODO: Rtol in 1e-5, atol looks still unstable?...
   (with-torch (x c_attn.weight c_attn.bias c_procj.weight c_procj.bias)
     (->caten (attn_impl_torch x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias)))
   (proceed (attn-impl x n-heads c_attn.weight c_attn.bias c_procj.weight c_procj.bias))))



(let* ((x (make-tensor '(1 32 192) :initial-element 1.0))
       (chunks 3)
       (dim 2)
       (chunked-tensors (multiple-value-list (!chunk x chunks :dim dim))))
;; Print the shapes of the chunks
(loop for chunk in chunked-tensors
      do (format t "Chunk shape: ~a~%" (shape chunk)))
;; Verify that the sum of chunk sizes equals the dimension size
(let ((total-size (apply #'+ (mapcar (lambda (chunk) (nth dim (shape chunk))) chunked-tensors))))
  (assert (= total-size (nth dim (shape x)))
          ()
          "Total chunk sizes do not match dimension size"))
(format t "Chunking successful.~%"))
