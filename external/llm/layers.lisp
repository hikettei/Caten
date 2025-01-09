(in-package :caten/llm)

(defun scaled-dot-product-attention (query key value &optional mask)
  (let ((qk (!div (!matmul query (!transpose key -1 -2)) (fconst (sqrt (car (last (shape query))))))))
    (!matmul (!softmax (if mask (!add qk mask) qk) :axis -1) value)))

(defparameter *use-kv-cache* t)
(defmodel (Attention (dim n-heads max-seq-len &key (use-kv-cache *use-kv-cache*)))
    ((c-attn (Linear dim (* 3 dim) :bias t))
     (c-proj (Linear dim dim :bias t))
     (n-heads n-heads)
     (dim dim)
     (max-seq-len max-seq-len :reader attn-max-seq-len)
     (head-dim (floor (/ dim n-heads)))
     (use-kv-cache use-kv-cache :type boolean :reader attn-use-kv-cache)
     (k-cache nil :accessor attn-k-cache)
     (v-cache nil :accessor attn-v-cache)))

(defmethod merge-kv-cache ((attn Attention) k v start-pos)
  (when (null (attn-use-kv-cache attn))
    (return-from merge-kv-cache (values k v)))
  (multiple-value-bind (batch-size seq-len n-heads head-dim) (apply #'values (shape k))
    (assert (integerp batch-size) () "KVCache does not support a dynamic batch size")
    (let* ((max-len (attn-max-seq-len attn))
           (k-cache (or (attn-k-cache attn) (linspace `(,batch-size ,max-len ,n-heads ,head-dim) 0 0)))
           (v-cache (or (attn-v-cache attn) (linspace `(,batch-size ,max-len ,n-heads ,head-dim) 0 0)))
           (range1 (list start-pos (!+ start-pos (iconst seq-len))))
           (range2 (list 0 (!+ start-pos (iconst seq-len)))))
      (setf (attn-k-cache attn) k-cache
            (attn-v-cache attn) v-cache)
      (setf k-cache (!assign (!view k-cache t range1 t t) k)
            v-cache (!assign (!view v-cache t range1 t t) v))
      (values (!view-from-base k-cache t range2 t t) (!view-from-base v-cache t range2 t t)))))

(defmethod call ((model Attention) &rest inputs)
  (with-slots ((c-attn c-attn) (c-proj c-proj) (n-heads n-heads) (dim dim) (head-dim head-dim) (max-seq-len max-seq-len)) model
    (multiple-value-bind (x mask start-pos) (apply #'values inputs)
      (multiple-value-bind (xq xk xv) (!chunk (call c-attn x) 3 :dim 2)
        (let* ((new-x-shape (append (butlast (shape xq)) (list n-heads (/ (car (last (shape xq))) n-heads))))
               (xq (!reshape xq new-x-shape))
               (xk (!reshape xk new-x-shape))
               (xv (!reshape xv new-x-shape)))
          (multiple-value-bind (xk xv) (merge-kv-cache model xk xv start-pos)
            (let* ((xq (!transpose xq 1 2))
                   (xk (!transpose xk 1 2))
                   (xv (!transpose xv 1 2))
                   (attn-output (scaled-dot-product-attention xq xk xv mask))
                   ;; Merging heads (todo: this thing can be rewritten in a more readable way by introducing DSL)
                   (attn-output (!permute attn-output 0 2 1 3))
                   (attn-output (!reshape attn-output (append (butlast (shape attn-output) 2) (list (apply #'* (last (shape attn-output) 2)))))))
              (call c-proj attn-output))))))))

(defmodel (LlamaAttention (dim n-heads max-seq-len &key (rope-dim nil)))  ; Name and initargs
    ((wq (Linear dim dim :bias nil))      ; Separate Q projection
     (wk (Linear dim dim :bias nil))      ; Separate K projection
     (wv (Linear dim dim :bias nil))      ; Separate V projection
     (wo (Linear dim dim :bias nil))      ; Output projection
     (n-heads n-heads)
     (dim dim)
     (head-dim (floor (/ dim n-heads)))
     (rope-dim (or rope-dim (floor (/ head-dim 2)))) ; RoPE dimension
     (max-seq-len max-seq-len)))

(defmethod call ((model LlamaAttention) &rest inputs)
  (with-slots ((wq wq) (wk wk) (wv wv) (wo wo) 
                       (n-heads n-heads) (dim dim) (head-dim head-dim) 
                       (rope-dim rope-dim)) model
    (multiple-value-bind (x mask start-pos) (apply #'values inputs)
      (let* ((batch-size (car (shape x)))
             (seq-len (second (shape x)))
             
             (q (call wq x))
             (k (call wk x))
             (v (call wv x))
             
             (new-shape (list batch-size seq-len n-heads head-dim))
             (q (!reshape q new-shape))
             (k (!reshape k new-shape))
             (v (!reshape v new-shape))
             ;; Apply RoPE to Q and K
             (q (!rope q rope-dim))
             (k (!rope k rope-dim))
 
             (q (!transpose q 1 2))
             (k (!transpose k 1 2))
             (v (!transpose v 1 2))
 
             (attn-output (scaled-dot-product-attention q k v mask))
             
             (attn-output (!permute attn-output 0 2 1 3))
             (attn-output (!reshape attn-output 
                                    (list batch-size seq-len dim))))
        (call wo attn-output)))))

  
(defmodel (FeedForwardLLAMA (dim hidden-dim))
    ((fc1   (Linear dim hidden-dim))
     (fc2 (Linear dim hidden-dim))
     (fc3 (Linear hidden-dim dim))))

(defcall (model FeedForwardLLAMA) (X[~])
  (with-slots ((fc1 fc1) (fc2 fc2) (fc3 fc3)) model
    (forward fc3 (!mul (!silu (forward fc1 x)) (forward fc2 x)))))

(defmodel (FeedForward (dim hidden-dim))
    ((c-fc   (Linear dim hidden-dim))
     (c-proj (Linear hidden-dim dim))))

(defcall (model FeedForward) (X[~])
  (with-slots ((c-fc c-fc) (c-proj c-proj)) model
    (forward c-proj (!gelu (forward c-fc x)))))

(defmodel (TransformerBlockLlama (dim n-heads &key (norm-eps 1e-5) (max-seq-len 1024)))
    ((attn (LlamaAttention dim n-heads max-seq-len))
     (mlp (FeedForwardLlama dim (* 4 dim)))
     (rms_1 (RMSNorm `(,dim) :eps norm-eps))
     (rms_2 (RMSNorm `(,dim) :eps norm-eps))))

(defmethod call ((model TransformerBlockLlama) &rest inputs)
  (multiple-value-bind (x mask start-pos) (apply #'values inputs)
    (with-slots ((attn attn) (mlp mlp) (rms_1 rms_1) (rms_2 rms_2)) model
      (let ((h (!add x (forward attn (forward rms_1 x) mask start-pos))))
        (!add h (forward mlp (forward rms_2 h)))))))

(defmodel (TransformerBlock (dim n-heads &key (norm-eps 1e-5) (max-seq-len 1024)))
    ((attn (Attention dim n-heads max-seq-len))
     (mlp (FeedForward dim (* 4 dim)))
     (ln_1 (LayerNorm `(,dim) :eps norm-eps))
     (ln_2 (LayerNorm `(,dim) :eps norm-eps))))

(defmethod call ((model TransformerBlock) &rest inputs)
  (multiple-value-bind (x mask start-pos) (apply #'values inputs)
    (with-slots ((attn attn) (mlp mlp) (ln_1 ln_1) (ln_2 ln_2)) model
      (let ((h (!add x (forward attn (forward ln_1 x) mask start-pos))))
	(!add h (forward mlp (forward ln_2 h)))))))

(defmodel (Transformer (dim n-heads n-layers norm-eps vocab-size &key (max-seq-len 1024)))
    ((vocab-size vocab-size)
     (wte (Embedding vocab-size dim))
     (wpe (Embedding max-seq-len dim))
     (h (loop repeat n-layers collect (TransformerBlock dim n-heads :norm-eps norm-eps :max-seq-len max-seq-len)))
     (ln-f (LayerNorm `(,dim) :eps norm-eps))
     (lm-head (Linear dim vocab-size :bias nil))))

(defcall (model Transformer) (Tokens[Batch Seq-Len] Start-Pos[])
  (with-slots ((wte wte) (wpe wpe) (h h) (ln-f ln-f) (lm-head lm-head)) model
    (let* ((token-emb (forward wte tokens))
	   (pos-emb   (forward wpe (!cast (!add start-pos (!index-components `(1 ,seq-len))) (dtype-of tokens))))
	   (hi (!add token-emb pos-emb))
	   (mask (!triu (!full `(1 1 ,seq-len ,(!+ start-pos (iconst seq-len))) (-inf)) :diagonal (!+ (iconst 1) start-pos)))
	   (_ (dolist (hn h) (setf hi (forward hn hi mask start-pos))))
	   (logits (forward lm-head (forward ln-f hi))))
      (declare (ignore _))
      (!argmax (!view logits t -1 t)))))

(defmodel (LlamaTransformer (dim n-heads n-layers norm-eps vocab-size &key (max-seq-len 1024)))
    ((vocab-size vocab-size)
     (wte (Embedding vocab-size dim))
     ;; Remove wpe (positional embeddings) since we use RoPE
     (h (loop repeat n-layers collect 
                 (TransformerBlockLlama dim n-heads 
                                        :norm-eps norm-eps 
                                        :max-seq-len max-seq-len)))
     (rms-f (RMSNorm `(,dim) :eps norm-eps))  ; LayerNorm -> RMSNorm
     (lm-head (Linear dim vocab-size :bias nil))))

(defcall (model LlamaTransformer) (Tokens[Batch Seq-Len] Start-Pos[])
  (with-slots ((wte wte) (h h) (rms-f rms-f) (lm-head lm-head)) model
    (let* ((token-emb (forward wte tokens))
           (hi token-emb)
           (mask (!triu (!full `(1 1 ,seq-len ,(!+ start-pos (iconst seq-len))) 
                               (-inf)) 
                        :diagonal (!+ (iconst 1) start-pos)))
           (_ (dolist (hn h) (setf hi (forward hn hi mask start-pos))))
           (logits (forward lm-head (forward rms-f hi))))
      (declare (ignore _))
      (!argmax (!view logits t -1 t)))))