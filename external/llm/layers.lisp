(in-package :caten/llm)
;; [TODO] Slot-name: is it possible to reconstruct from gguf in a meta-programming way?
;; Some of subgraph are purged?
;; call still has an issue?
;; Cannot be compiled with the dynamic shaped?
(defun scaled-dot-product-attention (query key value &optional mask)
  (let ((qk (!div (!matmul query (!transpose key -1 -2)) (fconst (sqrt (car (last (shape query))))))))
    (!matmul (!softmax (if mask (!add qk mask) qk) :axis -1) value)))

(defmodel (Attention (dim n-heads max-seq-len))
    ((c-attn (Linear dim (* 3 dim) :bias t))
     (c-proj (Linear dim dim :bias t))
     (n-heads n-heads)
     (dim dim)
     (max-seq-len max-seq-len)
     (head-dim (floor (/ dim n-heads)))
     (k-cache nil :accessor attn-k-cache)
     (v-cache nil :accessor attn-v-cache)))
;; [TODO] Add: KV-Cache
(defmethod call ((model Attention) &rest inputs)
  (with-slots ((c-attn c-attn) (c-proj c-proj) (n-heads n-heads) (dim dim) (head-dim head-dim) (max-seq-len max-seq-len)) model
    (multiple-value-bind (x mask) (apply #'values inputs)
      (let* ((batch-size (car (shape x)))
             (seq-len (second (shape x)))
             (xqkv (call c-attn x))
             (xqkv (!reshape xqkv `(,batch-size ,seq-len ,n-heads 3 ,head-dim)))
             (xqkv (!permute xqkv 3 0 2 1 4)))
        (multiple-value-bind (xq xk xv) (!chunk xqkv 3 :dim 0)
          (let* ((attn-output (scaled-dot-product-attention xq xk xv mask))
                 (attn-output (!reshape (!transpose attn-output 1 2) `(,batch-size ,seq-len ,dim))))
            (call c-proj attn-output)))))))

(defmodel (FeedForward (dim hidden-dim))
    ((c-fc   (Linear dim hidden-dim))
     (c-proj (Linear hidden-dim dim))))

(defmethod call ((model FeedForward) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((c-fc c-fc) (c-proj c-proj)) model
      (forward c-proj (!gelu (forward c-fc x))))))

(defmodel (TransformerBlock (dim n-heads &key (norm-eps 1e-5) (max-seq-len 1024)))
    ((attn (Attention dim n-heads max-seq-len))
     (mlp (FeedForward dim (* 4 dim)))
     (ln_1 (LayerNorm `(,dim) :eps norm-eps))
     (ln_2 (LayerNorm `(,dim) :eps norm-eps))))

(defmethod call ((model TransformerBlock) &rest inputs)
  (multiple-value-bind (x mask) (apply #'values inputs)
    (with-slots ((attn attn) (mlp mlp) (ln_1 ln_1) (ln_2 ln_2)) model
      (let ((h (forward attn (forward ln_1 x) mask)))
	(!add h (forward mlp (forward ln_2 h)))))))

(defmodel (Transformer (dim n-heads n-layers norm-eps vocab-size &key (max-seq-len 1024)))
    ((vocab-size vocab-size)
     (wte (Embedding vocab-size dim))
     (wpe (Embedding max-seq-len dim))
     (h (loop repeat n-layers collect (TransformerBlock dim n-heads :norm-eps norm-eps :max-seq-len max-seq-len)))
     (ln-f (LayerNorm `(,dim) :eps norm-eps))
     (lm-head (Linear dim vocab-size :bias nil))))

(defmethod call ((model Transformer) &rest inputs)
  (multiple-value-bind (tokens start-pos) (apply #'values inputs)
    (assert (and tokens start-pos))
    (st "Tokens[batch sentence_length] Start_Pos[] -> Tokens[batch sentence_length]" (tokens start-pos))
    (with-slots ((wte wte) (wpe wpe) (h h) (ln-f ln-f) (lm-head lm-head)) model
      (let* ((token-emb (forward wte tokens))
	     (pos-emb   (forward wpe (!cast (!add start-pos (!index-components `(1 ,(second (shape tokens))))) (dtype-of tokens))))
	     (hi (!add token-emb pos-emb))
             (seq-len (iconst (second (shape tokens))))
	     (mask (!triu (!full `(1 1 ,seq-len ,seq-len) (-inf)) :diagonal (!+ (iconst 1) start-pos)))
	     (_ (loop for hn in h do
	       (setf hi (forward hn hi mask))))
	     (logits (forward lm-head (forward ln-f hi))))
	(declare (ignore _))
	(!argmax logits)))))
#+(or)(with-no-grad (caten (forward (Transformer 64 4 4 1e-5 512) (make-tensor `(10 10)) (iconst 0))))
