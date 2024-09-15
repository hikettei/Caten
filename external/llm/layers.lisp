(in-package :caten/llm)

(defun scale-dot-product-attention (query key value &optional mask)
  (let ((qk (!div (!matmul query (!transpose key -1 -2)) (fconst (sqrt (car (last (shape query))))))))
    (!matmul (!softmax (if mask (!add qk mask) qk) :axis -1) value)))

(defmodel (Attention (dim n-heads max-len))
    ((c-attn (Linear dim (* 3 dim) :bias t))
     (c-proj (Linear dim dim :bias t))
     (n-heads n-heads)
     (dim dim)
     (max-len max-len)
     (head-dim (floor (/ dim n-heads)))
     (k-cache nil :accessor attn-k-cache)
     (v-cache nil :accessor attn-v-cache)))
;; (defmethod reset-kv-cache ())
(defmethod call ((model Attention) &rest inputs)
  (with-slots ((c-attn c-attn) (c-proj c-proj) (n-heads n-heads) (dim dim) (head-dim head-dim) (max-len max-len)) model
    (multiple-value-bind (x n mask) (apply #'values inputs)
      (let* ((xqkv       (call c-attn x))
	     (batch-size (car (shape x)))
	     (seq-len    (iconst (second (shape x))))
	     ;; [TODO] The allocation should be lazy; use make-tensor to compile into c code.
	     (k-cache (linspace `(,batch-size ,max-len ,n-heads ,head-dim) 0 0))
	     (v-cache (linspace `(,batch-size ,max-len ,n-heads ,head-dim) 0 0)))
	(setf (attn-k-cache model) k-cache (attn-v-cache model) v-cache)
	(multiple-value-bind (xq xk xv)
	    (apply #'values (loop for i upfrom 0 below 3
				  collect
				  (!reshape
				   (!view xqkv t t (list (* i dim) (* (1+ i) dim)))
				   `(,batch-size ,(second (shape x)) ,n-heads ,head-dim))))
	  (setf (attn-k-cache model)
		(!move (!view (attn-k-cache model) t `(,n ,(!+ n seq-len)) t t) xk :reduce t)
		(attn-v-cache model)
		(!move (!view (attn-v-cache model) t `(,n ,(!+ n seq-len)) t t) xv :reduce t))
	  (multiple-value-bind (keys vals)
	      (values
	       (!view (attn-k-cache model) t `(0 ,(!+ n seq-len)) t t)
	       (!view (attn-v-cache model) t `(0 ,(!+ n seq-len)) t t))
	    (multiple-value-bind (xq keys vals)
		(values
		 (!transpose xq 1 2)
		 (!transpose keys 1 2)
		 (!transpose vals 1 2))
	      (call
	       c-proj
	       (!reshape
		(!transpose
		 (scale-dot-product-attention xq keys vals mask)
		 1 2)
		`(,batch-size ,(second (shape x)) ,dim))))))))))

(defmodel (FeedForward (dim hidden-dim))
    ((c-fc   (Linear dim hidden-dim))
     (c-proj (Linear hidden-dim dim))))

(defmethod call ((model FeedForward) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((c-fc c-fc) (c-proj c-proj)) model
      (call c-proj (!gelu (call c-fc x))))))

