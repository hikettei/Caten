(in-package :caten/llm)

(defmodel (Attention (dim n-heads max-len))
    ((c-attn (Linear dim (* 3 dim) :bias nil))
     (c-proj (Linear dim dim :bias nil))
     (n-heads n-heads)
     (dim dim)
     (max-len max-len)
     (head-dim (floor (/ dim n-heads)))
     (k-cache nil :accessor attn-k-cache)
     (v-cache nil :accessor attn-v-cache)))
;; (defmethod reset-kv-cache ())

;; [TODO] How to implement the STATIC KV-Cache (w/o graph reconstruction)
;; [TODO] Needs a control flow refactoring
(defmethod call ((model Attention) &rest inputs)
  "X[batch slen dim]"
  (with-slots ((c-attn c-attn) (c-proj c-proj) (n-heads n-heads) (dim dim) (head-dim head-dim) (max-len max-len)) model
    (multiple-value-bind (x n mask) (apply #'values inputs)
      (let* ((xqkv       (call c-attn x))
	     (batch-size (car (shape x)))
	     (seq-len    (iconst (second (shape x))))
	     ;; [TODO] use make-tensor to compile into c code.
	     (k-cache (linspace `(,batch-size ,max-len ,n-heads ,dim) 0 0))
	     (v-cache (linspace `(,batch-size ,max-len ,n-heads ,dim) 0 0)))
	(setf (attn-k-cache model) k-cache
	      (attn-v-cache model) v-cache)
	(multiple-value-bind (xq xk xv)
	    (apply #'values (loop for i upfrom 0 below 2
				  collect
				  (!reshape (!view xqkv t t (list (* i dim) (* (1+ i) dim))) `(,batch-size ,seq-len ,n-heads ,head-dim))))
	  (setf (attn-k-cache model)
		(!move (!view (attn-k-cache model) t `(0 ,(!+ n seq-len)) t t) xk :reduce t)
		(attn-v-cache model)
		(!move (!view (attn-v-cache model) t `(0 ,(!+ n seq-len)) t t) xv :reduce t))
	  (multiple-value-bind (keys vals)
	      (values
	       (!view (attn-k-cache model) `(0 ,(!+ n seq-len)) t t)
	       (!view (attn-v-cache model) `(0 ,(!+ n seq-len)) t t))
	    
	    ))))))

(defmodel (FeedForward (dim hidden-dim))
    ((c-fc   (Linear dim hidden-dim))
     (c-proj (Linear hidden-dim dim))))

(defmethod call ((model FeedForward) &rest inputs)
  (multiple-value-bind (x) (apply #'values inputs)
    (with-slots ((c-fc c-fc) (c-proj c-proj)) model
      (call c-proj (!gelu (call c-fc x))))))

