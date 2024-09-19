(in-package :caten/test-suite)

;; Neural Ops used in implementing LLM

(deftest test-scaled-dot-product-attention
  (with-given-dtype ((:float32 . "float32"))
    (let ((q (randn `(2 8 8 8)))
	  (k (randn `(2 8 8 8)))
	  (v (randn `(2 8 8 8))))
      (assert-equal
	  ()
	  (with-torch (q k v)
	    (->caten (f:scaled_dot_product_attention q k v)))
	  (proceed (scaled-dot-product-attention q k v))))))
