(in-package :caten/test-suite)

;; Neural Ops used in implementing LLM
(deftest test-scaled-dot-product-attention
  (with-given-dtype ((:float32 . "float32"))
    (let ((q (rand `(2 8 128 64)))
	  (k (rand `(2 8 128 64)))
	  (v (rand `(2 8 128 64))))
      (assert-equal
	  ()
	  (with-torch (q k v)
	    (->caten (f:scaled_dot_product_attention q k v)))
	  (proceed (scaled-dot-product-attention q k v))))))
