(in-package :caten/test-suite)

(deftest test-scaled-dot-product-attention
  (with-given-dtype ((:float32 . "float32"))
    (let ((q (randn `(2 8 128 64)))
	  (k (randn `(2 8 128 64)))
	  (v (randn `(2 8 128 64))))
      (assert-equal
	  ()
	  (with-torch (q k v)
	    (->caten (f:scaled_dot_product_attention q k v)))
	  (proceed (scaled-dot-product-attention q k v))))))
