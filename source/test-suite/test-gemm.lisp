(in-package :caten/test-suite)

(macrolet ((def (name shape1 shape2)
	     `(deftest ,name
		(with-given-dtype ((:float32 . "float32"))
		  (let ((x (rand ',shape1))
			(y (rand ',shape2)))
		    (assert-equal
			(:atol 1e-6 :rtol 1e-5)
			(with-torch (x y)
			  (->caten (torch.matmul x y)))
			(proceed (!matmul x y))))))))
  (def test-squared-2d-matmul (256 256) (256 256))
  (def test-2d-matmul (1024 512) (512 2048))
  (def test-broadcast-matmul-left (2 1024 512) (512 130))
  (def test-broadcast-matmul-right (1024 512) (2 512 130))
  (def test-3d-matmul (64 64 64) (64 64 64))
  (def test-4d-matmul (16 16 16 16) (16 16 16 16)))
