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
  (def test-squared-2d-matmul (64 64) (64 64))
  (def test-2d-matmul (32 64) (64 128))
  (def test-broadcast-matmul-left (2 10 30) (30 20))
  (def test-broadcast-matmul-right (10 20) (2 20 30))
  (def test-3d-matmul (16 16 16) (16 16 16))
  (def test-4d-matmul (16 16 16 16) (16 16 16 16))
  (def test-5d-matmul (5 5 5 5 5) (5 5 5 5 5))
  (def test-6d-matmul (5 5 5 5 5 5) (5 5 5 5 5 5)))
  
