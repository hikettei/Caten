(in-package :caten/test-suite)

(deftest test-asnode
  (ok (caten (forward (asnode #'!relu) (make-tensor `(3 3)))))
  (ok (caten (forward (asnode #'!leaky-relu :neg-slope 1e-2) (make-tensor `(3 3))))))

(defsequence Simple-Test-MLP (in-features hidden-dim out-features &key (activation #'!relu))
	     (Linear in-features hidden-dim)
	     (asnode activation)
	     (Linear hidden-dim hidden-dim)
	     (asnode activation)
	     (Linear hidden-dim out-features))
