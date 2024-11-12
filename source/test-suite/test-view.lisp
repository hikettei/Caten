(in-package :caten/test-suite)

(deftest test-chunk-node
  (let ((x (ax+b `(12) 1 0)))
    (multiple-value-bind (a b c d) (!chunk x 4)
      (ok (every #'= (elements (proceed (!+ a b c d))) #(18.0 22.0 26.0)))))
  (let ((x (ax+b `(8 8) 1 0)))
    (multiple-value-bind (a b c d) (!chunk x 4)
      (ok (every #'= (elements (proceed (!contiguous (!+ a b c d))))
                 #(96.0 100.0 104.0 108.0 112.0 116.0 120.0 124.0 128.0 132.0 136.0
                   140.0 144.0 148.0 152.0 156.0 16.0 17.0 18.0 19.0 20.0 21.0 22.0
                   23.0 24.0 25.0 26.0 27.0 28.0 29.0 30.0 31.0 32.0 33.0 34.0 35.0
                   36.0 37.0 38.0 39.0 40.0 41.0 42.0 43.0 44.0 45.0 46.0 47.0 48.0
                   49.0 50.0 51.0 52.0 53.0 54.0 55.0 56.0 57.0 58.0 59.0 60.0 61.0
                   62.0 63.0))))))

(deftest test-concatenate-node
  (let ((x (ax+b `(3) 1 0))
        (y (ax+b `(3) 1 3))
        (z (ax+b `(3) 1 6)))
    (ok (= 9 (array-total-size (elements (proceed (!concatenate 0 x y z))))))
    (ok (every #'= (elements (proceed (!concatenate 0 x y z))) #(0 1 2 3 4 5 6 7 8 9)))))
;; !split + !matmul which is required for Attention Scheduling
