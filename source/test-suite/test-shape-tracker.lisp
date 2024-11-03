(in-package :caten/test-suite)

(deftest shrink+reshape-contiguous
  (testing "Reshape(Slice(...)) should create a copy"
    (ok
     (let ((caten/aasm:*default-order* :row)
           (a (ax+b `(2 2 8) 1 0)))
       (let ((b (!view a t t `(0 4)))
             (c (!view a t t `(4 8))))
         (let ((vals (buffer-value (tensor-buffer (proceed (!add (!reshape b `(2 2 1 4)) (!reshape c `(2 2 1 4))))))))
           (ok (every #'= vals #(4.0 6.0 8.0 10.0 20.0 22.0 24.0 26.0 36.0 38.0 40.0 42.0 52.0 54.0 56.0 58.0)))))))))
;; [TODO] More failing case follows ...
