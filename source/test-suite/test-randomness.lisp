(in-package :caten/test-suite)

;; [TODO] the behaviour is the same as numpy?
(deftest compile-randn
  (ok (caten (!randn `(n)))))

(deftest compile-normal
  (ok (caten (!normal `(10 10)))))
