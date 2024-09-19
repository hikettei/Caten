(in-package :caten/test-suite)

;; [TODO] Needs precision test
(deftest compile-randn
  (ok (caten (!randn `(n)))))

(deftest compile-normal
  (ok (caten (!normal `(10 10)))))
