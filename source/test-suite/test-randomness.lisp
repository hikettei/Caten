(in-package :caten/test-suite)

;; [TODO] Needs precision test
(deftest compile-randn
  (ok (caten (!randn `(n)))))
