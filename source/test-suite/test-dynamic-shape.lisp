(in-package :caten/test-suite)

;; failing case in the PR
#|
(deftest symbolic-function-args-test
  (with-protect-jit
    (when (= (ctx:getenv :JIT) 1)
      (let ((args (node-reads (get-jit-info (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb))))))))
	(ok (every #'(lambda (x) (find x args)) `(N FROMB TOB BYB TOA FROMA BYA)))
	(ok (= (length args) 9))))))

(deftest tensor-shaped-tensor-test
  (let* ((size1 (!add (iconst 'a) (iconst 'a)))
	 (size2 (!add (iconst 'b) (iconst 'b)))
	 (tensor (caten (make-tensor `(,size1 ,size2) :initial-element 'a)))
	 (out (elements (forward tensor `(a . 4) `(b . 8)))))
    (ok (= (length out) 128))
    (ok (every (equal-to 4) out)))
  ;; segv
  ;; (let* ((size1 (!add (iconst 'a) (iconst 'a)))
  ;;	 (size2 (!add (iconst 'b) (iconst 'b)))
  ;;	 (tensor (caten (!sin (make-tensor `(,size1 ,size2) :initial-element 'a))))
  ;;	 (out (elements (forward tensor `(a . 4) `(b . 8)))))g
  ;;  (ok (= (length out) 128))
  ;;  (ok (every (equal-to (sin 4)) out)))
  )

;; TODO: Tensor-Shaped-Tesnor Operation
;; TODO: Tensor-Shaped-Tensor Iteration Rendering (The scalar result should be passed via arguments)
;; segv

(deftest tensor-viewed-tensor-test
  (testing "Upfrom"
    (let* ((v1 (!add (iconst 'a) (iconst 'a)))
	   (v2 (!add (iconst 'b) (iconst 'b)))
	   (c (make-tensor `(10 10) :initial-element 1.0))
	   (out (!contiguous (!view c `(,v1 10) `(,v2 10))))
	   (model (caten out))
	   (result (forward model `(a . 1) `(b . 2))))
      (ok (equal `(8 6) (buffer-shape (tensor-buffer result))))
      (ok (= 48 (length (elements result))))
      (ok (every (equal-to 1.0) (elements result)))))
  (testing "Below"

    )
  (testing "By"

    )
  (testing "Broadcast"

;; [TODO] Minimal Repro for two failing case
;;(caten (!sin (make-tensor `(,(!add (make-tensor `(1)) (iconst 'n))))))
;; (caten (!add (!add (!index-components `(5)) (!add caten/apis::*rng-counter* (!* (iconst 2) (iconst 'n)) :reduce t))  (!* (iconst 2) (iconst 'n))))
))
|#
