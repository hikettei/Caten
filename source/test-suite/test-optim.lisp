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

(deftest test-end-to-end-training
  (with-protect-jit
    (let* ((model (Simple-Test-MLP 64 32 16))
           (output (call model (make-tensor `(10 64) :from :x)))
           (loss  (!cross-entropy (!softmax output) (make-tensor `(10 16) :from :y)))
           (runner (caten loss))
           (optimizers (hook-optimizers runner #'(lambda (x) (make-instance 'SGD :param x :lr 1e-2))))
           (x (rand `(10 64)))
           (y (rand `(10 16))))
      (ok (= (length optimizers) 6))
      (flet ((run-step ()
               (let ((loss (forward runner `(:x . ,x) `(:y . ,y))))
                 (backward runner)
                 (mapc #'step-optimizer optimizers)
                 (mapc #'zero-grad optimizers)
                 loss)))
        (let ((losses (loop repeat 10 collect (run-step))))
          (ok (> (aref (elements (first losses)) 0) (aref (elements (car (last losses))) 0)) (format nil "losses: ~a" (map 'list #'elements losses))))))))
