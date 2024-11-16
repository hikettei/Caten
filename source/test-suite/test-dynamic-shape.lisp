(in-package :caten/test-suite)

;; [TODO] Fix Dynamic Shape Testing
;; - Passing the comment outed test
;; - Transformer Full Symbolic
;; - (defparameter *model* (time (Transformer 64 1 2 1e-5 32)))
;; - (defparameter *transformer* (caten (call *model* (make-tensor `(10 32)) (iconst 'n))))
(deftest symbolic-function-args-test
  (with-protect-jit
    (let ((kernel (find :JIT_KERNEL (graph-nodes (avm-graph (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb)))))) :key #'node-type)))
      (assert kernel () "axpy is not scheduled")
      (let ((args (node-reads kernel)))
        (ok (equal (butlast (subseq args 1)) `(BYB TOB FROMB BYA TOA FROMA)))))))

(deftest tensor-shaped-tensor-test-1
  (let* ((size1 (!add (iconst 'a) (iconst 'a)))
         (size2 (!add (iconst 'b) (iconst 'b)))
         (tensor (caten (make-tensor `(,size1 ,size2) :initial-element 'a)))
         (out (elements (forward tensor `(a . 4) `(b . 8)))))
    (ok (= (length out) 128))
    (ok (every #'(lambda (x) (= x 4)) out))))

(deftest tensor-shaped-tensor-test-2
  (let* ((size1 (!add (iconst 'a) (iconst 'a)))
  	 (size2 (!add (iconst 'b) (iconst 'b)))
  	 (tensor (caten (!sin (make-tensor `(,size1 ,size2) :initial-element 'a))))
  	 (out (elements (forward tensor `(a . 4) `(b . 8)))))
    (ok (= (length out) 128))
    (ok (every #'(lambda (x) (= x (sin 4))) out))))

(deftest tensor-view-tensor-test-1
  (testing "Upfrom"
    (let* ((v1 (!add (iconst 'a) (iconst 'a)))
	   (v2 (!add (iconst 'b) (iconst 'b)))
	   (c (make-tensor `(10 10) :initial-element 1.0))
	   (out (!contiguous (!view c `(,v1 10) `(,v2 10))))
	   (model (caten out))
	   (result (forward model `(a . 1) `(b . 2))))
      (ok (equal `(8 6) (buffer-shape (tensor-buffer result))))
      (ok (= 48 (length (elements result))))
      (ok (every #'(lambda (x) (= x 1.0)) (elements result))))))

(deftest symbolic-tensor-failing-case-1
  (let ((m (caten (!sin (make-tensor `(,(!reshape (!add (make-tensor `(1)) (iconst 'n)))))))))
    (print (forward m `(n . 3)))))

(deftest symbolic-tensor-failing-case-2
  (let ((m (caten (!add (!add (!index-components `(5)) (!add caten/apis::*rng-counter* (!* (iconst 2) (iconst 'n)) :reduce t))  (!* (iconst 2) (iconst 'n))))))
    (print m)))
