(in-package :caten/test-suite)

(deftest symbolic-function-args-test
  (with-protect-jit
    (let ((kernel (find :JIT_KERNEL (graph-nodes (avm-graph (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb)))))) :key #'node-type)))
      (assert kernel () "axpy is not scheduled")
      (let ((args (node-reads kernel)))
        (ok (equal (butlast (subseq args 1)) `(TOA BYB FROMB BYA FROMA)))))))

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
      (ok (every #'(lambda (x) (= x 1.0)) (elements result)))))
  (testing "Below"
    (let* ((v1 (!add (iconst 'a) (iconst 'a)))
	   (v2 (!add (iconst 'b) (iconst 'b)))
	   (c (make-tensor `(10 10) :initial-element 1.0))
	   (out (!contiguous (!view c `(1 ,v1) `(1 ,v2))))
	   (model (caten out))
	   (result (forward model `(a . 3) `(b . 3))))
      (ok (equal `(5 5) (buffer-shape (tensor-buffer result))))
      (ok (= 25 (length (elements result))))
      (ok (every #'(lambda (x) (= x 1.0)) (elements result))))))

(deftest symbolic-tensor-failing-case-1
  (let ((m (caten (!sin (make-tensor `(,(!add (iconst 1) (iconst 'n))))))))
    (ok (= (length (elements (forward m `(n . 3))))))))

(deftest symbolic-tensor-failing-case-2
  (let* ((x (aref (buffer-value (tensor-buffer caten/apis::*rng-counter*)) 0))
         (m (caten (!add (!add (!index-components `(5)) (!add caten/apis::*rng-counter* (!* (iconst 2) (iconst 'n)) :reduce t))  (!* (iconst 2) (iconst 'n)))))
         (o `(,(+ x (* 4 4)) ,(+ x (* 4 4) 1) ,(+ x (* 4 4) 2) ,(+ x (* 4 4) 3))))
    (ok (every #'= o (elements (forward m `(n . 4)))))))
;; Transformer-Failing-Case-Repro:
;; ```
;; LOAD: val_3 = N
;; WPE:  val_5 = Embedding(..., val_3)
;; MASK: val_6 = Triu(..., val_3)
;; ```
;; - val_3 is used by both `val_5` and `val_6`
;; - graph-schedule once adds val_3 to `seen`, val_6 group has no val_3.
(deftest transformer-working-case-repro
  ;; Initialize 'N for WPE and MASK respectively
  (let* ((wpe (Embedding 10 10))
         (mask (!triu (!full `(1 1 10 10) (-inf)) :diagonal (!+ (iconst 1) (iconst 'n))))
         (pos-emb (forward wpe (!cast (!add (iconst 'n) (!index-components `(1 10))) :float32)))
         (out (!mul mask pos-emb)))
    (ok (caten out))))

(deftest transformer-failing-case-repro
  (let* ((n (iconst 'n)) ;; val_3 <- N, both WPE/MASK use this
         (wpe (Embedding 10 10))
         (mask (!triu (!full `(1 1 10 10) (-inf)) :diagonal (!+ (iconst 1) n)))
         (pos-emb (forward wpe (!cast (!add n (!index-components `(1 10))) :float32)))
         (out (!mul mask pos-emb)))
    (ok (caten out))))

(deftest tensor-shaped-index-component
  (let* ((n (iconst 'n))
         (m (iconst 'm))
         (ic (!index-components `(,(!add n m) ,(!add n m)))))
    (ok (caten ic))))

(deftest symbolic-small-repro
  (ok (caten (!triu (make-tensor `(s s s s))))))

(deftest binary-symbolic-small-repro
  (ok (caten (!+ (make-tensor `(1 1 s 1)) (make-tensor `(1 1 1 s))))))

(deftest binary-symbolic-small-repro-1
  (let ((s (!add (iconst 'n) (iconst 1))))
    (ok (caten (!+ (make-tensor `(1 1 ,s 1)) (make-tensor `(1 1 1 ,s)))))))

(deftest symbolic-view+triu
  (let* ((n (iconst 'n))
         (s 's)
         (mask (!triu (!full `(1 1 ,s ,(!+ (iconst s) (iconst 1))) (-inf)) :diagonal (!+ (iconst 1) n))))
    (ok (caten mask))))
;; ~~ Symbolic Accuracy Testing ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(deftest symbolic-matmul-test
  (loop with m = (caten (!matmul (make-tensor `(a a) :from 'x) (make-tensor `(a a) :from 'y)))
        for a upfrom 10 below 20
        for x = (rand `(,a ,a))
        for y = (rand `(,a ,a))
        for symbolic = (forward m `(a . ,a) `(x . ,x) `(y . ,y))
        for expected = (proceed (!matmul x y))
        do (setf (tensor-shape symbolic) (tensor-shape expected)) ;; Symbolic returns `(A A) tensor.
           (assert-equal () symbolic expected)))

(deftest symbolic-scaled-dot-product-attention-test
  (loop with m = (caten (scaled-dot-product-attention
                         (make-tensor `(2 s 64) :from 'query)
                         (make-tensor `(2 s 64) :from 'key)
                         (make-tensor `(2 s 64) :from 'value)))
        for s upfrom 10 below 20
        for query = (randn `(2 ,s 64))
        for key   = (randn `(2 ,s 64))
        for value = (randn `(2 ,s 64))
        for symbolic = (forward m `(s . ,s) `(query . ,query) `(key . ,key) `(value . ,value))
        for expected = (proceed (scaled-dot-product-attention query key value))
        do (setf (tensor-shape symbolic) (tensor-shape expected))
           (assert-equal () symbolic expected)))

(deftest full-symbolic-scaled-dot-product-attention-test
  (loop with m = (caten (scaled-dot-product-attention
                         (make-tensor `(b s 32) :from 'query)
                         (make-tensor `(b s 32) :from 'key)
                         (make-tensor `(b s 32) :from 'value)))
        for s upfrom 10 below 13 do
          (loop for b upfrom 2 below 4
                for query = (randn `(,b ,s 32))
                for key   = (randn `(,b ,s 32))
                for value = (randn `(,b ,s 32))
                for symbolic = (forward m `(s . ,s) `(b . ,b) `(query . ,query) `(key . ,key) `(value . ,value))
                for expected = (proceed (scaled-dot-product-attention query key value))
                do (setf (tensor-shape symbolic) (tensor-shape expected))
                   (assert-equal () symbolic expected))))

(deftest symbolic-tensor-shaped-triu-test
  (loop with n = (iconst 'n)
        with s = (iconst 's)
        with m = (caten (!triu (!full `(1 1 ,(!+ n s) ,s) 5.0) :diagonal (!+ (iconst 1) n)))
        for nn upfrom 10 below 13 do
          (loop for ss upfrom 10 below 13
                for symbolic = (forward m `(s . ,ss) `(n . ,nn))
                for expected = (proceed (!triu (!full `(1 1 ,(+ nn ss) ,ss) 5.0) :diagonal (+ 1 nn)))
                do (setf (tensor-shape symbolic) (tensor-shape expected))
                   (assert-equal () symbolic expected))))
;; Failing
#|
(deftest symbolic-tensor-shaped-two-kernel-test-1
  (loop with n = (iconst 'n)
        with s = (iconst 's)
        with m = (caten
                  (!mul
                   (!matmul (!triu (!full `(1 1 ,(!+ n s) ,s) 5.0) :diagonal (!+ (iconst 1) n))
                            (!t (!triu (!full `(1 1 ,(!+ n s) ,s) 5.0) :diagonal (!+ (iconst 1) n))))
                   (!sum (!triu (!full `(1 1 ,(!+ n s) ,s) 5.0) :diagonal (!+ (iconst 1) n)))))
        for nn upfrom 10 below 13 do
          (loop for ss upfrom 10 below 13
                for symbolic = (forward m `(s . ,ss) `(n . ,nn))
                for expected = (proceed
                                (!mul
                                 (!matmul (!triu (!full `(1 1 ,(+ nn ss) ,ss) 5.0) :diagonal (+ 1 nn))
                                          (!t (!triu (!full `(1 1 ,(+ nn ss) ,ss) 5.0) :diagonal (+ 1 nn))))
                                 (!sum (!triu (!full `(1 1 ,(+ nn ss) ,ss) 5.0) :diagonal (+ 1 nn)))))
                do (setf (tensor-shape symbolic) (tensor-shape expected))
                   (assert-equal () symbolic expected))))

(deftest symbolic-tensor-shaped-two-kernel-test-2
  (loop with n = (iconst 'n)
        with s = (iconst 's)
        with m = (caten
                  (!mul
                   (!matmul (ax+b `(,n ,s) n s) (ax+b `(,s ,n) s n))
                   (!sum (!triu (!full `(1 1 ,(!+ n s) ,s) 5.0) :diagonal (!+ (iconst 1) n)))))
        for nn upfrom 10 below 13 do
          (loop for ss upfrom 10 below 13
                for symbolic = (forward m `(s . ,ss) `(n . ,nn))
                for expected = (proceed
                                (!mul
                                 (!matmul (ax+b `(,nn ,ss) nn ss) (ax+b `(,ss ,nn) ss nn))
                                 (!sum (!triu (!full `(1 1 ,(+ nn ss) ,ss) 5.0) :diagonal (+ 1 nn)))))
                do (setf (tensor-shape symbolic) (tensor-shape expected))
                   (assert-equal () symbolic expected))))
|#
