(in-package :caten/test-suite)

;; - (defparameter *model* (time (Transformer 64 1 2 1e-5 32)))
;; - (defparameter *transformer* (caten (call *model* (make-tensor `(10 32)) (iconst 'n))))

(deftest symbolic-function-args-test
  (with-protect-jit
    (let ((kernel (find :JIT_KERNEL (graph-nodes (avm-graph (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb)))))) :key #'node-type)))
      (assert kernel () "axpy is not scheduled")
      (let ((args (node-reads kernel)))
        (ok (equal (butlast (subseq args 1)) `(BYB FROMB BYA FROMA TOA)))))))

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

;;(deftest transformer-serialized-compilation-test
;;  (with-protect-jit
;;    (ctx:with-contextvar (:SERIALIZE 1)
;;      (ok (caten (forward (Transformer 32 4 2 1e-5 32 :max-seq-len 32) (make-tensor `(1 s)) (iconst 'n)))))))
