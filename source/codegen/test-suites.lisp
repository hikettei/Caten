(defpackage :caten/codegen/test-suite
  (:use :cl :rove :caten :caten/nn :caten/air :caten/avm :trivia))
(in-package :caten/codegen/test-suite)
;; [TODO] Delete test-suite here
(defun equal-to (a) #'(lambda (x) (= x a)))
(defun pproceed (params tensor)
  (let ((mdl (caten tensor)))
    (apply #'forward mdl params)))
(defun elements (tensor) (buffer-value (tensor-buffer tensor)))
(defun n-kernels (avm)
  (declare (type avm avm))
  (count :JIT_KERNEL (graph-nodes (avm-graph avm)) :key #'node-type))
(defun n-args (shape avm)
  ;; shape ... (t t) specify t to match w/ anything
  ;; shape = t to any shape
  ;; shape = :tensor to enumerate tensors
  (declare (type avm avm))
  (count :Allocate (graph-nodes (avm-graph avm))
	 :test
	 #'(lambda (id node)
	     (and
              (eql id (node-type node))
              (or (eql shape t)
                  (when (eql shape :tensor)
                    (> (getattr node :nrank) 0))
		  (let* ((rank (getattr node :nrank))
		 	 (s1 (subseq (node-reads node) 0 rank)))
		    (and
                     (listp shape)
		     (= (length shape) (length s1))
		     (every
		      #'(lambda (x y) (or (eql x t) (equal x y)))
		      shape s1))))))))

(defun check-kernels (n avm)
  (if (= 1 (ctx:getenv :JIT))
      (ok (= n (n-kernels avm)) (format nil "got nkernels=~a (expected ~a)" (n-kernels avm) n))
      (skip "Needs JIT")))
(defun check-args (n shape avm)
  (if (= 1 (ctx:getenv :JIT))
      (ok (= n (n-args shape avm)) (format nil "got nargs=~a (expected ~a)" (n-args shape avm) n))
      (skip "Needs JIT")))
(defmacro with-jit-only-mode (&body body)
  `(if (= 1 (ctx:getenv :JIT))
       (progn ,@body)
       (skip "Requires JIT")))

(deftest matmul-schedule-test
  (with-no-grad
    (with-jit-only-mode
      (let* ((m (caten (!matmul (make-tensor `(3 10)) (make-tensor `(10 20)))))
	     (allocs (loop for node in (graph-nodes (avm-graph m))
			   if (eql (node-type node) :Allocate) collect node)))
	(ok (= (length allocs) 3) "gemm(a, b, c)")
	(ok (every #'(lambda (x) (if (= (getattr x :nrank) 2)
				     t
				     (if (= (getattr x :nrank) 3)
					 (some #'(lambda (x) (= x 1)) (subseq (node-reads x) 0 3))
					 nil)))
		   allocs)
	    "Contiguous array creations are not allowed")))))

(deftest embedding-schedule-test
  (testing "Embedding < 1 Kernels, < 3 Tensors."
    (with-no-grad
      (with-jit-only-mode
        (check-kernels 1 (caten (call (Embedding 100 100) (make-tensor `(100 100)))))
        (check-args 3 t (caten (call (Embedding 100 100) (make-tensor `(100 100)))))
        (check-kernels 3 (caten (call (Embedding 100 100) (make-tensor `(batch_size sentence_len)))))
        ;; 3 tensors for input/output/weight, 8 tensors for scalar (computing strides)
        (check-args 3 :tensor (caten (call (Embedding 100 100) (make-tensor `(batch_size sentence_len)))))))))

(deftest matmul-schedule-test
  (testing "Static Matmul"
    (flet ((f () (caten (!matmul (make-tensor `(10 20)) (!matmul (make-tensor `(20 30)) (make-tensor `(30 40)))))))
      (with-no-grad
        (with-jit-only-mode
          (check-kernels 2 (f))
          (check-args 5 :tensor (f))))))
  (testing "Symbolic Matmul"
    (flet ((f () (caten (!matmul (make-tensor `(a b)) (!matmul (make-tensor `(b c)) (make-tensor `(c d)))))))
      (with-no-grad
        (with-jit-only-mode
          (check-kernels 2 (f))
          (check-args 5 :tensor (f)))))))

;;(deftest symbolic-function-args-test
;;  (with-no-grad
;;    (when (= (ctx:getenv :JIT) 1)
;;      (let ((args (node-reads (get-jit-info (caten (!add (!view (make-tensor `(n)) `(froma toa bya)) (!view (make-tensor `(n)) `(fromb tob byb))))))))
;;	(ok (every #'(lambda (x) (find x args)) `(N FROMB TOB BYB TOA FROMA BYA)))
;;	(ok (= (length args) 9))))))


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
  ;;	 (out (elements (forward tensor `(a . 4) `(b . 8)))))
  ;;  (ok (= (length out) 128))
  ;;  (ok (every (equal-to (sin 4)) out)))
  )

;; TODO: Tensor-Shaped-Tesnor Operation
;; TODO: Tensor-Shaped-Tensor Iteration Rendering (The scalar result should be passed via arguments)
;; segv
#|
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

))
|#

