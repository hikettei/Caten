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



