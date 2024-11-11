(cl:in-package :cl-user)
(defpackage :caten/nn
  (:documentation "Caten Neural Network Frontends
Policy:
  - We only provide the official implementation to established and well used nn modules.
  - You have to ensure that each files works standalone. (Use 1 file, 1 package coding rule at caten/nn)
  - Each module should be tested well (we expected to have a ton of regression tests here); 1 file 1 test-suite.
  - TODO: Add test-helpers.lisp")
  (:local-nicknames (:docs :caten/common.documentation))
  (:use :cl :caten :caten/air :alexandria)
  ;; from activations.lisp
  (:export
   #:Sigmoid
   #:!sigmoid

   #:ReLU
   #:!relu

   #:LeakyReLU
   #:!leaky-relu

   #:LogSoftmax
   #:!log-softmax

   #:ELU
   #:!elu

   #:ReLU6
   #:!relu6

   #:Softmax
   #:!softmax

   #:Softplus
   #:!softplus

   #:Softsign
   #:!softsign

   #:SoftShrink
   #:!softshrink

   #:GeLU
   #:!gelu

   #:SeLU
   #:!selu

   #:CeLU
   #:!celu

   #:LogSigmoid
   #:!logsigmoid

   #:SiLU
   #:!silu

   #:HardSwish
   #:!hardswish

   #:Mish
   #:!mish

   #:HardTanh
   #:!hardtanh

   #:Softmin
   #:!softmin
   )
  ;; from normalization.lisp
  (:export
   #:LayerNorm
   #:!layer-norm
   #:BatchNorm
   #:!batch-norm
   #:RMSNorm
   #:!rms-norm)
  ;; from embedding.lisp
  (:export
   #:Embedding)
  ;; from conv.lisp
  (:export
   #:ConvND
   #:!convnd
   #:convnd-weight
   #:convnd-bias)
  ;; from padding.lisp
  (:export
   #:!padding
   #:!padding2d)
  ;; from pool.lisp
  (:export
   #:AvgPool
   #:!avgpool
   #:MaxPool
   #:!maxpool)
  ;; from linear.lisp
  (:export
   #:Linear))

(in-package :caten/nn)

(defmacro slice (list upfrom &optional (below (length list)) (by 1))
  (with-gensyms (upfrom1 below1)
    `(let* ((redirect (signum ,by))
	    (,upfrom1 (if (>= ,upfrom 0) ,upfrom (+ (length ,list) ,upfrom)))
	    (,below1  (when ,below (if (>= ,below 0) ,below (+ (length ,list) ,below))))
	    (out
	      (loop for i upfrom ,upfrom1 below (or ,below1 (length ,list)) by (abs ,by) collect (nth i ,list))))
       (if (= 1 redirect)
	   out
	   (reverse out)))))

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

(defpackage :caten/nn.test
  (:use :cl :caten :caten/aasm :caten/nn :caten/avm :caten/air :rove :alexandria))
;; ~~ Unittest ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(in-package :caten/nn.test)

;; Common Utils for caten/nn.test
(defun elements (tensor) (buffer-value (tensor-buffer tensor)))
(defun n-kernels (avm)
  (declare (type avm avm))
  (count :JIT_KERNEL (graph-nodes (avm-graph avm)) :key #'node-type))
(defun n-args (shape avm)
  (declare (type avm avm))
  (count :Allocate (graph-nodes (avm-graph avm))
	 :test
	 #'(lambda (id node)
	     (and (eql id (node-type node))
		  (let ((rank (getattr node :nrank)))
		    (equal shape (subseq (node-reads node) 0 rank)))))))
(defun check-kernels (n avm)
  (if (= 1 (ctx:getenv :JIT))
      (ok (= n (n-kernels avm)))
      (skip "Needs JIT")))
(defun check-args (n shape avm)
  (if (= 1 (ctx:getenv :JIT))
      (ok (= n (n-args shape avm)))
      (skip "Needs JIT")))
(defun ~= (error) #'(lambda (x y) (<= (abs (- x y)) error)))

(defgeneric test/compile (op) (:documentation "Return a target compiled kernel."))
(defgeneric test/inputs  (op) (:documentation "Generate an array for testing"))
(defgeneric test/compute-in-caten (op avm &rest inputs) (:documentation "Compute the avm, and test the accuracy."))
(defgeneric test/compute-in-lisp  (op avm &rest inputs) (:documentation ""))
(defgeneric test/in-place         (op avm))
(defgeneric test/kernel-count     (op avm))
(defgeneric test/assert-close (op result1 result2))
(defgeneric test/run (op))
(defun make-copy (tensor)
  (declare (type tensor tensor))
  (ctx:with-contextvar (:JIT 0)
    ;; [Note] Assuming JIT will not perform in-place mutation
    (proceed (!add tensor (fconst 0 :dtype (dtype-of tensor))))))
(defmacro define-nn-test (name description
			  &key
			    (dtypes `(:float32))
			    (orders `(:row :column))
			    (compile) (inputs) (caten) (lisp)
			    (assert-close) (in-place) (kernel)
			  &aux
			    (name (intern (symbol-name name) "KEYWORD")))
  (with-gensyms (op avm args result1 result2)
    `(progn
       (defmethod test/compile ((,op (eql ,name))) ,compile)
       (defmethod test/inputs ((,op (eql ,name))) ,inputs)
       (defmethod test/compute-in-caten ((,op (eql ,name)) ,avm &rest ,args)
	 (multiple-value-bind (,@(car caten)) (apply #'values ,avm ,args) ,@(cdr caten)))
       (defmethod test/compute-in-lisp ((,op (eql ,name)) ,avm &rest ,args)
	 (ctx:with-contextvar (:jit 0 :avm :lisp) ;; Allowed to use Custom/LazyApply if element-wise
	   (multiple-value-bind (,@(car lisp)) (apply #'values ,avm ,args)
	     (declare (ignorable ,@(car lisp)))
	     ,@(cdr lisp))))
       (defmethod test/in-place     ((,op (eql ,name)) ,avm) (let ((,(caar in-place) ,avm)) ,@(cdr in-place)))
       (defmethod test/kernel-count ((,op (eql ,name)) ,avm) (let ((,(caar kernel) ,avm)) ,@(cdr kernel)))
       (defmethod test/assert-close ((,op (eql ,name)) ,result1 ,result2)
	 (multiple-value-bind (,@(car assert-close)) (values ,result1 ,result2)
	   ,@(cdr assert-close)))
       (defmethod test/run ((,op (eql ,name)))
	 (testing ,description
	   (let ((model (testing "1. Compiling the kernel..." (test/compile ,name))))
	     (ok model "Getting a compiled avm")
	     (let ((inputs (testing "2. Generating the input..." (test/inputs ,name))))
	       (ok inputs "Prepared for inputs")
	       (let* ((caten (testing "3. Computing the kernel in Caten" (apply #'test/compute-in-caten ,name model (map 'list #'make-copy inputs))))
		      (lisp  (testing "4. Computing the kernel in Lisp"  (apply #'test/compute-in-lisp ,name model inputs)))
		      (accuracy (testing "5. Comparing the two results..." (test/assert-close ,name caten lisp))))
		 (ok accuracy "Satisfying the accuracy.")
		 (if (= 0 (ctx:getenv :JIT))
		     (skip "Requires JIT")
		     (let ((in-place (testing "5. Testing the in-place mutation" (test/in-place ,name model))))
		       (ok in-place "Satisfying the in-place test.")
		       (let ((kernel-count (testing "6. Testing the kernel count" (test/kernel-count ,name model))))
			 (ok kernel-count "Satisfying the kernel count test.")))))))))
       (deftest ,(intern (format nil "~a" name))
	 (dolist (*default-float* ',dtypes)
	   (dolist (*default-order* ',orders)
	     (testing (format nil "Testing w/ dtype=~a, order=~a" *default-float* *default-order*)
	       (test/run ,name))))))))

;; ~~ Custom Kernel for calling element-wise lisp kernel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defnode (:Testing :Test/Lisp-Lazy-Apply) () "" :slots ((f)))
(defclass Custom/LazyApply (Func) ((f :initarg :f :accessor lazyapply-f))
  (:documentation "This custom op is dedicated to testing, only supported in Lisp VM"))
(defmethod forward ((op Func) &rest tensors) (st "A[~] -> A[~]" (tensors)))
(defmethod backward ((op Func) &optional dout) (declare (ignore dout)) nil)
(defmethod lower ((op Func) &rest nodes)
  (with-context (_ (emit (make-node :Testing :Test/Lisp-Lazy-Apply (list (gensym)) (map 'list #'node->id nodes) :f (lazyapply-f op))))))
(defmethod %impl ((device (eql :lisp)) (op (eql :Test/Lisp-Lazy-Apply)) graph node args) (apply #'map-view nil (getattr node :f) args))
(defun lazy-lisp (f tensor)
  (declare (type function f) (type tensor tensor))
  (forward (make-instance 'Custom/LazyApply :f f) tensor))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; TODO: Implement assert-close like numpy's one.
