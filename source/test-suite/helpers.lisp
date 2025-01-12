(in-package :caten/test-suite)

(defmacro range (from below &optional (by 1)) `(loop for i from ,from below ,below by ,by collect i))

(defparameter *torch-dtype* "float32")
(defparameter *caten-dtype* :float32)

(defun ->numpy (tensor &key (dtype *torch-dtype*))
  (declare (type tensor tensor))
  (assert (buffer-value (tensor-buffer tensor)) () "The tensor ~a is not realized yet!." tensor)
  (np:reshape
   (np:array
    (elements tensor)
    :dtype
    dtype)
   (buffer-shape (tensor-buffer tensor))))

(defun sync-visible-size (tensor)
  (when (arrayp (buffer-value (tensor-buffer tensor)))
    (when (not (= (apply #'* (buffer-shape (tensor-buffer tensor))) (array-total-size (buffer-value (tensor-buffer tensor)))))
      (setf tensor (proceed (!copy tensor)))))
  tensor)

(defun ->torch (tensor &key (dtype *torch-dtype*))
  (remote-objects (torch.from_numpy (->numpy (sync-visible-size tensor) :dtype dtype))))

(defun torch-shape (tensor) (remote-objects* (py.list (chain tensor (size)))))

(defun ->caten (tensor &key (dtype *caten-dtype*))
  (let* ((size (coerce (torch-shape tensor) 'list))
 	 (buffer (remote-objects* (chain tensor (detach) (cpu) (flatten) (numpy))))
	 (buffer-out (make-buffer size (caten/api::row-major-calc-strides size) dtype nil :device (caten/codegen/backend:get-buffer-type))))
    (open-buffer (get-global-runtime) buffer-out)
    (transfer-from-array (get-global-runtime) buffer-out buffer)
    (let ((new (make-tensor (coerce size 'list) :dtype dtype :order :row)))
      (setf (tensor-buffer new) buffer-out)
      (make-param #'(lambda (_) _ new) size :dtype dtype :order :row))))
;; Utils for testing the generated kernel
(defun elements (tensor)
  "The function elements refers to the buffer of the given tensor, returning simple-array. (if the buffer was not on the cpu they are transferred)"
  (declare (type tensor tensor))
  ;; Always returns simple-array
  (transfer-into-array (tensor-buffer tensor)))

(defun n-kernels (runtime)
  (declare (type GraphRuntime runtime))
  (count :JIT_KERNEL (graph-nodes (runtime-graph runtime)) :key #'node-type))

(defun n-args (shape runtime)
  ;; shape ... (t t) specify t to match w/ anything
  ;; shape = t to any shape
  ;; shape = :tensor to enumerate tensors
  (declare (type GraphRuntime runtime))
  (count :Allocate (graph-nodes (runtime-graph runtime))
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

(defun check-kernels (n runtime)
  (if (caten/codegen/backend:jit-mode-p)
      (ok (= n (n-kernels runtime)) (format nil "got nkernels=~a (expected ~a)" (n-kernels runtime) n))
      (skip "Needs JIT")))

(defun check-args (n shape runtime)
  (if (caten/codegen/backend:jit-mode-p)
      (ok (= n (n-args shape runtime)) (format nil "got nargs=~a (expected ~a)" (n-args shape runtime) n))
      (skip "Needs JIT")))

(defun transfer-to-lisp (tensor)
  (ctx:with-contextvar (:BACKEND "LISP")
    (!reshape (change-facet (change-facet tensor :array) :tensor) (shape tensor))))

(defun ~= (error) #'(lambda (x y) (<= (abs (- x y)) error)))

(defgeneric test/compile (op) (:documentation "Return a target compiled kernel."))
(defgeneric test/inputs  (op) (:documentation "Generate an array for testing"))
(defgeneric test/compute-in-caten (op runtime &rest inputs) (:documentation "Compute the runtime, and test the accuracy."))
(defgeneric test/compute-in-lisp  (op runtime &rest inputs) (:documentation ""))
(defgeneric test/in-place         (op runtime))
(defgeneric test/kernel-count     (op runtime))
(defgeneric test/assert-close (op result1 result2))
(defgeneric test/run (op))
(defun make-copy (tensor)
  (declare (type tensor tensor))
  (proceed (!copy tensor)))

(defmacro define-nn-test (name description
			  &key
			    (dtypes `(:float32))
			    (orders `(:row :column))
			    (compile) (inputs) (caten) (lisp)
			    (assert-close) (in-place) (kernel)
			  &aux
			    (name (intern (symbol-name name) "KEYWORD")))
  (with-gensyms (op runtime args result1 result2)
    `(progn
       (defmethod test/compile ((,op (eql ,name))) ,compile)
       (defmethod test/inputs ((,op (eql ,name))) ,inputs)
       (defmethod test/compute-in-caten ((,op (eql ,name)) ,runtime &rest ,args)
	 (multiple-value-bind (,@(car caten)) (apply #'values ,runtime ,args) ,@(cdr caten)))
       (defmethod test/compute-in-lisp ((,op (eql ,name)) ,runtime &rest ,args)
	 (ctx:with-contextvar (:BACKEND "LISP") ;; Allowed to use Custom/LazyApply if element-wise
	   (multiple-value-bind (,@(car lisp)) (apply #'values ,runtime (map 'list #'transfer-to-lisp ,args))
	     (declare (ignorable ,@(car lisp)))
	     ,@(cdr lisp))))
       (defmethod test/in-place     ((,op (eql ,name)) ,runtime) (let ((,(caar in-place) ,runtime)) ,@(cdr in-place)))
       (defmethod test/kernel-count ((,op (eql ,name)) ,runtime) (let ((,(caar kernel) ,runtime)) ,@(cdr kernel)))
       (defmethod test/assert-close ((,op (eql ,name)) ,result1 ,result2)
	 (multiple-value-bind (,@(car assert-close)) (values ,result1 ,result2)
	   ,@(cdr assert-close)))
       (defmethod test/run ((,op (eql ,name)))
	 (testing ,description
	   (let ((model (testing "1. Compiling the kernel..." (test/compile ,name))))
	     (ok model "Getting a compiled runtime")
	     (let ((inputs (testing "2. Generating the input..." (test/inputs ,name))))
	       (ok inputs "Prepared for inputs")
	       (let* ((caten (testing "3. Computing the kernel in Caten" (apply #'test/compute-in-caten ,name model (map 'list #'make-copy inputs))))
		      (lisp  (testing "4. Computing the kernel in Lisp"  (apply #'test/compute-in-lisp ,name model inputs)))
		      (accuracy (testing "5. Comparing the two results..." (test/assert-close ,name caten lisp))))
		 (ok accuracy "Satisfying the accuracy.")
		 (if (null (caten/codegen/backend:jit-mode-p))
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

(defmacro with-torch ((&rest tensors) &body body)
  `(let (,@(loop for tensor in tensors
		 collect `(,tensor (->torch ,tensor))))
     ,@body))

(defmacro with-dtype ((caten-dtype torch-dtype) &body body)
  `(let ((*caten-dtype* ,caten-dtype)
	 (*torch-dtype* ,torch-dtype))
     ,@body))

(defun compute-rtol-atol (array1 array2)
  (declare (type simple-array array1 array2))
  (assert (= (array-total-size array1) (array-total-size array2))
	  ()
	  "The length of two arrays does not match: ~a vs ~a"
	  (array-total-size array1) (array-total-size array2))
  (let ((max-abs-diff 0.0)
        (max-rel-diff 0.0))
    (dotimes (i (array-total-size array1))
      (let* ((val1 (aref array1 i))
             (val2 (aref array2 i))
             (abs-diff (abs (- val1 val2)))
             (denominator (max (abs val1) (abs val2) 1e-8))
             (rel-diff (/ abs-diff denominator)))
        (when (> abs-diff max-abs-diff)
          (setf max-abs-diff abs-diff))
        (when (> rel-diff max-rel-diff)
          (setf max-rel-diff rel-diff))))
    (values max-abs-diff max-rel-diff)))

(defmacro assert-equal ((&key (rtol 1e-7) (atol 0.0)) torch-form lisp-form)
`(let ((torch ,torch-form) (lisp ,lisp-form))
   (ok (equal (shape torch) (shape lisp)) "Shapes match")
   (multiple-value-bind (atol1 rtol1) (compute-rtol-atol (elements (sync-visible-size torch)) (elements (sync-visible-size lisp)))
     (ok (<= atol1 ,atol) (format nil "Satisfying (atol=~a) <= ~a" atol1 ,atol))
     (ok (<= rtol1 ,rtol) (format nil "Satisfying (rtol=~a) <= ~a" rtol1 ,rtol)))))

(defmacro assert-equals ((&key (rtol 1e-7) (atol 0.0)) torch-form lisp-form)
  `(let ((torch (multiple-value-list ,torch-form)) (lisp (multiple-value-list ,lisp-form)))
     (assert (= (length torch) (length lisp)) () "assert-equals: The number of tensors does not match.")
     (ok (every #'(lambda (x y) (equal (shape x) (shape y))) torch lisp) "Shapes match")
     (loop for torchi in torch
           for lispi in lisp do
             (multiple-value-bind (atol1 rtol1) (compute-rtol-atol (elements (sync-visible-size torchi)) (elements (sync-visible-size lispi)))
               (ok (<= atol1 ,atol) (format nil "Satisfying (atol=~a) <= ~a" atol1 ,atol))
               (ok (<= rtol1 ,rtol) (format nil "Satisfying (rtol=~a) <= ~a" rtol1 ,rtol))))))

(defmacro with-given-dtype ((&rest dtypes) &body body)
  `(loop for (*caten-dtype* . *torch-dtype*) in ',dtypes
         for *default-float* = *caten-dtype*
         do (testing (format nil "dtype=~a" *caten-dtype*) ,@body)))
