(in-package :caten/apis)
;; initializers.lisp: Implements the lazy (or static) initialization of dense/sparse arrays.
;; How randomness should be implemented under the nature of laziness:
;;   All computational nodes that exhibit random behavior must depend on `RandNode`.
;;   it implements PRNG Generator based on threefry2x32.
;;   *rng-counter* and *manual-seed* should be depended upon commonly by all graphs.
;; ~~ randomness ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-rng-counter () (proceed (make-tensor `(1) :dtype :uint32 :id '_rng_counter)))
(defparameter *manual-seed* 0)
(defparameter *rng-counter* (make-rng-counter))
(defun set-manual-seed (&key (seed 0))
  "Sets the seed for random operations."
  (setf *manual-seed* seed *rng-counter* (make-rng-counter)))
(defmacro with-manual-seed ((seed) &body body) `(let ((*manual-seed* ,seed) (*rng-counter* (make-rng-counter))) ,@body))
;; ~~~~ threefry2x32 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Introduce IDIV
(defun !idiv (x y)  (!cast (!div (!cast x :float32) (!cast y :float32)) (dtype-of x)))
(defun !idiv1 (x divisor) (!mul (!cast x :float32) (fconst (/ divisor))))
(defun !shr (x shift dtype) (!cast (!idiv1 x (expt 2 shift)) dtype))
(defun !threefry2x32 (x seed &aux (rotations `((13 15 26 6) (17 29 16 24))) (*wrap-around-mode* t))
  "Implements threefry2x32
- Paper: https://www.thesalmons.org/john/random123/papers/random123sc11.pdf"
  (declare (type fixnum seed))
  (multiple-value-bind (seed x0 x1)
      (values
       (uconst seed :dtype :uint32)
       (!cast (!and x (uconst 4294967295 :dtype :uint64)) :uint32)
       (!cast (!and (!idiv x (uconst (expt 2 32) :dtype :uint64)) (uconst 4294967295 :dtype :uint64)) :uint32))
    (let* ((ks (list (uconst 0 :dtype :uint32) (!xor seed (uconst 466688986 :dtype :uint32)) seed))
	   (xr (list (!add x0 (nth 2 ks)) (!add x1 (car ks)))))
      (dotimes (i 5)
	(dolist (r (nth (mod i 2) rotations))
	  (setf x0 (!add (car xr) (second xr))
		(nth 0 xr) x0
	        (nth 1 xr) (!xor x0 (!add (!mul (nth 1 xr) (uconst (expt 2 r) :dtype :uint32)) (!cast (!idiv1 (nth 1 xr) (expt 2 (- 32 r))) :uint32)))))
	(setf xr (list (!add (first xr) (nth (mod i 3) ks)) (!add (second xr) (!add (nth (mod (1+ i) 3) ks) (uconst (1+ i) :dtype :uint32))))))
      (!or (!mul (!cast (second xr) :uint64) (uconst (expt 2 32) :dtype :uint64)) (!cast (car xr) :uint64)))))

;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmodel (Threefry2x32-Random () :where "A[~] -> A[~]"
				  :documentation "Generates a random array sampled from a uniform distribution in the range of [0.0, 1.0)")
    ())
(defmethod call ((op Threefry2x32-Random) &rest inputs)
  (st "A[~] -> A[~]" (inputs))
  (let* ((x (car inputs))
	 (rng-counter (!add *rng-counter* (apply #'!* (map 'list #'->iconst (shape x))) :reduce t)))
    ;; [FixME] Currently slice generates two kernels, so our implementation discards count1 of threefry2x32.
    (let* ((num (reduce #'!mul (map 'list #'->iconst (shape x))))
	   ;; FixME: wrapping num w/ !ceiling will occur a compile error.
	   ;;(size (!add (iconst 1) (!cast (!div (!cast num *default-float*) (fconst 2)) :uint32)))
	   (size num)
	   (counts1 (make-tensor (list size) :dtype :uint32))
	   (counts1 (!add (!index-components counts1) rng-counter))
	   (counts2 (!add counts1 size))
	   (x1 (!or (!mul (!cast counts2 :uint64) (iconst (expt 2 32) :dtype :uint64)) counts1))
	   (x1 (!threefry2x32 x1 *manual-seed*))
	   ;; (counts1 (!cast (!and x (uconst 4294967295)) :uint32))
	   (counts2 (!and (!shr x1 32 :uint32) (uconst 4294967295 :dtype :uint32)))
	   (cc counts2)
	   (cc (!shr cc 8 :float32))
	   (cc (!div cc (fconst (expt 2 24)))))
      (!reshape cc (shape x)))))

(defmodel (Gaussian-Distribution-Node () :where "A[~] -> A[~]") ())
(defmethod call ((op Gaussian-Distribution-Node) &rest inputs)
  ;; https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
  (st "A[~] -> A[~]" (inputs))
  (multiple-value-bind (x) (apply #'values inputs)
    (let* ((source (!rand `(2 ,@(shape x)) :dtype :float32 :order (order x)))
	   (mapper #'(lambda (n) `(,n ,@(loop for s in (shape x) collect t)))))
      (!cast
       (!reshape
	(!*
	 (!cos (!mul (apply #'!view source (funcall mapper 0)) (fconst (* pi 2) :dtype :float32)))
	 (!sqrt (!mul (!log (!sub (fconst 1 :dtype :float32) (apply #'!view source (funcall mapper 1)))) (fconst -2 :dtype :float32))))
	(shape x))
       (dtype-of x)))))

(defmodel (Random-Normal () :where "A[~] STD[] MEAN[] -> A[~]") ())
(defmethod call ((op Random-Normal) &rest inputs)
  (st "A[~] STD[] MEAN[] -> A[~]" (inputs))
  (multiple-value-bind (x std mean) (apply #'values inputs)
    (!add (!mul (!randn (shape x) :dtype (dtype-of x) :order (order x) :out x) std) mean)))

(defmodel (Uniform-Random () :where "A[~] Upfrom[] Below[] -> A[~]") ())
(defmethod call ((op Uniform-Random) &rest inputs)
  (st "A[~] Upfrom[] Below[] -> A[~]" (inputs))
  (multiple-value-bind (x upfrom below) (apply #'values inputs)
    (!add (!cast (!mul (!sub below upfrom) (!rand (shape x) :dtype (dtype-of x) :order (order x) :out x)) (dtype-of x)) upfrom)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Linspace (Func) nil
  (:documentation "Generates an array sampled from this formula: x_i = a * index_components(i) + b"))
(defmethod forward ((op Linspace) &rest inputs) (st "A[] B[] X[~] -> X[~]" (inputs)))
(defmethod backward ((op Linspace) &optional dout) (values nil nil dout))
(defmethod lower ((op Linspace) &rest inputs)
  (multiple-value-bind (a b x) (apply #'values inputs)
    (with-context
      (i (%index-components x (%shape (shape (third (func-variables op))))))
      (t1 (%mul i a))
      (t2 (%add t1 b))
      (c  (%store x t2)))))
;; ~~ callers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ax+b (shape a b &key (out nil) (dtype *default-float*) (order *default-order*))
  "Initializes a tensor"
  (declare (type list shape))
  (flet ((->val (x) (->const x #'(lambda (x) (make-scalar x :dtype dtype :order order)))))
    (forward (make-instance 'Linspace) (->val a) (->val b) (or out (make-tensor shape :dtype dtype :order order)))))

(defun !rand (shape &key (dtype *default-float*) (order *default-order*) (out nil))
  "Initializes a tensor with randomly sampled from [0, 1)"
  (forward (Threefry2x32-Random) (or out (make-tensor shape :dtype dtype :order order))))

(defun !normal (shape &key (mean 0.0) (std 1.0) (dtype *default-float*) (order *default-order*) (out nil))
  "Initializes a tensor, filled with random value sampled from a normal distribution."
  (declare (type (or Tensor symbol number) mean std) (type list shape))
  (flet ((->cast (x) (->const x #'(lambda (x) (fconst x :dtype dtype)))))
    (forward (make-instance 'Random-Normal)
	     (or out (make-tensor shape :dtype dtype :order order))
	     (->cast std) (->cast mean))))

(defun !randn (shape &key (dtype *default-float*) (order *default-order*) (out nil))
  (forward (make-instance 'Gaussian-Distribution-Node) (or out (make-tensor shape :dtype dtype :order order))))

(defun !uniform (shape &key (upfrom 0.0) (below 1.0) (dtype *default-float*) (order *default-order*) (out nil))
  (flet ((->cast (x) (->const x #'(lambda (x) (fconst x :dtype dtype)))))
    (forward (make-instance 'Uniform-Random) (or out (make-tensor shape :dtype dtype :order order)) (->cast upfrom) (->cast below))))

(defun !randint (shape &key (upfrom 0) (below 1) (dtype *default-int*) (order *default-order*) (out nil))
  (flet ((->cast (x) (->const x #'(lambda (x) (fconst x :dtype dtype)))))
    (forward (make-instance 'Uniform-Random) (or out (make-tensor shape :dtype dtype :order order)) (->cast upfrom) (->cast below))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; TODO: Xavier/He/Orthogonal etc ...
;; TODO: (parameter x :requires-grad t :id xx)
;; TODO: Pre-compile the function (symbolic!)
;; (export-avm :clang avm)
(caten/defun[float] ($random "random") (n) (!rand `(,n)))
