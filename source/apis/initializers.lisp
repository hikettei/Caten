(in-package :caten/apis)

(defclass Linspace (Func) nil)
(defmethod forward ((op Linspace) &rest inputs) (st "A[] B[] X[~] -> X[~]" (inputs)))
(defmethod backward ((op Linspace) &optional dout) (values nil nil dout))
(defmethod lower ((op Linspace) &rest inputs)
  (multiple-value-bind (a b x) (apply #'values inputs)
    (with-context
      (i (%index-components x))
      (t1 (%mul i a))
      (t2 (%add t1 b))
      (c  (%store x t2)))))
(defun ax+b (shape a b &key (out nil) (dtype *default-float*) (order *default-order*))
  "Initializes a tensor"
  (declare (type list shape))
  (flet ((->val (x) (->const x #'(lambda (x) (make-scalar x :dtype dtype :order order)))))
    (forward (make-instance 'Linspace) (->val a) (->val b) (or out (make-tensor shape :dtype dtype :order order)))))

;; ~~ random ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-rng-counter () (proceed (make-tensor `(1) :dtype :uint32 :id '_rng_counter)))
(defparameter *manual-seed* 0)
(defparameter *rng-counter* (make-rng-counter))
(defun set-manual-seed (&key (seed 0))
  "Sets the seed for random operations."
  (setf *manual-seed* seed *rng-counter* (make-rng-counter)))
(defmacro with-manual-seed ((seed) &body body) `(let ((*manual-seed* ,seed) (*rng-counter* (make-rng-counter))) ,@body))
;; ~~ helpers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %autocast (shape x dtype) (%cast (%make-tensor (%shape shape) :dtype dtype) x dtype))
(defun %idiv (shape x divisor) (%mul (%autocast shape x *default-float*) (%recip (%autocast shape divisor *default-float*))))
(defun %idiv1 (shape x divisor)
  (declare (type number divisor))
  (%mul (%autocast shape x *default-float*) (%fconst (/ divisor))))
(defun %shr (shape x shift dtype) (%autocast shape (%idiv1 shape x (expt 2 shift)) dtype))
(defun %trunc (shape x) (%autocast shape (%autocast shape x :uint32) :float32))
(defun %broadcast-to (shape value &key (dtype :uint64) (skip-load nil))
  (when (null shape) (return-from %broadcast-to (if skip-load value (%iconst value :dtype dtype))))
  (let ((x (if skip-load value (%load (%make-tensor `(1) :dtype dtype) value))))
    (%view x shape
	   (loop for s in shape collect (%iconst 0))
	   (loop for s in shape collect s)
	   (loop for s in shape collect (%iconst 1))
	   (loop for s in shape collect nil)
	   (loop for s in shape collect (%iconst 0)))))
(defun %ceiling (shape x &aux (trunc (%trunc shape x))) (%autocast shape (%where (%> shape *default-order* x trunc) (%add trunc (%broadcast-to shape 1)) trunc) :uint32))
(defun %cat (size dtype a b)
  "Conatenates two 1d tensors a and b"
  (let* ((2xsize (%add size size))
	 (after (%make-tensor (list 2xsize) :dtype dtype))
	 (a (%move (%view after (list size) (list (%iconst 0)) (list size) (list (%iconst 1)) (list nil) (list (%iconst 1))) a))
	 (b (%move (%view a (list size) (list size) (list 2xsize) (list (%iconst 1)) (list nil) (list (%iconst 1))) b)))
    (%view b (list 2xsize) (list (%iconst 0)) (list 2xsize) (list (%iconst 1)) (list nil) (list (%iconst 1)))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %threefry2x32 (size x seed &aux (rotations `((13 15 26 6) (17 29 16 24))) (*wrap-around-mode* t))
  "Implements threefry2x32
- Paper: https://www.thesalmons.org/john/random123/papers/random123sc11.pdf"
  (declare (type list size) (type fixnum seed))
  (multiple-value-bind (seed x0 x1)
      (values
       (%uconst seed :dtype :uint32)
       (%autocast size (%and x (%broadcast-to size 4294967295)) :uint32)
       (%autocast size (%and (%autocast size (%idiv size x (%broadcast-to size (expt 2 32))) :uint64) (%broadcast-to size 4294967295 :dtype :uint64)) :uint32))
    (let* ((ks (list (%uconst 0 :dtype :uint32) (%xor seed (%uconst 466688986 :dtype :uint32)) seed))
	   (xr (list (%add x0 (nth 2 ks)) (%add x1 (car ks)))))
      (dotimes (i 5)
	(dolist (r (nth (mod i 2) rotations))
	  (setf x0 (%add (car xr) (second xr))
		(nth 0 xr) x0
	        (nth 1 xr) (%xor x0 (%add (%mul (nth 1 xr) (%uconst (expt 2 r) :dtype :uint32))
					  (%autocast size (%idiv1 size (nth 1 xr) (expt 2 (- 32 r))) :uint32)))))
	(setf xr (list (%add (first xr) (nth (mod i 3) ks)) (%add (second xr) (%add (nth (mod (1+ i) 3) ks) (%uconst (1+ i) :dtype :uint32))))))
      (%or (%mul (%autocast size (second xr) :uint64) (%uconst (expt 2 32) :dtype :uint64)) (%autocast size (car xr) :uint64)))))
(defclass RandNode (Func) ((shape :initform nil :accessor rand-shape))
  (:documentation "Initializes a uniform random tensor sampled from [0, 1)"))
(defmethod forward ((op RandNode) &rest inputs) (st "A[~] Counter[x] -> A[~]" (inputs) (:x . 1)))
(defmethod backward ((op RandNode) &optional dout) (declare (ignore op dout)))
(defmethod lower ((op RandNode) &rest inputs)
  (multiple-value-bind (x rng-counter) (apply #'values inputs)
    (declare (ignore x))
    (multiple-value-bind (xt) (apply #'values (func-variables op))
      (with-context
	(base-shape (%shape (shape xt)))
	(num (reduce #'%mul base-shape))
	(rng-counter (%add rng-counter num :reduction t))
	(size (%ceiling nil (%idiv nil num (%iconst 2))))
	(counts1 (%make-tensor (list size) :dtype :uint32))
	(counts1 (%index-components counts1))
	(counts2 (%add counts1 size))
	(counts1_64 (%make-tensor (list size) :dtype :uint64))
	(counts2_64 (%make-tensor (list size) :dtype :uint64))
	(x (%or (%mul (%cast counts2_64 counts2 :uint64) (%broadcast-to (list size) (expt 2 32))) counts1_64))
        (x (%threefry2x32 (list size) x *manual-seed*))
	(counts1 (%autocast (list size) (%and x (%uconst 4294967295 :dtype :uint32)) :uint32))
	(counts2 (%autocast (list size) (%and (%shr (list size) x 32 :uint32) (%uconst 4294967295 :dtype :uint32)) :uint32))
	(2xsize (%add size size))
	(cc (%shr (list 2xsize) (%cat size :uint32 counts1 counts2) 8 :float32))
	(cc (%div cc (%fconst (expt 2 24))))
	(cc (%view cc (list num) (list (%iconst 0)) (list num) (list (%iconst 1)) (list nil) (list (%iconst 1))))
	(cc (%reshape cc base-shape :order (order xt)))
	(cc (if (eql (dtype-of xt) :float32) cc (%autocast base-shape cc (dtype-of xt))))))))

(defun !rand (shape &key (dtype *default-float*) (order *default-order*))
  "Initializes a tensor with randomly sampled from [0, 1)"
  (forward (make-instance 'RandNode) (make-tensor shape :dtype dtype :order order) *rng-counter*))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;; rand
;; beta
;; Ref: https://dl.acm.org/doi/pdf/10.1145/359460.359482
;; gamma
;; chisquare
;; xavier
;; uniform
;; randint
;; AxpyみたいにCompileした物を事前にLoadしておく
