(cl:in-package :cl-user)
(defpackage :caten/common.dtype
  (:documentation "")
  (:use :cl :alexandria)
  (:export
   #:dtype-t
   #:dtype->lisp
   #:lisp->dtype
   #:dtype/cast
   #:dtype/floatp
   #:dtype/integerp
   #:dtype/uintegerp
   #:dtype/max
   #:dtype/min
   #:dtype/min
   #:dtype/smallest
   #:dtype/size-of

   #:encode-float64
   #:decode-float64
   #:encode-float32
   #:decode-float32
   #:encode-float16
   #:decode-float16
   #:encode-bfloat16
   #:decode-bfloat16
   
   #:forall
   #:1.0ulp
   #:3.5ulp
   #:forall
   ))
(in-package :caten/common.dtype)
;; TODO: Add complex32/complex64 to implement HiFi-GAN
(deftype dtype-t ()
  "A list of available keywords as a dtype"
  `(and keyword
	(member
	 :float64 :float32 :float16 :bfloat16
	 :uint64 :uint32 :uint16 :uint8
	 :int64 :int32 :int16 :int8 :bool)))

(defun dtype->lisp (dtype)
  "to which dtypes values are coerced?"
  (case dtype
    (:float64 'double-float)
    (:float32 'single-float)
    (:float16 'single-float)
    (:bfloat16 'single-float)
    (:uint64  '(unsigned-byte 64))
    (:int64   '(signed-byte 64))
    (:uint32  '(unsigned-byte 32))
    (:int32   '(signed-byte 32))
    (:uint16  '(unsigned-byte 16))
    (:int16   '(signed-byte 16))
    (:uint8   '(unsigned-byte 8))
    (:int8    '(signed-byte 8))
    (:bool    'boolean)
    (otherwise (error "dtype->lisp: ~a is not supported" dtype))))

(defun lisp->dtype (lisp-type)
  "Does the opposite thing of dtype->lisp"
  (cond
    ((eql lisp-type 'double-float) :float64)
    ((eql lisp-type 'single-float) :float32)
    ((equal lisp-type '(signed-byte 8)) :int8)
    ((equal lisp-type '(unsigned-byte 8)) :uint8)
    ((equal lisp-type '(unsigned-byte 16)) :uint16)
    ((equal lisp-type '(signed-byte 16)) :int16)
    ((equal lisp-type '(unsigned-byte 32)) :uint32)
    ((equal lisp-type '(signed-byte 32)) :int32)
    ((equal lisp-type '(unsigned-byte 64)) :uint64)
    ((equal lisp-type '(signed-byte 64)) :int64)
    ((eql lisp-type 'boolean) :bool)
    (t (error "Cannot convert ~a to dtype" lisp-type))))

(defun dtype/floatp (dtype)
  (declare (type dtype-t dtype))
  (find dtype `(:float64 :float32 :float16 :bfloat16)))
(defun dtype/uintegerp (dtype) (find dtype `(:uint64 :uint32 :uint16 :uint8)))
(defun dtype/integerp (dtype) (find dtype `(:int64 :int32 :int16 :int8 :uint8 :uint16 :uint32 :uint64)))

(defun dtype/cast (x cast-to)
  "Casts the given number x into cast-to"
  (declare (type (or boolean number) x)
	   (type dtype-t cast-to))
  (when (and (typep x 'boolean) (eql cast-to :bool)) (return-from dtype/cast x))
  (let ((x (if (typep x 'boolean)
	       (if x 1 0)
	       x)))
    (if (or (typep x 'ratio) (floatp x))
	(if (or (eql cast-to :float64) (eql cast-to :float32) (eql cast-to :float16) (eql cast-to :bfloat16))
	    (coerce x (dtype->lisp cast-to))
	    (coerce (truncate x) (dtype->lisp cast-to)))
	(coerce x (dtype->lisp cast-to)))))

(defun 1.0ulp (dtype)
  (declare (type dtype-t dtype))
  (if (dtype/integerp dtype)
      0
      (ecase dtype
	(:float64 2.220446049250313e-16)
	(:float32 1.1920928955078125e-07)
	(:float16 0.0009765625)
	(:bfloat16 0.0009765625))))
(defun 3.5ulp (dtype) (* 3.5 (1.0ulp dtype)))
(defun dtype/max (dtype)
  (ecase dtype
    (:float64 3.40282e+38) ;;1.79769313486232d+308
    (:float32 3.40282e+38)
    (:float16 6.55040e+04)
    (:bfloat16 6.55040e+04)    
    (:uint64 (1- (expt 2 64)))
    (:int64  (1- (expt 2 63)))
    (:uint32 (1- (expt 2 32)))
    (:int32  (1- (expt 2 31)))
    (:uint16 (1- (expt 2 16)))
    (:int16  (1- (expt 2 15)))
    (:uint8  (1- (expt 2 8)))
    (:int8   (1- (expt 2 7)))
    (:bool   t)))

(defun dtype/min (dtype)
  (case dtype
    (:uint64 0)
    (:uint32 0)
    (:uint16 0)
    (:uint8  0)
    (:bool nil)
    (otherwise (- (dtype/max dtype)))))

(defun dtype/smallest (dtype)
  (case dtype
    (:float64 2.2250738585072e-308)
    (:float32
     ;; Actual FLT_MIN is 1.17549435e-38 but MTL Symbolic cannot handle it
     1.17549e-37)
    (:float16 6.104e-05)
    (:bfloat16 6.104e-05)    
    (:bool nil)
    (otherwise 0)))

(defun dtype/size-of (dtype)
  "Return: bit"
  (ecase dtype
    (:float64 64)
    (:float32 32)
    (:float16 16)
    (:bfloat16 16)
    (:uint64 64)
    (:int64  64)
    (:uint32 32)
    (:int32 32)
    (:int16 16)
    (:uint16 16)
    (:uint8 8)
    (:int8 8)
    (:bool 1)))

(declaim (inline caten/common.dtype:encode-float64 caten/common.dtype:decode-float32 caten/common.dtype:encode-float64 caten/common.dtype:decode-float64))
(ieee-floats:make-float-converters encode-float64 decode-float64 11 52 t)
(ieee-floats:make-float-converters encode-float32 decode-float32 8 23 t)
(ieee-floats:make-float-converters encode-float16 decode-float16 5 10 t)
(ieee-floats:make-float-converters encode-bfloat16 decode-bfloat16 8 7 t)

;; TODO: ANSI-Portable NAN/INF/-INF ?
(defmacro forall ((bind dtype &key (duration 1.001) (fuzzing t)) &body body)
  "Iterates body over {NaN, Inf, -Inf, Min(dtype) ~ Max(dtype)}"
  (with-gensyms (body-f)
    `(flet ((,body-f (,bind &aux (,bind (dtype/cast ,bind ,dtype))) ,@body))
       (multiple-value-bind (max min)
	   (values (dtype/max ,dtype) (dtype/min ,dtype))
	 (loop with count = 0
	       with stop = nil
	       for nth upfrom 0
	       for fuzz = (if ,fuzzing (random (dtype/cast 2 ,dtype)) 0)
	       while (and (not stop) (<= count max)) do
		 (,body-f (+ fuzz count))
		 (if (>= count 3.223714e37)
		     (setf stop t)
		     (incf count (expt ,duration nth))))
	 (when (not (= min 0))
	   ;; Assuming symmetric
	   (loop with count = 0
		 with stop = nil
		 for nth upfrom 0
		 for fuzz = (if ,fuzzing (random (dtype/cast 2 ,dtype)) 0)
		 while (and (not stop) (<= count max)) do
		   (,body-f (dtype/cast (- (+ fuzz count)) ,dtype))
		   (if (>= count 3.223714e37)
		       (setf stop t)
		       (incf count (expt ,duration nth))))))
       (,body-f (dtype/max ,dtype))
       (,body-f (dtype/min ,dtype))
       ;; TODO ...
       ;;  (,body-f (dtype/inf ,dtype))
       ;;  (,body-f (dtype/-inf ,dtype))
       ;;  (,body-f (dtype/nan ,dtype))
       (,body-f 0)
       (when (dtype/floatp ,dtype) (,body-f (dtype/smallest ,dtype))))))
