(cl:in-package :cl-user)
(defpackage :caten/common.dtype
  (:documentation "If you insist your function works well, they needs to be tested via this package.")
  (:use :cl)
  (:export
   #:dtype-t
   #:dtype->lisp
   #:dtype/cast
   
   #:forall
   #:1.0ulp
   #:3.5ulp
   ))
(in-package :caten/common.dtype)

(deftype dtype-t ()
  "A list of available keywords as a dtype"
  `(and keyword
	(member
	 :float64 :float32 :float16
	 :uint64 :uint32 :uint16 :uint8
	 :int64 :int32 :int16 :int8 :bool)))

(defun dtype->lisp (dtype)
  "to which dtypes values are coerced?"
  (case dtype
    (:float64 'double-float)
    (:float32 'single-float)
    (:float16 'single-float)
    (:uint64  '(unsigned-byte 64))
    (:int64   '(unsigned-byte 64))
    (:uint32  '(unsigned-byte 32))
    (:int32   '(signed-byte 32))
    (:uint16  '(unsigned-byte 16))
    (:int16   '(signed-byte 16))
    (:uint8  '(unsigned-byte 8))
    (:int8   '(signed-byte 8))
    (:bool    'boolean)
    (otherwise (error "dtype->lisp: ~a is not supported" dtype))))

(defun dtype/cast (x cast-to)
  (declare (type dtype-t cast-to))
  (if (or (typep x 'ratio) (floatp x))
      (if (or (eql cast-to :float64) (eql cast-to :float32) (eql cast-to :float16))
	  (coerce x (dtype->lisp cast-to))
	  (coerce (round x) (dtype->lisp cast-to)))
      (coerce x (dtype->lisp cast-to))))

(defun 1.0ulp (dtype)
  
  )

(defun 3.5ulp (dtype) (* 3.5 (1.0ulp dtype)))

(defun dtype/max ())
(defun dtype/min ())
(defmacro forall ((bind dtype &key (duration 1.0)) &body body)
  "Iterates body over {NaN, Inf, -Inf, Min(dtype) ~ Max(dtype)}"
  `(progn

     ))

;; hypothesis.forall (float64/float32/float16)
