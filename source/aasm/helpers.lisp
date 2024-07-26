(in-package :caten/aasm)

(defmacro range (from below &optional (by 1))
  `(loop for i from ,from below ,below by ,by collect i))

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
