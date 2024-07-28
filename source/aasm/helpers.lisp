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

(defun dtype/cast (x cast-to)
  (declare (type dtype-t cast-to))
  (if (floatp x)
      (if (or (eql cast-to :float64) (eql cast-to :float32) (eql cast-to :float16))
	  (coerce x (dtype->lisp cast-to))
	  (coerce (round x) (dtype->lisp cast-to)))
      (coerce x (dtype->lisp cast-to))))

(defun %column-major-calc-strides (shape)
  (declare (type list shape))
  (flet ((const (n) (if (node-p n) n (%iconst n))))
    (let* ((num-dims (length shape))
           (strides (make-list num-dims :initial-element (%iconst 1))))
      (loop for i from 1 to (- num-dims 1) do
	(setf (nth i strides) (%mul (const (nth (- i 1) strides)) (const (nth (- i 1) shape)))))
      strides)))

(defun %row-major-calc-strides (shape)
  (declare (type list shape))
  (flet ((const (n) (if (node-p n) n (%iconst n))))
    (let* ((num-dims (length shape))
           (strides (make-list num-dims :initial-element (%iconst 1))))
      (loop for i downfrom (- num-dims 2) to 0 do
	(setf (nth i strides) (%mul (const (nth (+ i 1) strides)) (const (nth (+ i 1) shape)))))
      strides)))
