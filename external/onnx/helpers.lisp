(in-package :caten/onnx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out))))))

(defmacro range (from to &optional (by 1))
  `(loop for i from ,from to ,to by ,by collect i))

(defun int->dtype (int)
  (let ((name (int->data-type int)))
    (ecase name
      (:float :float32)
      (:double :float64)
      (otherwise name))))
      
