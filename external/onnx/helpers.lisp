(in-package :caten/onnx)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun symb (&rest inputs)
    (intern (with-output-to-string (out) (dolist (sym inputs) (princ sym out))))))

(defmacro range (from to &optional (by 1))
  `(loop for i from ,from to ,to by ,by collect i))
