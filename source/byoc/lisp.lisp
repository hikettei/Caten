(defpackage :caten/byoc/lisp
  (:use :cl :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime
   :caten/codegen/backend :caten/codegen/renderer :caten/air
   :caten/codegen/expr :caten/codegen/helpers :caten/codegen/shape-inference)
  (:export :LispBuffer))

(in-package :caten/byoc/lisp)

(defclass LispBuffer (AbstractBuffer) nil)

(defmethod open-buffer (runtime (buffer LispBuffer))
  (let ((initial-value (if (eql (buffer-dtype buffer) :bool)
                           nil
                           (coerce 0 (dtype->lisp (buffer-dtype buffer))))))
    (if (= 0 (buffer-nrank buffer))
        (setf (buffer-value buffer) initial-value)
        (setf (buffer-value buffer) (make-array (apply #'* (buffer-shape buffer)) :element-type (dtype->lisp (buffer-dtype buffer)) :initial-element initial-value)))))

(defmethod close-buffer (runtime (buffer LispBuffer)) (setf (buffer-value buffer) nil))
(defmethod transfer-from-array (runtime (buffer LispBuffer) array) (setf (buffer-value buffer) array))
(defmethod transfer-into-array ((buffer LispBuffer)) (buffer-value buffer))
(defmethod copy-buffer-value (runtime (buffer LispBuffer)) (copy-seq (buffer-value buffer)))
(defmethod bref ((buffer LispBuffer) idx) (aref (buffer-value buffer) idx))
(define-backend :lisp LispBuffer GraphRuntime nil nil nil)
