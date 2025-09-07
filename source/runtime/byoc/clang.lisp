(defpackage :caten/byoc/clang
  (:use
   :cl
   :caten/utilities/dtype
   :caten/graph
   :caten/runtime/buffer     
   :caten/runtime/bring-your-own-backend
   :caten/graph)
  (:export
   
   ))
(in-package :caten/byoc/clang)

(define-runtime ClangRuntime () nil
  :open ((runtime) runtime)
  :close ((runtime) runtime))

(define-buffer
    (ClangBuffer ClangRuntime) ()
    nil
    :open-buffer ((runtime buffer)
                    (let ((initial-value (if (eql (buffer-dtype buffer) :bool)
                           nil
                           (coerce 0 (dtype->lisp (buffer-dtype buffer))))))
    (if (= 0 (buffer-nrank buffer))
        (setf (buffer-value buffer) initial-value)
        (setf (buffer-value buffer) (make-array (apply #'* (buffer-shape buffer)) :element-type (dtype->lisp (buffer-dtype buffer)) :initial-element initial-value)))))
    :close-buffer ((runtime buffer) (setf (buffer-value buffer) nil))
    :transfer-from-array ((runtime buffer array) (setf (buffer-value buffer) array))
    :transfer-into-array ((buffer) (buffer-value buffer))
    :bref ((buffer idx) (aref (buffer-value buffer) idx)))

(define-renderer CStyle-Renderer () nil
  (:ADD ((:ADD (x y)) -> (format nil "~a+~a" (render x) (render y))))
  )
;  ((:PROGN (~ x)) nil))

(define-kernel (ClangKernel CStyle-Renderer) () nil
               :launch nil
               :compile nil)

;; (define-optimizer (ClangOptimizer) :allow-profile t)
(define-backend :CLANG :runtime ClangRuntime :buffer ClangBuffer :kernel ClangKernel :renderer CStyle-Renderer)
