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

;; Inherit default expression rendering so all ops like :ADD/:MUL/:SIN work out of the box.
(define-renderer CStyle-Renderer (Default-Renderer) nil)

(define-kernel (ClangKernel CStyle-Renderer) () nil
  :specs
  ((:PROGN ((:PROGN (~ _))
            ->
            ((node graph)
             (format nil "{~%~{~a~^~%~}~%}"
                     (map 'list #'(lambda (id) (render-kernel id)) (node-reads node))))))
   (:IF ((:IF (cond body))
         ->
         ((node graph)
          (format nil "if (~a) {~%~a~%}"
                  (render (car (node-reads (id->value graph cond))))
                  (render-kernel body)))))
   (:FOR ((:FOR (range body))
          ->
          ((node graph)
           (let* ((range (id->value graph range))
                  (bind (getattr range :idx))
                  (size (first (node-reads range)))
                  (step (second (node-reads range)))
                  (size-str (if (symbolp size) (render (car (node-reads (id->value graph size)))) (format nil "~a" size)))
                  (step-str (if (symbolp step) (render (car (node-reads (id->value graph step)))) (format nil "~a" step))))
             (format nil "for (int ~(~a~)=0; ~(~a~)<~a; ~(~a~)+=~a) {~%~a~%}"
                     bind bind size-str bind step-str (render-kernel body))))))
   (:EXPR ((:EXPR (x))
           ->
           ((node graph)
            (format nil "~a;" (render x))))))
  :launch nil
  :compile nil)

;; (define-optimizer (ClangOptimizer) :allow-profile t)
(define-backend :CLANG :runtime ClangRuntime :buffer ClangBuffer :kernel ClangKernel :renderer CStyle-Renderer)
