(defpackage :caten/codegen/backends/clang
  (:use :cl :caten/air :cffi :caten/codegen :caten/codegen/renderer :caten/codegen/helpers
        :caten/codegen/shape-inference :caten/avm :caten/codegen/expr))

(in-package :caten/codegen/backends/clang)

(defun ->cdtype (dtype)
  (ecase dtype
    (:bool "boolean")
    (:float64 "double")
    (:float32 "float")
    (:uint64 "uint64_t")
    (:int64 "int64_t")
    (:int32 "int32_t")
    (:uint32 "uint32_t")
    (:int16 "int16_t")
    (:uint16 "uint16_t")
    (:uint8 "uint8_t")
    (:int8 "int8_t")))

(defvar *indent*)
(defmethod %render-kernel ((renderer CStyle-Renderer) si)
  (setf (getattr si :name) (gensym "KERNEL"))
  (with-output-to-string (out)
    (let ((args
            (apply
             #'concatenate
             'string
             (butlast
              (loop for arg in (append (node-writes si) (node-reads si))
                    for type in (append (getattr si :write-types) (getattr si :read-types))
                    unless (= (buffer-nrank type) -1)
                    append (list (format nil "~a ~(~a~)" (->cdtype (buffer-dtype type)) arg) ", "))))))
      (format out "void ~(~a~)(~a);~%" (getattr si :name) args)
      (format out "void ~(~a~)(~a) {~%" (getattr si :name) args)
      (let ((*indent* 2))
        (dolist (bp (getattr si :blueprint))
          (render-bp out bp)))
      (format out "}~%"))))

(defun render-bp (stream bp)
  (flet ((indent () (make-string *indent* :initial-element #\space)))
    (ecase (node-type bp)
      (:FOR
       (format stream "~afor (int ~(~a~)=~(~a~); ~(~a~); ~(~a~)+=~(~a~)) {~%"
               (indent)
               (getattr bp :idx)
               (render-expr 'CStyle-Renderer (getattr bp :upfrom))
               (render-expr 'CStyle-Renderer (getattr bp :below))
               (getattr bp :idx)
               (render-expr 'CStyle-Renderer (getattr bp :by)))
       (incf *indent* 2))
      (:ENDFOR
       (format stream "~a}~%" (indent))
       (decf *indent* 2))
      (:IF
       (error "")
       )
      (:ENDIF
       (error ""))
      (:EXPR
       (let ((pre-iterations (getattr bp :Iterations)))
         (labels ((print-aref (name b is &key iterations)
                    (if (and is (not (= -1 (buffer-nrank b))) (> (length (iteration-space-shape is)) 0) (> (length iterations) 0))
                        (format nil "~a[~(~a~)]" name
                                (render-expr
                                 'CStyle-Renderer
                                 (apply
                                  #'expr-add
                                  (map
                                   'list
                                   #'(lambda (view stride i)
                                       (if view
                                           (expr-mul (expr-const (third view) :int64) (expr-mul stride (expr-add (expr-const (car view) :int64) i)))
                                           (expr-mul stride i)))
                                   (iteration-space-views is)
                                   (iteration-space-strides is)
                                   iterations))))
                        (format nil "~(~a~)" name))))
           (format stream "~a~a = ~a;~%"
                   (indent)
                   (render-list
                    (map 'list #'(lambda (x y z) (print-aref x y z :iterations pre-iterations))
                         (node-writes bp) (relay-writes (read-type-relay bp)) (relay-write-iters (read-type-relay bp))))
                   (render-expr 'CStyle-Renderer (getattr bp :EXPR) :index-space pre-iterations))))))))


(defun header ()
  (format nil "~%#include <math.h>
#include <stdint.h>
~a
#define boolean _Bool
#define _infinity INFINITY
#define _negative_infinity -INFINITY
#define _nan NAN
#define min(a, b) ((a) < (b) ? (a) : (b))~%#define max(a, b) ((a) > (b) ? (a) : (b))
"
	  (if (= 1 (ctx:getenv :OMP))
	      "#include <omp.h>"
	      "")))

(defmethod %compile-kernel ((renderer CStyle-Renderer) items)
  (let ((code
          (apply #'concatenate 'string
                 (append
                  (list (header))
                  (loop for item in items
                        if (getattr item :rendered-object)
                          collect (getattr item :rendered-object))))))
    (print code)
    ))
