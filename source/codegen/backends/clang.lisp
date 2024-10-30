(defpackage :caten/codegen/backends/clang
  (:use :cl :caten/air :cffi :caten/codegen/renderer :caten/codegen/helpers
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
  (let ((args (loop for item in (getattr si :blueprint)
                    if (eql (node-type item) :DEFINE-GLOBAL)
                      collect item)))
    (with-output-to-string (out)
      (let ((args
              (apply
               #'concatenate
               'string
               (butlast
                (loop for arg in args
                      append (list
                              (format nil "~a~a~a~a ~(~a~)"
                                      (ecase (getattr arg :type)
                                        (:input "const ")
                                        (:output "")
                                        (:shape "const "))
                                      (->cdtype (getattr arg :dtype))
                                      (if (getattr arg :pointer-p) "*" "")
                                      (if (eql :input (getattr arg :type))
                                          " restrict"
                                          "")
                                      (car (node-writes arg)))
                              ", "))))))
        (format out "void ~(~a~)(~a);~%" (getattr si :name) args)
        (format out "void ~(~a~)(~a) {~%" (getattr si :name) args)
        (let ((*indent* 2))
          (dolist (bp (getattr si :blueprint))
            (render-bp out bp)))
        (format out "}~%")))))

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
       (decf *indent* 2)
       (format stream "~a}~%" (indent)))
      (:IF
       (error "not ready"))
      (:ENDIF
       (error "not ready"))
      (:EXPR
       (let ((pre-iterations (getattr bp :iterations)))
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
           (format stream "~a~a~a = ~a;~%"
                   (indent)
                   (if (car (getattr bp :declare-type))
                       (format nil "~a " (->cdtype (buffer-dtype (car (relay-writes (read-type-relay bp))))))
                       "")
                   (render-list
                    (map 'list #'(lambda (x y z) (print-aref x y z :iterations pre-iterations))
                         (node-writes bp) (relay-writes (read-type-relay bp)) (relay-write-iters (read-type-relay bp))))
                   (render-expr 'CStyle-Renderer (getattr bp :EXPR) :index-space pre-iterations)))))
      (:DEFINE-GLOBAL))))


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

(defun load-foreign-function (source &key (compiler "gcc") (lang "c") (compiler-flags) (dir nil))
  (declare (type string source compiler))
  (uiop:with-temporary-file (:pathname sharedlib :type "so" :keep t :directory dir)
    nil
    :close-stream
    (let* ((cmd
	     ;; gcc -shared -o sharedlib
	     (append
	      (list
	       compiler "-shared"
	       "-x" lang)
	      compiler-flags
	      (list "-o" (uiop:native-namestring sharedlib) "-")))
	   (process-info (uiop:launch-program
			  cmd
			  :input :stream
			  :error-output :stream))
	   (input (uiop:process-info-input process-info))
	   (error-output (uiop:process-info-error-output process-info)))
      (unwind-protect (princ source input)
	(close input))
      (unless (zerop (uiop:wait-process process-info))
	(error "Caten[Clang]: Failed to compile a shared library:~%~a~%

Compiled with this command: ~a"
	       (alexandria:read-stream-content-into-string error-output)
	       (with-output-to-string (out)
		 (dolist (c cmd) (princ c out) (princ " " out))))))
    (cffi:load-foreign-library sharedlib)))

(defmethod %compile-kernel ((renderer CStyle-Renderer) items)
  (let ((code
          (apply #'concatenate 'string
                 (append
                  (list (header))
                  (loop for item in items
                        if (getattr item :rendered-object)
                          collect (getattr item :rendered-object))))))
    (when (>= (ctx:getenv :JIT_DEBUG) 3)
      (format t "[Final Code]:~%~a~%" code))
    (load-foreign-function code :compiler (ctx:getenv :CC) :lang "c" :compiler-flags '("-O3"))
    nil))

