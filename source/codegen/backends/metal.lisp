(defpackage :caten/codegen/backends/metal
  (:use :cl :cffi :caten/air :cffi :caten/codegen/renderer :caten/codegen/helpers
        :caten/codegen/shape-inference :caten/avm :caten/codegen/expr)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler)
  (:export #:use-metal #:with-metal))

(in-package :caten/codegen/backends/metal)

(defun use-metal () (setf (ctx:getenv :JIT) 1 (ctx:getenv :JIT_BACKEND) "METAL"))
(defmacro with-metal (() &body body)
  `(ctx:with-contextvar (:JIT 1 :JIT_BACKEND "METAL") ,@body))

(defclass Metal-Renderer (CStyle-Renderer) nil)
(defmethod get-default-renderer ((id (eql :metal))) (make-instance 'Metal-Renderer))
(define-auto-scheduler
    (Metal-Auto-Scheduler ())
    :n-global-loop 3)
(define-hook-auto-scheduler (Metal-Renderer Metal-Auto-Scheduler))
;; Ref: https://github.com/tinygrad/tinygrad/blob/master/tinygrad/runtime/ops_metal.py
(defparameter *request-type-compile* 13)

(defun ensure-metal-compiler ()
  (load-foreign-library "/usr/lib/libobjc.dylib")
  (load-foreign-library "/System/Library/Frameworks/Metal.framework/Metal")
  (load-foreign-library "/System/Library/PrivateFrameworks/MTLCompiler.framework/MTLCompiler")
  (load-foreign-library "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics")
  (load-foreign-library "/usr/lib/libSystem.dylib"))

(defcfun "MTLCodeGenServiceCreate" :pointer (service-name :string))
(defcfun "MTLCodeGenServiceBuildRequest" :void (cgs :pointer) (unused :pointer) (request-type :int) (request :pointer) (request-len :size) (callback :pointer))

(defclass MetalCompiler () ((device :accessor mc-cgs)))
(defmethod initialize-instance :after ((compiler MetalCompiler) &key &allow-other-keys)
  (setf (mc-cgs compiler) (MTLCodeGenServiceCreate "caten")))

(defparameter *ret* nil)
(defcallback callback :void
    ((blockptr :pointer) (error :int32) (data :pointer) (datalen :size) (errormsg :pointer))
  nil)

(defmethod mc-compile-request ((compiler MetalCompiler) request)
  ;; [TODO] How to tell callbacks?
  (with-foreign-string (*request request)
    (print *request)
    (print "A")
    (print request)
    (print (mc-cgs compiler))
    (print (length request))
    (MTLCodeGenServiceBuildRequest
     (mc-cgs compiler) (null-pointer) *request-type-compile* *request (length request)
     (mem-ref (callback callback) :pointer -16))
    (print "B")))

(defun round-up (n multiple)
  (multiple-value-bind (quotient remainder) (truncate n multiple)
    (if (zerop remainder) n (* (1+ quotient) multiple))))

(defun make-request-form (src params)
  (let* ((src-encoded (babel:string-to-octets src :encoding :utf-8))
         (src-padded-len (round-up (1+ (length src-encoded)) 4))
         (src-padding-len (- src-padded-len (length src-encoded)))
         (src-padded (concatenate
                      '(vector (unsigned-byte 8))
                      src-encoded (make-array src-padding-len :element-type '(unsigned-byte 8) :initial-element 0)))
         (params-encoded (babel:string-to-octets params :encoding :utf-8))
         (params-padded (concatenate '(vector (unsigned-byte 8)) params-encoded (make-array 1 :element-type '(unsigned-byte 8) :initial-element 0)))
         (header (cl-pack:pack "<QQ" (length src-padded) (length params-padded)))
         (request (concatenate 'string header (babel:octets-to-string src-padded) (babel:octets-to-string params-padded))))
    request))
;; [TODO] -fmodules-cache-dir
(defun compile-metal-code (mc source &aux (params "-fno-fast-math -std=metal3.1 --driver-mode=metal -x metal"))
  (let ((request (make-request-form source params)))
    (mc-compile-request mc request)))
;; ~~~ Renderers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun dtype->mtype (dtype)
  (ecase dtype
    (:bool 'boolean)
    (:float64 (error "float64 math is not supported on Metal."))
    (:float32 'float)
    (:bfloat16 'bfloat)
    (:uint64 'uint64)
    (:int64 'int64)
    (:int32 'int32)
    (:uint32 'uint32)
    (:int16 'int16)
    (:uint16 'uint16)
    (:uint8 'uint8)
    (:int8 'int8)))

(defvar *indent*)
(defvar *depth*)
(defvar *global-idx-list*)

(defmethod %render-kernel ((renderer Metal-Renderer) si)
  (let ((args (loop for item in (getattr si :blueprint)
                    if (eql (node-type item) :DEFINE-GLOBAL)
                      collect item)))
    (with-output-to-string (out)
      (format out "kernel void ~(~a~)(" (getattr si :name))
      (dolist (item args)
        (format out "device~a ~(~a~) ~a~(~a~), " (if (eql (getattr item :type) :output) "" " const") (dtype->mtype (getattr item :dtype))
                (if (getattr item :pointer-p) "*" "") (car (node-writes item))))
      (format out "uint3 gid [[threadgroup_position_in_grid]], uint3 lid [[thread_position_in_threadgroup]]) {~%")
      (let ((*indent* 2) (*depth* 0) (*global-idx-list*))
        (dolist (node (getattr si :blueprint))
          (render-bp node out)))
      (format out "}"))))

(defun render-bp (bp stream)
  (flet ((indent () (make-string *indent* :initial-element #\space)))
    (ecase (node-type bp)
      ;; [TODO] Optimize Global Loops once we've finished implementing polyhedral compiler
      (:FOR
       (if nil;(eql (getattr bp :scope) :global)
           (progn
             (format stream "~auint ~(~a~) = lid.~a;~%" (indent) (getattr bp :idx) (case *depth* (0 "x") (1 "y") (2 "z") (otherwise (error "Exceecive loop depth"))))
             (push (getattr bp :idx) *global-idx-list*)
             (incf *depth*))
           (progn
             (format stream "~afor(int ~(~a~)=~a;~a;~a+=~a) {~%" (indent)
                     (getattr bp :idx)
                     (render-expr 'CStyle-Renderer (getattr bp :upfrom))
                     (render-expr 'CStyle-Renderer (getattr bp :below))
                     (getattr bp :idx)
                     (render-expr 'CStyle-Renderer (getattr bp :by)))
             (incf *indent* 2))))
      (:ENDFOR
       (if nil;(find (getattr bp :idx) *global-idx-list*)
           nil
           (progn (decf *indent* 2) (format stream "~a}~%" (indent)))))
      (:IF
       (format stream "~aif(~a){~%" (indent) (render-expr 'CStyle-Renderer (getattr bp :condition)))
       (incf *indent* 2))
      (:ENDIF
       (decf *indent* 2)
       (format stream "~a}~%" (indent)))
      (:EXPR
       (let ((pre-iterations (getattr bp :iterations)))
         (labels ((print-aref (name b is &key iterations)
                    (if (and is (not (= -1 (buffer-nrank b))) (> (length (iteration-space-shape is)) 0) (> (length iterations) 0))
                        (format nil "~(~a~)[~(~a~)]" name
                                (render-expr
                                 'CStyle-Renderer
                                 (apply
                                  #'expr-add
                                  (map
                                   'list
                                   #'(lambda (view stride i)
                                       (if view
                                           (expr-mul stride (expr-add (expr-const (car view) :int64) (expr-mul (expr-const (third view) :int64) i)))
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
  (format nil "#include <metal_stdlib>
using namespace metal;
"))

(defmethod %compile-kernel ((renderer Metal-Renderer) items dir)
  (ensure-metal-compiler)
  (let* ((session (make-instance 'MetalCompiler))
         (code (apply #'concatenate 'string
                      (append (list (header))
                              (loop for item in items
                                    if (getattr item :rendered-object) collect (getattr item :rendered-object))))))
    (when (>= (ctx:getenv :JIT_DEBUG) 3)
      (format t "[Final Code]:~%~a~%" code))
    (compile-metal-code session code)
    (error "STOP")
    ))
