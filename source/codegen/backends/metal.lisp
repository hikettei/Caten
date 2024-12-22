(defpackage :caten/codegen/backends/metal
  (:use :cl :cffi :caten/air :caten/codegen/renderer :caten/codegen/helpers
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
;; ~~ Compiler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ensure-foreign-library ()
  (load-foreign-library "/usr/lib/libobjc.dylib")
  (load-foreign-library "/System/Library/Frameworks/Metal.framework/Metal")
  (load-foreign-library "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics")
  (load-foreign-library "/usr/lib/libSystem.dylib"))

(defcfun "MTLCreateSystemDefaultDevice" :pointer)
(defcfun "sel_registerName" :pointer (name :pointer))
(defcfun "dispatch_data_create" :pointer (data :pointer) (offset :size) (x :pointer) (y :pointer))
(defun sel (name) (with-foreign-string (*name name) (sel-registername *name)))
(defmacro msg (ptr selector restype &rest args)
  `(foreign-funcall "objc_msgSend" :pointer ,ptr :pointer (sel ,selector)
                    ,@(loop for arg in args append `(:pointer ,arg))
                    ,restype))

(defun mtl-compile-source (source)
  (flet ((run-cmd (cmd input)
           (let* ((process-info (uiop:launch-program cmd :input :stream :output :stream :error-output :stream))
                  (error-output (uiop:process-info-error-output process-info))
                  (input-stream (uiop:process-info-input process-info)))
             (unwind-protect
                  (if (stringp input)
                      (princ input input-stream)
                      (loop for i across input do (write-byte i input-stream)))
               (close input-stream))
             (unless (zerop (uiop:wait-process process-info))
	       (error "Caten[Clang]: Failed to compile a shared library:~%~a~%

Compiled with this command: ~a"
	              (alexandria:read-stream-content-into-string error-output)
	              (with-output-to-string (out)
		        (dolist (c cmd) (princ c out) (princ " " out)))))
             (alexandria:read-stream-content-into-byte-vector (uiop:process-info-output process-info)))))
    (let* ((air (run-cmd '("xcrun" "-sdk" "macosx" "metal" "-x" "metal" "-c" "-" "-o" "-") source))
           (lib (run-cmd '("xcrun" "-sdk" "macosx" "metallib" "-" "-o" "-") air)))
      lib)))

(defun compile-metal-code (source)
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
      (format out "}~%~%"))))

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

(defclass Metal-Program ()
  ((lib :initarg :lib :accessor mp-lib)
   (device :initarg :device :accessor mp-device)
   (name :initarg :name :accessor mp-name)
   (pipeline-state :accessor mp-pipeline-state)))


(defmethod initialize-instance :after ((mp Metal-Program) &key)
  (let ((data (dispatch-data-create (mp-lib mp) (length (mp-lib mp)) (null-pointer) (null-pointer))))
    (print data)))

(defmethod invoke ((mp Metal-Program) &rest buffers)
  
  )

(defmethod %compile-kernel ((renderer Metal-Renderer) items dir)
  (ensure-foreign-library)
  (let* ((code (apply #'concatenate 'string
                      (append (list (header))
                              (loop for item in items
                                    if (getattr item :rendered-object) collect (getattr item :rendered-object))))))
    (when (>= (ctx:getenv :JIT_DEBUG) 3)
      (format t "[Final Code]:~%~a~%" code))
    (let* ((lib (mtl-compile-source code))
           (device (MTLCreateSystemDefaultDevice))
           (callers
             (loop for item in items
                   collect (make-instance 'Metal-Program :lib lib :name (getattr item :name) :device device))))
      ;; [TODO] Use cl-metal
      (error "STOP"))))