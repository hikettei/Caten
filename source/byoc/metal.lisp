(defpackage :caten/byoc/metal
  (:use
   :cl :caten/air :caten/codegen/expr :caten/codegen/renderer :caten/codegen/shape-inference
   :caten/runtime/buffer :caten/codegen/helpers
   :caten/runtime/buffer :caten/runtime/runtime :caten/common.dtype :caten/codegen/backend :cffi :flexi-streams :float-features)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler))

(in-package :caten/byoc/metal)
;; [Note] Currently not tested :(
(defun ensure-foreign-library ()
  (load-foreign-library "/usr/lib/libobjc.dylib")
  (load-foreign-library "/System/Library/Frameworks/Metal.framework/Metal")
  (load-foreign-library "/System/Library/Frameworks/CoreGraphics.framework/CoreGraphics")
  (load-foreign-library "/usr/lib/libSystem.dylib"))

(defcfun "MTLCreateSystemDefaultDevice" :pointer)
(defcfun "sel_registerName" :pointer (name :pointer))
(defcfun "dispatch_data_create" :pointer (data :pointer) (offset :size) (x :pointer) (y :pointer))
(defcfun "objc_getClass" :pointer (name :string))

(defun sel (name) (with-foreign-string (*name name) (sel-registername *name)))
(defmacro msg (ptr selector restype &rest args)
  `(foreign-funcall "objc_msgSend" :pointer ,ptr :pointer (sel ,selector) ,@args ,restype))
(defun to-ns-str (str) (with-foreign-string (*str str) (msg (objc-getclass "NSString") "stringWithUTF8String:" :pointer :pointer *str)))

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
      (assert (string= "MTLB" (flexi-streams:octets-to-string (subseq lib 0 4))) () "Invalid Metal library. Corrupt XCode?")
      lib)))
;; ~~ Extension ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass MetalBuffer (AbstractBuffer) nil)
(defclass MetalRuntime (GraphRuntime) ((device :accessor metal-runtime-device)))

(defmethod initialize-instance :after ((runtime MetalRuntime) &key)
  (ensure-foreign-library)
  (assert (runtime-renderer runtime))
  (with-float-traps-masked t
    (setf (metal-runtime-device runtime) (metal-renderer-device (runtime-renderer runtime)))))

(defmethod open-buffer ((runtime MetalRuntime) (buffer MetalBuffer))
  (let ((initial-value (if (eql (buffer-dtype buffer) :bool)
                           nil
                           (coerce 0 (dtype->lisp (buffer-dtype buffer)))))
        (size (* (apply #'* (buffer-shape buffer)) (dtype/size-of (buffer-dtype buffer)))))
    (if (= 0 (buffer-nrank buffer))
        (setf (buffer-value buffer) initial-value)
        (setf (buffer-value buffer) (msg (metal-runtime-device runtime) "newBufferWithLength:options:" :pointer :ulong size :int 0)))))

(defmethod close-buffer ((runtime MetalRuntime) (buffer MetalBuffer))
  (when (pointerp (buffer-value buffer))
    (msg (buffer-value buffer) "release" :void)
    (setf (buffer-value buffer) nil)))

(defmethod transfer-from-array ((runtime MetalRuntime) (buffer MetalBuffer) array)
  ;; [TODO] contents are setfable?
  ;; [TODO] Optimize (there's an api to do the same thing)
  (let ((val (msg (buffer-value buffer) "contents" :pointer)))
    (dotimes (i (apply #'* (buffer-shape buffer)))
      (setf (mem-aref val (caten/codegen/helpers:->cffi-dtype (buffer-dtype buffer)) i) (aref array i)))))

(defmethod transfer-into-array ((runtime MetalRuntime) (buffer MetalBuffer))

  (let ((val (msg (buffer-value buffer) "contents" :pointer))
        (placeholder (make-array (apply #'* (buffer-shape buffer)) :element-type (dtype->lisp (buffer-dtype buffer)))))
    (dotimes (i (apply #'* (buffer-shape buffer)) placeholder)
      (setf (aref placeholder i) (mem-aref val (caten/codegen/helpers:->cffi-dtype (buffer-dtype buffer)) i)))))

(defmethod copy-buffer-value ((runtime MetalRuntime) (buffer MetalBuffer))
  (buffer-value buffer))

(defmethod bref ((buffer MetalBuffer) idx)
  (let ((val (msg (buffer-value buffer) "contents" :pointer)))
    (mem-aref val (caten/codegen/helpers:->cffi-dtype (buffer-dtype buffer)) idx)))

(defclass Metal-Renderer (CStyle-Renderer) ((device :accessor metal-renderer-device)))

(define-auto-scheduler (Metal-Auto-Scheduler ()) :n-global-loop 3)

(define-backend :metal MetalBuffer MetalRuntime Metal-Renderer Metal-Auto-Scheduler t)
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
      (:FOR
       (if (eql (getattr bp :scope) :global)
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
       (if (find (getattr bp :idx) *global-idx-list*)
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
   (mtl-queue :initarg :mtl-queue :accessor mp-mtl-queue)
   (name :initarg :name :accessor mp-name)
   (library :accessor mp-library)
   (fxn :accessor mp-fxn)
   (global-size :initform `(1 1 1) :accessor mp-global-size)
   (local-size :initform `(1 1 1) :accessor mp-local-size)
   (pipeline-state :accessor mp-pipeline-state)))

(defmethod initialize-instance :after ((mp Metal-Program) &key)
  (assert (string= "MTLB" (flexi-streams:octets-to-string (subseq (mp-lib mp) 0 4))) () "Invalid Metal library. Corrupt XCode?")
  (with-pointer-to-vector-data (*lib (mp-lib mp))
    (let ((data (dispatch-data-create *lib (length (mp-lib mp)) (null-pointer) (null-pointer)))
          (error-ptr (null-pointer)))
      (setf (mp-library mp) (msg (mp-device mp) "newLibraryWithData:error:" :pointer :pointer data :pointer error-ptr))
      (assert (null-pointer-p error-ptr) () "Failed to create a Metal library: ~a" (msg error-ptr "localizedDescription" :pointer))
      (setf (mp-fxn mp) (msg (mp-library mp) "newFunctionWithName:" :pointer :pointer (to-ns-str (string-downcase (princ-to-string (mp-name mp))))))
      (let ((descriptor (msg (objc-getclass "MTLComputePipelineDescriptor") "new" :pointer)))
        (msg descriptor "setComputeFunction:" :void :pointer (mp-fxn mp))
        (msg descriptor "setSupportIndirectCommandBuffers:" :void :bool 1)
        (let ((error-ptr (null-pointer)))
          (setf (mp-pipeline-state mp) (msg (mp-device mp) "newComputePipelineStateWithDescriptor:options:reflection:error:" :pointer :pointer descriptor :int 1 :pointer (null-pointer) :pointer error-ptr))
          (assert (null-pointer-p error-ptr) () "Failed to create a Metal pipeline state: ~a" (msg error-ptr "localizedDescription" :pointer)))))))

(defcstruct MTLSize (width :int) (height :int) (depth :int))
(defun load-size (mtl-size width height depth)
  (setf (foreign-slot-value mtl-size '(:struct mtlsize) 'width) width
        (foreign-slot-value mtl-size '(:struct mtlsize) 'height) height
        (foreign-slot-value mtl-size '(:struct mtlsize) 'depth) depth))

(defmethod invoke ((mp Metal-Program) &rest buffers)
  (let ((total-max-threads (msg (mp-pipeline-state mp) "maxTotalThreadsPerThreadgroup" :int)))
    (when (> (apply #'* (mp-local-size mp)) total-max-threads)
      (error "Error: TODO"))
    (let* ((command-buffer (msg (mp-mtl-queue mp) "commandBuffer" :pointer))
           (encoder (msg command-buffer "computeCommandEncoder" :pointer)))
      (msg encoder "setComputePipelineState:" :void :pointer (mp-pipeline-state mp))
      (loop for buf in buffers
            for nth upfrom 0 do
              (msg encoder "setBuffer:offset:atIndex:" :void :pointer (buffer-value buf) :int 0 :int nth))
      (loop for buf in buffers
            for nth upfrom 0 do
              (msg encoder "setBytes:length:atIndex:" :void :pointer (buffer-value buf) :int 4 :int nth))
      (with-foreign-objects ((gs '(:struct MTLSize)) (ls '(:struct MTLSize)))
        (apply #'load-size gs (mp-global-size mp))
        (apply #'load-size ls (mp-local-size mp))
        (msg encoder "dispatchThreadgroups:threadsPerThreadgroup:" :void :pointer gs :pointer ls))
      (msg encoder "endEncoding" :void)
      (msg command-buffer "setLabel:" :void :pointer (to-ns-str (string-downcase (princ-to-string (mp-name mp)))))
      (msg command-buffer "commit" :void)
      (msg command-buffer "waitUntilCompleted" :void)
      (let ((err (msg command-buffer "error" :pointer)))
        (assert (null-pointer-p err) () "Failed to execute a Metal command buffer: ~a" (msg err "localizedDescription" :pointer))))))

(defun make-metal-caller (mp) `(lambda (&rest args) (apply #'invoke ,mp args)))

(defmethod %compile-kernel ((renderer Metal-Renderer) items dir)
  (ensure-foreign-library)
  (float-features:with-float-traps-masked t
    (let* ((code (apply #'concatenate 'string
                        (append (list (header))
                                (loop for item in items
                                      if (getattr item :rendered-object) collect (getattr item :rendered-object))))))
      (when (>= (ctx:getenv :JIT_DEBUG) 3)
        (format t "[Final Code]:~%~a~%" code))
      (let* ((lib (mtl-compile-source code))
             (device (MTLCreateSystemDefaultDevice))
             (mtl-queue (msg device "newCommandQueueWithMaxCommandBufferCount:" :pointer :int 1024)))
        (setf (metal-renderer-device renderer) device)
        (loop for item in items
              if (getattr item :rendered-object)
                do (let ((caller (make-instance 'Metal-Program :lib lib :name (getattr item :name) :device device :mtl-queue mtl-queue)))
                     (setf (getattr item :compiled-object) (make-metal-caller caller))))))))
