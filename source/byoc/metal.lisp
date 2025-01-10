(defpackage :caten/byoc/metal
  (:use
   :cl :caten/air :caten/codegen/expr :caten/codegen/renderer :caten/codegen/shape-inference
   :caten/runtime/buffer :caten/codegen/helpers :caten/codegen/blueprint
   :caten/runtime/buffer :caten/runtime/runtime :caten/common.dtype :caten/codegen/backend :cffi :flexi-streams :float-features)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler))

(in-package :caten/byoc/metal)

(defconstant +request-type-compile+ 13)

(defun ensure-foreign-library ()
  (load-foreign-library "/usr/lib/libobjc.dylib")
  (load-foreign-library "/System/Library/Frameworks/Metal.framework/Metal")
  (load-foreign-library "/System/Library/PrivateFrameworks/MTLCompiler.framework/MTLCompiler")
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
;; ~~ MTLCompiler ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defcfun "MTLCodeGenServiceCreate" :pointer (service-name :string))
(defcfun "MTLCodeGenServiceBuildRequest" :void (cgs :pointer) (unused :pointer) (request-type :int) (request :pointer) (request-len :size) (callback :pointer))
(defcfun "closure_cffi_callback" :pointer (callback :pointer))

(defvar *callback-handler*)

(defcallback callback :void
    ((blockptr :pointer) (error :int32) (data :pointer) (datalen :size) (errormsg :pointer))
  (declare (ignore blockptr))
  (assert (eql :ready *callback-handler*) () "*call-back-handler* is not set to :ready.")
  (case error
    (0
     ;; offset from beginning to data = header size + warning size
     (let* ((octets (loop for i upfrom 0 below datalen collect (mem-aref data :char i)))
            (offsets (cl-pack:unpack "<LL" (with-output-to-string (out) (map 'list #'(lambda (x) (princ (code-char x) out)) (subseq octets 8 16))))))
       (setf *callback-handler* (cons :succeed (subseq octets offsets)))))
    (otherwise
     (setf *callback-handler* (cons :failed (foreign-string-to-lisp errormsg)))))
  nil)

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

(defun mtl-compile-source (source
                           &key
                             (fmodules-cache-path
                              #+darwin(progn "~/Library/Caches")
                              #-darwin(progn "~/.cache"))
                           &aux
                             (service (MTLCodeGenServiceCreate "caten"))
                             (params (format nil "-fno-fast-math -std=metal3.1 --driver-mode=metal -x metal -fmodules-cache-path=~a -fno-caret-diagnostics" fmodules-cache-path))
                             (*callback-handler* :ready))
  (declare (type foreign-pointer service) (type string source params))
  (assert (<= (ctx:getenv :PARALLEL) 1) () "METAL does not support parallel compilation.")
  (let ((request (make-request-form source params)))
    (with-foreign-string (*request request)
      (MTLCodeGenServiceBuildRequest
       service (null-pointer) +request-type-compile+
       *request (length request) (closure-cffi-callback (get-callback 'callback))))
    (assert (consp *callback-handler*) () "*callback-handler* did not receive anything!")
    (case (car *callback-handler*)
      (:succeed
       (let* ((len (length (cdr *callback-handler*)))
              (octets (make-array len :element-type '(signed-byte 8) :initial-contents (cdr *callback-handler*))))
         (assert (string= "MTLB" (flexi-streams:octets-to-string (subseq octets 0 4))) () "Invalid Metal library. Corrupt XCode?")
         (assert (string= "ENDT" (flexi-streams:octets-to-string (subseq octets (- len 4)))) () "Invalid Metal library. Corrupt XCode?")
         octets))
      (:failed
       (error "Failed to compile a metallib:~%~a~%Compiled with this command: ~a" (cdr *callback-handler*) params)))))
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
;; [TODO] Optimize Transfer
(defmethod transfer-from-array ((runtime MetalRuntime) (buffer MetalBuffer) array)
  ;; CPU -> METAL
  (let ((val (msg (buffer-value buffer) "contents" :pointer)))
    (dotimes (i (apply #'* (buffer-shape buffer)))
      (setf (mem-aref val (caten/codegen/helpers:->cffi-dtype (buffer-dtype buffer)) i) (aref array i)))))

(defmethod transfer-into-array ((runtime MetalRuntime) (buffer MetalBuffer))
  ;; METAL -> CPU
  (let ((val (msg (buffer-value buffer) "contents" :pointer))
        (placeholder (make-array (apply #'* (buffer-shape buffer)) :element-type (dtype->lisp (buffer-dtype buffer)))))
    (dotimes (i (apply #'* (buffer-shape buffer)) placeholder)
      (setf (aref placeholder i) (mem-aref val (caten/codegen/helpers:->cffi-dtype (buffer-dtype buffer)) i)))))

(defmethod copy-buffer-value ((runtime MetalRuntime) (buffer MetalBuffer))
  (let ((buffer (copy-buffer buffer)))
    (transfer-from-array runtime buffer (transfer-into-array runtime buffer))
    (buffer-value buffer)))

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
    (:uint64 'uint64_t)
    (:int64 'int64_t)
    (:int32 'int32_t)
    (:uint32 'uint32_t)
    (:int16 'int16_t)
    (:uint16 'uint16_t)
    (:uint8 'uint8_t)
    (:int8 'int8_t)))

(defvar *indent*)
(defvar *depth*)
(defvar *global-idx-list*)

(defmethod %render-node ((renderer Metal-Renderer) (id (eql :SPACE)) node)
  (let ((lv (ecase (getattr node :level) (:block "gid") (:thread "lid")))
        (dim (ecase (getattr node :rank) (0 "x") (1 "y") (2 "z"))))
    (format nil "~a.~a" lv dim)))

(defmethod %render-kernel ((renderer Metal-Renderer) si)
  (let ((args (schedule-item-args si)))
    (with-output-to-string (out)
      (format out "kernel void ~(~a~)(" (getattr si :name))
      (dolist (item args)
        (format out "device~a ~(~a~) ~a~(~a~), " (if (eql (getattr item :type) :output) "" " const") (dtype->mtype (getattr item :dtype))
                (if (getattr item :pointer-p) "*" "&") (car (node-writes item))))
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
                   (render-expr 'Metal-Renderer (getattr bp :EXPR) :index-space pre-iterations)))))
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
   (grid-size :initarg :grid-size :accessor mp-grid-size)
   (argtypes :initarg :argtypes :accessor mp-argtypes)
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
        (assert (not (null-pointer-p (mp-fxn mp))) () "setComputeFunction: function must not be a null pointer! looks compilation was failed?")
        (msg descriptor "setComputeFunction:" :void :pointer (mp-fxn mp))
        (msg descriptor "setSupportIndirectCommandBuffers:" :void :bool t)
        (let ((error-ptr (null-pointer)))
          (setf (mp-pipeline-state mp) (msg (mp-device mp) "newComputePipelineStateWithDescriptor:options:reflection:error:" :pointer :pointer descriptor :int 1 :pointer (null-pointer) :pointer error-ptr))
          (assert (null-pointer-p error-ptr) () "Failed to create a Metal pipeline state: ~a" (msg error-ptr "localizedDescription" :pointer)))))))

(defcstruct MTLSize (width :ulong) (height :ulong) (depth :ulong))
(defun load-size (mtl-size width height depth)
  (setf (foreign-slot-value mtl-size '(:struct mtlsize) 'width) width
        (foreign-slot-value mtl-size '(:struct mtlsize) 'height) height
        (foreign-slot-value mtl-size '(:struct mtlsize) 'depth) depth))

(defmethod invoke ((mp Metal-Program) &rest buffers)
  (assert (= (length buffers) (length (mp-argtypes mp))) () "Metal: The number of arguments does not match the number of arguments in the Metal program.")
  (let ((total-max-threads (msg (mp-pipeline-state mp) "maxTotalThreadsPerThreadgroup" :int)))
    (when (> (apply #'* (map 'list #'exprgrid-local-size-int (mp-grid-size mp))) total-max-threads)
      (error "Error: TODO"))
    (let* ((command-buffer (msg (mp-mtl-queue mp) "commandBuffer" :pointer))
           (encoder (msg command-buffer "computeCommandEncoder" :pointer)))
      (msg encoder "setComputePipelineState:" :void :pointer (mp-pipeline-state mp))
      (loop for buf in buffers
            for argtyp in (mp-argtypes mp)
            for nth upfrom 0 do
              (if (typep buf 'MetalBuffer)
                  (msg encoder "setBuffer:offset:atIndex:" :void :pointer (buffer-value buf) :int 0 :int nth)
                  (if (and (buffer-p buf) (arrayp (buffer-value buf)))
                      (with-pointer-to-vector-data (*p (buffer-value buf))
                        ;; [TODO] METAL cannot update *rng_counter* ...
                        (msg encoder "setBytes:length:atIndex:" :void :pointer *p :int 4 :int nth))
                      (let ((buf (if (and (buffer-p buf) (numberp (buffer-value buf))) (buffer-value buf) buf)))
                        (assert (numberp buf) () "Metal: Please transfer the buffe ~a into metal." buf)
                        (with-foreign-object (*p (->cffi-dtype argtyp))
                          (setf (mem-ref *p (->cffi-dtype argtyp)) buf)
                          (msg encoder "setBytes:length:atIndex:" :void :pointer *p :int 4 :int nth))))))
      (assert (= (length (mp-grid-size mp)) 3) () "Metal only supports for 3d parallelism!")
      (with-foreign-objects ((gs '(:struct MTLSize)) (ls '(:struct MTLSize)))
        ;; [TODO] Integer
        (apply #'load-size gs (map 'list #'(lambda (x) (exprgrid-global-size-int x nil)) (mp-grid-size mp)))
        (apply #'load-size ls (map 'list #'exprgrid-local-size-int (mp-grid-size mp)))
        (msg encoder "dispatchThreadgroups:threadsPerThreadgroup:" :void :pointer gs :pointer ls))
      (msg encoder "endEncoding" :void)
      (msg command-buffer "setLabel:" :void :pointer (to-ns-str (string-downcase (princ-to-string (mp-name mp)))))
      (msg command-buffer "commit" :void)
      (msg command-buffer "waitUntilCompleted" :void)
      (let ((err (msg command-buffer "error" :pointer)))
        (assert (null-pointer-p err) () "Failed to execute a Metal command buffer: ~a" (msg err "localizedDescription" :pointer))))))

(defun make-metal-caller (mp) `(lambda (&rest args) (apply #'invoke ,mp args)))

(defmethod %compile-kernel ((renderer Metal-Renderer) items dir)
  (ensure-foreign-library) ;; TODO: O(0.05) time elapsed ...
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
                do (let* ((argtypes (map 'list #'(lambda (x) (getattr x :dtype)) (schedule-item-args item)))
                          (caller (make-instance
                                   'Metal-Program :lib lib :name (getattr item :name) :device device :mtl-queue mtl-queue :argtypes argtypes
                                   :grid-size (blueprint-gather-grids (getattr item :blueprint) :max-dimension 3))))
                     (setf (getattr item :compiled-object) (make-metal-caller caller))))))))
