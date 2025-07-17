(defpackage :caten/byoc/webgpu
  (:use :cl :cffi
        :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime
        :caten/codegen/byoc :caten/codegen/renderer :caten/air
        :caten/aasm :caten/aasm/expr :caten/codegen/helpers :caten/codegen/iteration)
  (:export #:WebGPUBuffer #:WebGPURuntime))
(in-package :caten/byoc/webgpu)
;; Requirement: https://github.com/gfx-rs/wgpu-native?tab=readme-ov-file
;; ARCH = aarch64 if you have an apple silicon, otherwise x86_64
;; curl -L  https://github.com/gfx-rs/wgpu-native/releases/download/v25.0.2.1/wgpu-macos-{ARCH}-release.zip -o wgpu-macos-release.zip
;; unzip -j wgpu-macos-release.zip 'lib/libwgpu_native.dylib'
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (load-foreign-library "libwgpu_native.dylib")
    (t (c) (declare (ignore c)))))
;; -- Basic Handle Types ------------------------------------------------------
(defctype WGPUInstance        :pointer)
(defctype WGPUAdapter         :pointer)
(defctype WGPUDevice          :pointer)
(defctype WGPUQueue           :pointer)
(defctype WGPUBuffer          :pointer)
(defctype WGPUShaderModule    :pointer)
(defctype WGPUPipelineLayout  :pointer)
(defctype WGPUComputePipeline :pointer)
(defctype WGPUBindGroupLayout :pointer)
(defctype WGPUBindGroup       :pointer)
(defctype WGPUCommandEncoder  :pointer)
(defctype WGPUCommandBuffer   :pointer)
(defctype WGPUComputePass     :pointer)

(defconstant +wgpu-success+ 0)
;; Error Check Utils
(defun %check-null (ptr msg)
  (when (null-pointer-p ptr)
    (error "WebGPU: ~a returned NULL" msg))
  ptr)

(defun %check-true (ok msg)
  (unless ok (error "WebGPU: ~a failed" msg)))
;; -- C enum / flag -----------------------------------------------------------
(defconstant +buffer-usage-map-read+   #x0001)
(defconstant +buffer-usage-map-write+  #x0002)
(defconstant +buffer-usage-copy-src+   #x0004)
(defconstant +buffer-usage-copy-dst+   #x0008)
(defconstant +buffer-usage-index+      #x0010)
(defconstant +buffer-usage-vertex+     #x0020)
(defconstant +buffer-usage-uniform+    #x0040)
(defconstant +buffer-usage-storage+    #x0080)
(defconstant +buffer-usage-indirect+   #x0100)

(defconstant +shader-stage-compute+ #x4)

;; -- Dtypes -------------------------------------------------------------------
(defcstruct WGPUBufferDescriptor
  (nextInChain      :pointer)
  (label            :pointer)
  (usage            :uint32)
  (size             :uint64)
  (mappedAtCreation :bool))

(defcstruct WGPURequestAdapterOptions
  (nextInChain :pointer)
  (compatibleSurface :pointer)
  (powerPreference   :int)      ; 0:Undefined 1:LowPower 2:HighPerformance
  (backendType       :int)      ; 0:Undefined 1:Vulkan 2:Metal 3:D3D12 4:Browser
  (forceFallbackAdapter :bool))

(defcstruct WGPUDeviceDescriptor
  (nextInChain       :pointer)
  (label             :pointer)
  (requiredFeaturesCount :uint32)
  (requiredFeatures  :pointer)
  (requiredLimits    :pointer)
  (defaultQueue      :pointer))  ; <- The head of structure chain (according to wgpu c header)

(defcstruct WGPUBindGroupLayoutEntry
  (nextInChain  :pointer)
  (binding      :uint32)
  (visibility   :uint32)
  (buffer       :pointer)  ; pointer to WGPUBufferBindingLayout
  (sampler      :pointer)
  (texture      :pointer)
  (storageTexture :pointer))

(defcstruct WGPUBufferBindingLayout
  (nextInChain :pointer)
  (type        :int)       ; 0:Undefined 1:Uniform 2:Storage 3:ReadOnlyStorage
  (hasDynamicOffset :bool)
  (minBindingSize  :uint64))

(defcstruct WGPUBindGroupEntry
  (nextInChain :pointer)
  (binding     :uint32)
  (buffer      WGPUBuffer)
  (offset      :uint64)
  (size        :uint64))

(defcstruct WGPUBindGroupDescriptor
  (nextInChain :pointer)
  (layout      WGPUBindGroupLayout)
  (entryCount  :uint32)
  (entries     :pointer))

(defcstruct WGPUShaderModuleWGSLDescriptor
  (chain-s  :pointer)
  (code     :string))

(defcstruct WGPUShaderModuleDescriptor
  (nextInChain :pointer)
  (label       :pointer))

;; -- C API --------------------------------------------------------------
(defcfun ("wgpuCreateInstance"      wgpuCreateInstance)       WGPUInstance (desc :pointer))
(defcfun ("wgpuInstanceRequestAdapter" wgpuInstanceRequestAdapter) WGPUAdapter
  (instance WGPUInstance) (options :pointer))
(defcfun ("wgpuAdapterRequestDevice"   wgpuAdapterRequestDevice)  WGPUDevice
  (adapter WGPUAdapter) (desc :pointer))
(defcfun ("wgpuDeviceGetQueue"         wgpuDeviceGetQueue)     WGPUQueue (device WGPUDevice))
(defcfun ("wgpuDeviceCreateBuffer"     wgpuDeviceCreateBuffer) WGPUBuffer
  (device WGPUDevice) (desc :pointer))
(defcfun ("wgpuDeviceCreateShaderModule" wgpuDeviceCreateShaderModule) WGPUShaderModule
  (device WGPUDevice) (desc :pointer))
(defcfun ("wgpuDeviceCreateComputePipeline"
          wgpuDeviceCreateComputePipeline) WGPUComputePipeline
  (device WGPUDevice) (desc :pointer))
(defcfun ("wgpuDeviceCreateBindGroupLayout"
          wgpuDeviceCreateBindGroupLayout) WGPUBindGroupLayout
  (device WGPUDevice) (desc :pointer))
(defcfun ("wgpuDeviceCreateBindGroup"
          wgpuDeviceCreateBindGroup) WGPUBindGroup
  (device WGPUDevice) (desc :pointer))
(defcfun ("wgpuDeviceCreateCommandEncoder"
          wgpuDeviceCreateCommandEncoder) WGPUCommandEncoder
  (device WGPUDevice) (desc :pointer))
(defcfun ("wgpuCommandEncoderBeginComputePass"
          wgpuCommandEncoderBeginComputePass) WGPUComputePass
  (encoder WGPUCommandEncoder) (desc :pointer))
(defcfun ("wgpuComputePassEncoderSetPipeline"
          wgpuComputePassEncoderSetPipeline) :void
  (pass WGPUComputePass) (pipeline WGPUComputePipeline))
(defcfun ("wgpuComputePassEncoderSetBindGroup"
          wgpuComputePassEncoderSetBindGroup) :void
  (pass WGPUComputePass) (index :uint32) (group WGPUBindGroup) (dynamicOffsetCount :uint32) (dynamicOffsets :pointer))
(defcfun ("wgpuComputePassEncoderDispatchWorkgroups"
          wgpuComputePassEncoderDispatchWorkgroups) :void
  (pass WGPUComputePass) (x :uint32) (y :uint32) (z :uint32))
(defcfun ("wgpuComputePassEncoderEnd"
          wgpuComputePassEncoderEnd) :void
  (pass WGPUComputePass))
(defcfun ("wgpuCommandEncoderFinish"
          wgpuCommandEncoderFinish) WGPUCommandBuffer
  (encoder WGPUCommandEncoder) (desc :pointer))
(defcfun ("wgpuQueueSubmit"
          wgpuQueueSubmit) :void
  (queue WGPUQueue) (count :uint32) (commands :pointer))
(defcfun ("wgpuDevicePoll"          wgpuDevicePoll)   :bool
  (device WGPUDevice) (wait :bool) (timeout :pointer))
(defcfun ("wgpuBufferDestroy"       wgpuBufferDestroy) :void (buffer WGPUBuffer))
(defcfun ("wgpuBufferGetMappedRange" wgpuBufferGetMappedRange) :pointer
  (buffer WGPUBuffer) (offset :uint64) (size :uint64))
(defcfun ("wgpuBufferMapAsync"      wgpuBufferMapAsync) :void
  (buffer WGPUBuffer) (mode :uint32) (offset :uint64) (size :uint64) (callback :pointer) (userdata :pointer))
(defcfun ("wgpuBufferUnmap"         wgpuBufferUnmap)    :void (buffer WGPUBuffer))
(defcfun ("wgpuQueueWriteBuffer"    wgpuQueueWriteBuffer) :void
  (queue WGPUQueue) (buffer WGPUBuffer) (offset :uint64) (data :pointer) (size :uint64))

(defun dtype->wgsl (dtype)
  (ecase dtype
    (:float32 "f32")
    (:int32   "i32")
    (:uint32  "u32")
    (:bool    "bool")
    ((:float64 :int64 :uint64 :int16 :uint16 :int8 :uint8 :bfloat16)
     (error "WGSL: dtype ~a is not supported." dtype))))

(defun dtype->cffi (dtype)
  (ecase dtype
    (:float32 :float)
    (:int32   :int32)
    (:uint32  :uint32)
    (:bool    :uint8)))
;; ───────────────────────────────────────────────────────────────────────────
;;  WebGPUBuffer
;; ───────────────────────────────────────────────────────────────────────────
(defclass WebGPUBuffer (AbstractBuffer) nil)

(defmethod open-buffer ((rt WebGPURuntime) (buf WebGPUBuffer))
  (if (= 0 (buffer-nrank buf))
      (setf (buffer-value buf)
            (coerce 0 (dtype->lisp (buffer-dtype buf))))
      (let* ((bytes (* (buffer-storage-size buf)
                       (dtype/size-of (buffer-dtype buf))))
             (usage (logior +buffer-usage-storage+
                            +buffer-usage-copy-src+
                            +buffer-usage-copy-dst+
                            +buffer-usage-map-read+
                            +buffer-usage-map-write+)))
        (with-foreign-object (desc '(:struct WGPUBufferDescriptor))
          (setf (foreign-slot-value desc '(:struct WGPUBufferDescriptor) 'usage) usage
                (foreign-slot-value desc '(:struct WGPUBufferDescriptor) 'size)  bytes
                (foreign-slot-value desc '(:struct WGPUBufferDescriptor) 'mappedAtCreation) nil
                (foreign-slot-value desc '(:struct WGPUBufferDescriptor) 'label) (null-pointer)
                (foreign-slot-value desc '(:struct WGPUBufferDescriptor) 'nextInChain) (null-pointer))
          (setf (buffer-value buf)
                (%check-null (wgpuDeviceCreateBuffer (wgpu-device rt) desc)
                             "wgpuDeviceCreateBuffer"))))))

(defmethod close-buffer ((rt WebGPURuntime) (buf WebGPUBuffer))
  (when (pointerp (buffer-value buf))
    (wgpuBufferDestroy (buffer-value buf))
    (setf (buffer-value buf) nil)))

(defun %queue-upload (rt gpu-buf host-vec)
  (with-pointer-to-vector-data (*data host-vec)
    (wgpuQueueWriteBuffer (wgpu-queue rt) (buffer-value gpu-buf) 0 *data
                          (* (length host-vec) (dtype/size-of (buffer-dtype gpu-buf))))))

(defmethod transfer-from-array ((rt WebGPURuntime) (buf WebGPUBuffer) array)
  (assert (arrayp array))
  (if (= 0 (buffer-nrank buf))
      (setf (buffer-value buf) (aref array 0))
      (%queue-upload rt buf array)))

;; GPU→CPU
(defun %sync-map-read (rt gpu-buf bytes)
  (let ((done nil))
    (flet ((cb (_status _ud) (declare (ignore _status _ud))
             (setf done t)))
      (with-foreign-callback (callback (:void (:uint32 :pointer)) #'cb)
        (wgpuBufferMapAsync (buffer-value gpu-buf) +buffer-usage-map-read+ 0 bytes callback (null-pointer))
        (loop until done do (wgpuDevicePoll (wgpu-device rt) t (null-pointer)))))
  (let ((ptr (wgpuBufferGetMappedRange (buffer-value gpu-buf) 0 bytes)))
    (prog1 ptr
      (wgpuBufferUnmap (buffer-value gpu-buf))))))

(defmethod transfer-into-array ((buf WebGPUBuffer))
  (cond
    ((numberp (buffer-value buf)) (buffer-value buf))
    (t
     (let* ((bytes (* (buffer-storage-size buf)
                      (dtype/size-of (buffer-dtype buf))))
            (rt (buffer-runtime buf))
            (ptr (%sync-map-read rt buf bytes))
            (vec (make-array (buffer-storage-size buf)
                             :element-type (dtype->lisp (buffer-dtype buf)))))
       (with-pointer-to-vector-data (*dst vec)
         (loop for i below bytes
               do (setf (mem-aref *dst :uint8 i) (mem-aref ptr :uint8 i))))
       vec))))

(defmethod copy-buffer-value ((rt WebGPURuntime) (buf WebGPUBuffer))
  (let ((clone (copy-buffer buf)))
    (transfer-from-array rt clone (transfer-into-array buf))
    (buffer-value clone)))

(defmethod bref ((buf WebGPUBuffer) idx)
  (let ((arr (transfer-into-array buf)))
    (aref arr idx)))
;; ───────────────────────────────────────────────────────────────────────────
;;  ④ WebGPURuntime
;; ───────────────────────────────────────────────────────────────────────────
(defclass WebGPURuntime (GraphRuntime)
  ((instance :accessor wgpu-instance)
   (adapter  :accessor wgpu-adapter)
   (device   :accessor wgpu-device)
   (queue    :accessor wgpu-queue)))

(defmethod initialize-instance :after ((rt WebGPURuntime) &key)
  (setf (wgpu-instance rt) (%check-null (wgpuCreateInstance (null-pointer)) "wgpuCreateInstance"))
  (with-foreign-object (opt '(:struct WGPURequestAdapterOptions))
    (setf (foreign-slot-value opt '(:struct WGPURequestAdapterOptions) 'backendType) 0  ; ANY
          (foreign-slot-value opt '(:struct WGPURequestAdapterOptions) 'powerPreference) 2 ; HIGH_PERF
          (foreign-slot-value opt '(:struct WGPURequestAdapterOptions) 'forceFallbackAdapter) nil
          (foreign-slot-value opt '(:struct WGPURequestAdapterOptions) 'compatibleSurface) (null-pointer)
          (foreign-slot-value opt '(:struct WGPURequestAdapterOptions) 'nextInChain) (null-pointer))
    (setf (wgpu-adapter rt) (%check-null
                             (wgpuInstanceRequestAdapter (wgpu-instance rt) opt)
                             "wgpuInstanceRequestAdapter")))
  (setf (wgpu-device rt) (%check-null
                          (wgpuAdapterRequestDevice (wgpu-adapter rt) (null-pointer))
                          "wgpuAdapterRequestDevice"))
  (setf (wgpu-queue rt) (wgpuDeviceGetQueue (wgpu-device rt))))
;; ───────────────────────────────────────────────────────────────────────────
;;  Renderer / Kernel / AutoScheduler
;; ───────────────────────────────────────────────────────────────────────────
(defclass WGSL-Renderer (CStyle-Renderer) nil)

(defmethod %render-const ((r WGSL-Renderer) obj)
  (cond
    ;; bool
    ((typep obj 'boolean) (if obj "true" "false"))
    ((and (floatp obj)
          (or (float-infinity-p obj) (float-nan-p obj)))
     (cond
       ((float-nan-p obj) "nan")
       ((> obj 0)         "infinity")
       (t                 "-infinity")))
    ;; float
    ((typep obj 'double-float) (format nil "~,15e" obj))
    ((typep obj 'single-float) (format nil "~,8e" obj))
    (t (format nil "~(~a~)" obj))))
;; ---------------------------------------------------------------------------
(defmethod %render-node ((r WGSL-Renderer) (id (eql :LOAD)) node)
  (%render-const r (getattr node :value)))
(defmethod %render-node ((r WGSL-Renderer) (id (eql :SPACE)) node)
  (let* ((lv  (ecase (getattr node :level)
                (:block  "workgroup_id")
                (:thread "local_invocation_id")))
         (dim (ecase (getattr node :rank) (0 ".x") (1 ".y") (2 ".z"))))
    (format nil "~a~a" lv dim)))
(macrolet ((def (tag op)
             `(defmethod %render-node ((r WGSL-Renderer) (id (eql ,tag)) node)
                (let ((lhs (render-node r (nth 0 (node-reads node))))
                      (rhs (render-node r (nth 1 (node-reads node)))))
                  (simplify-arithmetic-code (format nil "(~a~a~a)" lhs ,op rhs))))))
  (def :ADD   "+")
  (def :MUL   "*")
  (def :MOD   "%")
  (def :IDIV  "/")
  (def :AND   " & ")
  (def :OR    " | ")
  (def :XOR   " ^ "))
(macrolet ((def (tag fn)
             `(defmethod %render-node ((r WGSL-Renderer) (id (eql ,tag)) node)
                (format nil "~a(~a, ~a)"
                        ,fn
                        (render-node r (nth 0 (node-reads node)))
                        (render-node r (nth 1 (node-reads node)))))))
  (def :MAX "max"))
(macrolet ((def (tag op)
             `(defmethod %render-node ((r WGSL-Renderer) (id (eql ,tag)) node)
                (format nil "~a(~a)" ,op (render-node r (nth 0 (node-reads node)))))))
  (def :NEG "-")
  (def :NOT "!")
  (def :SIN "sin")
  (def :log2 "log2")
  (def :exp2 "exp2")
  (def :SQRT "sqrt"))
(defmethod %render-node ((r WGSL-Renderer) (id (eql :RECIP)) node)
  (format nil "(1.0/(~a))" (render-node r (nth 0 (node-reads node)))))
(macrolet ((def (tag op)
             `(defmethod %render-node ((r WGSL-Renderer) (id (eql ,tag)) node)
                (format nil "(~a~a~a)"
                        (render-node r (nth 1 (node-reads node)))
                        ,op
                        (render-node r (nth 2 (node-reads node)))))))
  (def :!= "!=")
  (def :<  "<"))

(defmethod %render-node ((r WGSL-Renderer) (id (eql :AREF)) node)
  (format nil "~a[~a]"
          (render-node r (car (node-reads node)))
          (render-node r (second (node-reads node)))))

(defmethod %render-node ((r WGSL-Renderer) (id (eql :MOVE)) node)
  (render-node r (second (node-reads node))))

(defmethod %render-node ((r WGSL-Renderer) (id (eql :SETF)) node)
  (format nil "~a = ~a"
          (render-node r (car (node-reads node)))
          (render-node r (second (node-reads node)))))

(defmethod %render-node ((r WGSL-Renderer) (id (eql :STORE)) node)
  (render-node r (second (node-reads node))))

(defmethod %render-node ((r WGSL-Renderer) (id (eql :CAST)) node)
  (format nil "~a(~a)"
          (dtype->wgsl (getattr node :dtype))
          (render-node r (second (node-reads node)))))

(defmethod %render-node ((r WGSL-Renderer) (id (eql :WHERE)) node)
  (format nil "select(~a, ~a, ~a)"
          (render-node r (third  (node-reads node)))   ; falseVal
          (render-node r (second (node-reads node)))   ; trueVal
          (render-node r (car    (node-reads node)))))  ; condition

(defmethod %render-node ((r WGSL-Renderer) id node)
  (call-next-method))
  
(defclass WebGPUKernel (AbstractKernel)
  ((program :accessor webgpu-program)     ; WGSL string
   (pipeline :accessor webgpu-pipeline)
   (bind-layout :accessor webgpu-bind-layout)
   (caller  :accessor webgpu-caller)))

(define-auto-scheduler WebGPU-Auto-Scheduler
  :n-profile 1 :per-band-optrules 2
  :ptile-max-rank 3 :shared-max 32768)

(define-backend :WEBGPU WebGPUBuffer WebGPURuntime WGSL-Renderer WebGPUKernel WebGPU-Auto-Scheduler t)
;; ── Blueprint → WGSL ───────────────────────────────────────────────────────
(defun wgsl-header () "
alias boolean = bool;
@group(0) @binding(255) var<workgroup> _dummy: array<u32,1>; // keeps group size valid
")

(defun dtype-decl (arg idx)
  (let ((wgsl-ty (dtype->wgsl (getattr arg :dtype))))
    (format nil "@group(0) @binding(~a) var<storage, ~a> ~(~a~): array<~a>;"
            idx
            (if (eq (getattr arg :mode) :read) "read" "read_write")
            (car (node-writes arg)) wgsl-ty)))

(defun render-bp-wgsl (graph out &aux (indent 0) (seen))
  (labels ((indent-str () (make-string indent :initial-element #\Space))
           (fmt (s &rest args) (apply #'format out (format nil "~a~a~%" (indent-str) s) args))
           (r (s &aux (v (id->value graph s)))
             (when (and v (null (find (node-id v) seen)))
               (f v) (push (node-id v) seen)))
           (e (id) (render-node (make-instance 'WGSL-Renderer :graph graph) id))
           (f (node)
             (case (node-type node)
               (:PROGN (fmt "{") (incf indent 2) (mapc #'r (node-reads node))
                       (decf indent 2) (fmt "}"))
               (:EXPR
                (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                    (fmt "~a;" (e (car (node-reads node))))
                    (let ((ty (->cdtype (tensor-relay-dtype
                                         (car (relay-writes (read-type-relay node)))))))
                      (fmt "var ~(~a~): ~a = ~a;" (car (node-writes node)) ty
                           (e (car (node-reads node)))))))
               (:FOR
                (multiple-value-bind (range body) (apply #'values (node-reads node))
                  (setf range (id->value graph range))
                  (multiple-value-bind (bind size step)
                      (values (getattr range :idx) (first (node-reads range)) (second (node-reads range)))
                    (when (symbolp size) (setf size (e (car (node-reads (id->value graph size))))))
                    (when (symbolp step) (setf step (e (car (node-reads (id->value graph step))))))
                    (fmt "for (var ~(~a~)=0; ~(~a~)<~a; ~(~a~)+=~a) {" bind bind size bind step)
                    (incf indent 2) (r body) (decf indent) (fmt "}"))))
               (:IF
                (multiple-value-bind (cond body) (apply #'values (node-reads node))
                  (fmt "if (~a) {" (e (car (node-reads (id->value graph cond)))))
                  (incf indent 2) (r body) (decf indent) (fmt "}")))
               (:BARRIER (fmt "workgroupBarrier();"))
               (:DEFINE-SHARED-MEMORY
                (fmt "var<workgroup> ~(~a~): array<~a,~a>;"
                     (car (node-writes node))
                     (dtype->wgsl (getattr node :dtype))
                     (getattr node :size)))
               (:DEFINE-GLOBAL) (:RANGE) (:ALLOCATE)
               (otherwise (error "WGSL render: unsupported node ~a" (node-type node))))))
    (f (id->value graph (car (graph-outputs graph))))))

(defmethod %render-kernel ((renderer WGSL-Renderer) kernel)
  (let* ((args     (kernel-args kernel))
         (bs       (map 'list #'second (caten/codegen/blueprint:blueprint-gather-grids
                                        (kernel-blueprint kernel)))))
    (setf (webgpu-program kernel)
          (with-output-to-string (out)
            (format out "~a~%" (wgsl-header))
            (loop for arg in args
                  for idx upfrom 0
                  do (format out "~a~%" (dtype-decl arg idx)))
            (format out "@compute @workgroup_size(~{~a, ~}1) fn ~(~a~)() {" bs (kernel-name kernel))
            (render-bp-wgsl (kernel-blueprint kernel) out)
            (format out "}~%")))))

;; ───────────────────────────────────────────────────────────────────────────
;;  %compile‑kernel : WGSL → Shader → ComputePipeline
;; ───────────────────────────────────────────────────────────────────────────
(defun %wgpu-make-shader (device wgsl-src)
  ;; chain → WGSL desc → ShaderModule desc
  (with-foreign-object (wgsl '(:struct WGPUShaderModuleWGSLDescriptor))
    (setf (foreign-slot-value wgsl '(:struct WGPUShaderModuleWGSLDescriptor) 'code) wgsl-src
          (foreign-slot-value wgsl '(:struct WGPUShaderModuleWGSLDescriptor) 'chain-s) (null-pointer))
    (with-foreign-object (sdesc '(:struct WGPUShaderModuleDescriptor))
      (setf (foreign-slot-value sdesc '(:struct WGPUShaderModuleDescriptor) 'nextInChain) wgsl
            (foreign-slot-value sdesc '(:struct WGPUShaderModuleDescriptor) 'label) (null-pointer))
      (%check-null (wgpuDeviceCreateShaderModule device sdesc)
                   "wgpuDeviceCreateShaderModule"))))

(defun %wgpu-make-bind-layout (device nargs modes)
  (with-foreign-object (entry '(:struct WGPUBindGroupLayoutEntry) nargs)
    (dotimes (i nargs)
      (let* ((e (mem-aptr entry '(:struct WGPUBindGroupLayoutEntry) i)))
        (with-foreign-object (buf-layout '(:struct WGPUBufferBindingLayout))
          (setf (foreign-slot-value buf-layout '(:struct WGPUBufferBindingLayout) 'type)
                (if (eq (nth i modes) :read) 3 2) ; ReadOnlyStorage / Storage
                (foreign-slot-value buf-layout '(:struct WGPUBufferBindingLayout) 'hasDynamicOffset) nil
                (foreign-slot-value buf-layout '(:struct WGPUBufferBindingLayout) 'minBindingSize) 0)
          (setf (foreign-slot-value e '(:struct WGPUBindGroupLayoutEntry) 'binding) i
                (foreign-slot-value e '(:struct WGPUBindGroupLayoutEntry) 'visibility) +shader-stage-compute+
                (foreign-slot-value e '(:struct WGPUBindGroupLayoutEntry) 'buffer) buf-layout
                (foreign-slot-value e '(:struct WGPUBindGroupLayoutEntry) 'nextInChain) (null-pointer)
                (foreign-slot-value e '(:struct WGPUBindGroupLayoutEntry) 'sampler) (null-pointer)
                (foreign-slot-value e '(:struct WGPUBindGroupLayoutEntry) 'texture) (null-pointer)
                (foreign-slot-value e '(:struct WGPUBindGroupLayoutEntry) 'storageTexture) (null-pointer)))))
    (with-foreign-object (layout-desc '(:struct (:pointer WGPUBindGroupLayoutEntry)))
      (setf (foreign-slot-value layout-desc :pointer) entry)
      (with-foreign-object (bgldesc '(:struct
                                       (nextInChain :pointer)
                                       (entryCount  :uint32)
                                       (entries     :pointer)))
        (setf (foreign-slot-value bgldesc :pointer 'nextInChain) (null-pointer)
              (foreign-slot-value bgldesc :pointer 'entryCount) nargs
              (foreign-slot-value bgldesc :pointer 'entries) entry)
        (%check-null (wgpuDeviceCreateBindGroupLayout device bgldesc)
                     "wgpuDeviceCreateBindGroupLayout")))))

(defun %wgpu-create-pipeline (device shader bind-layout)
  (with-foreign-object (pldesc '(:struct
                                 (nextInChain :pointer)
                                 (layout      WGPUPipelineLayout)
                                 (compute     :pointer)))
    ;; pipeline layout ← bind-group-layout[0]
    (with-foreign-object (pipeline-layout-desc '(:struct
                                                 (nextInChain :pointer)
                                                 (bindGroupLayoutCount :uint32)
                                                 (bindGroupLayouts :pointer)))
      (setf (foreign-slot-value pipeline-layout-desc :pointer 'nextInChain) (null-pointer)
            (foreign-slot-value pipeline-layout-desc :pointer 'bindGroupLayoutCount) 1)
      (with-foreign-object (bgl-array :pointer 1)
        (setf (mem-aref bgl-array :pointer 0) bind-layout)
        (setf (foreign-slot-value pipeline-layout-desc :pointer 'bindGroupLayouts) bgl-array)
        (let ((pl (* (foreign-slot-value pldesc '(:struct) 'layout)
                     (wgpuDeviceCreateBindGroupLayout device pipeline-layout-desc))))
          (setf (foreign-slot-value pldesc '(:struct) 'layout) pl))))
    ;; compute stage
    (with-foreign-object (stage '(:struct
                                  (nextInChain :pointer)
                                  (module      WGPUShaderModule)
                                  (entryPoint  :string)))
      (setf (foreign-slot-value stage '(:struct) 'module) shader
            (foreign-slot-value stage '(:struct) 'entryPoint) "main"
            (foreign-slot-value stage '(:struct) 'nextInChain) (null-pointer))
      (setf (foreign-slot-value pldesc '(:struct) 'compute) stage)
      (%check-null (wgpuDeviceCreateComputePipeline device pldesc)
                   "wgpuDeviceCreateComputePipeline"))))

(defmethod %compile-kernel ((r WGSL-Renderer) items dir)
  (declare (ignore dir))
  (let* ((rt (renderer-runtime r))
         (device (wgpu-device rt)))
    (dolist (k items)
      ;; ① WGSL 文字列生成済み
      (let* ((shader (%wgpu-make-shader device (webgpu-program k)))
             (modes  (map 'list #'(lambda (x) (getattr x :mode)) (kernel-args k)))
             (layout (%wgpu-make-bind-layout device (length modes) modes))
             (pipe   (%wgpu-create-pipeline device shader layout)))
        (setf (webgpu-bind-layout k) layout
              (webgpu-pipeline    k) pipe)
        ;; caller を生成
        (setf (webgpu-caller k)
              (compile nil
                       `(lambda (runtime node &rest args)
                          (let* ((q   (wgpu-queue runtime))
                                 (gbuf-count ,(length modes)))
                            ;; BindGroup 作成
                            (with-foreign-object (entries '(:struct WGPUBindGroupEntry) gbuf-count)
                              (dotimes (i gbuf-count)
                                (let* ((e (mem-aptr entries '(:struct WGPUBindGroupEntry) i))
                                       (arg (nth i args))
                                       (buf (if (typep arg 'WebGPUBuffer)
                                                (buffer-value arg)
                                                (progn
                                                  ;; scalar → 4/8byte uniform buffer
                                                  (with-foreign-object (bd '(:struct WGPUBufferDescriptor))
                                                    (setf (foreign-slot-value bd '(:struct WGPUBufferDescriptor) 'usage)
                                                          +buffer-usage-uniform+
                                                          (foreign-slot-value bd '(:struct WGPUBufferDescriptor) 'size)
                                                          4
                                                          (foreign-slot-value bd '(:struct WGPUBufferDescriptor) 'mappedAtCreation)
                                                          t
                                                          (foreign-slot-value bd '(:struct WGPUBufferDescriptor) 'label)
                                                          (null-pointer)
                                                          (foreign-slot-value bd '(:struct WGPUBufferDescriptor) 'nextInChain)
                                                          (null-pointer))
                                                    (let ((tmpb (wgpuDeviceCreateBuffer (wgpu-device runtime) bd)))
                                                      ;; write value
                                                      (with-foreign-pointer (ptr 4)
                                                        (setf (mem-ref ptr :float) arg)
                                                        (wgpuQueueWriteBuffer q tmpb 0 ptr 4))
                                                      tmpb)))))
                                       (size 4))
                                  (setf (foreign-slot-value e '(:struct WGPUBindGroupEntry) 'binding) i
                                        (foreign-slot-value e '(:struct WGPUBindGroupEntry) 'buffer) buf
                                        (foreign-slot-value e '(:struct WGPUBindGroupEntry) 'offset) 0
                                        (foreign-slot-value e '(:struct WGPUBindGroupEntry) 'size) size
                                        (foreign-slot-value e '(:struct WGPUBindGroupEntry) 'nextInChain) (null-pointer))))
                              (with-foreign-object (bg-desc '(:struct WGPUBindGroupDescriptor))
                                (setf (foreign-slot-value bg-desc '(:struct WGPUBindGroupDescriptor) 'layout) ,layout
                                      (foreign-slot-value bg-desc '(:struct WGPUBindGroupDescriptor) 'entryCount) gbuf-count
                                      (foreign-slot-value bg-desc '(:struct WGPUBindGroupDescriptor) 'entries) entries
                                      (foreign-slot-value bg-desc '(:struct WGPUBindGroupDescriptor) 'nextInChain) (null-pointer))
                                (let* ((bg  (wgpuDeviceCreateBindGroup (wgpu-device runtime) bg-desc))
                                       (ce  (wgpuDeviceCreateCommandEncoder (wgpu-device runtime) (null-pointer)))
                                       (cp  (wgpuCommandEncoderBeginComputePass ce (null-pointer))))
                                  (wgpuComputePassEncoderSetPipeline cp ,pipe)
                                  (wgpuComputePassEncoderSetBindGroup cp 0 bg 0 (null-pointer))
                                  ;; Dispatch: workgroups = ceil(grid / block)
                                  (let* ((dims  (caten/codegen/blueprint:blueprint-gather-grids (kernel-blueprint ,k)))
                                         (gs    (list ,@(loop for i below 3 collect
                                                              `(expr-realize-as-value (nth 0 (nth ,i dims))
                                                                                      (map 'list #'cons
                                                                                           (node-reads node) args))))
                                         (ls    (list ,@(loop for i below 3 collect
                                                              `(expr-realize-as-value (nth 1 (nth ,i dims))
                                                                                      (map 'list #'cons
                                                                                           (node-reads node) args)))))
                                         (wx (max 1 (ceiling (nth 0 gs) (max 1 (nth 0 ls)))))
                                         (wy (max 1 (ceiling (nth 1 gs) (max 1 (nth 1 ls)))))
                                         (wz (max 1 (ceiling (nth 2 gs) (max 1 (nth 2 ls))))))
                                    (wgpuComputePassEncoderDispatchWorkgroups cp wx wy wz))
                                  (wgpuComputePassEncoderEnd cp)
                                  (let* ((cb (wgpuCommandEncoderFinish ce (null-pointer))))
                                    (with-foreign-object (cbarr :pointer 1)
                                      (setf (mem-aref cbarr :pointer 0) cb)
                                      (wgpuQueueSubmit q 1 cbarr))
                                    (wgpuDevicePoll (wgpu-device runtime) t (null-pointer))))))))))))))))

(defmethod kernel-call ((k WebGPUKernel) (rt WebGPURuntime) node args) (apply (webgpu-caller k) rt node args))
