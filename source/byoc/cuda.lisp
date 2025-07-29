;; Not enough tested!!
(defpackage :caten/byoc/cuda
  (:use :cl :cffi
        :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime
        :caten/codegen/byoc :caten/codegen/renderer :caten/air
        :caten/aasm :caten/aasm/expr :caten/codegen/helpers :caten/codegen/iteration)
  (:export #:CudaBuffer #:CudaRuntime #:cu-compile-source))
(in-package :caten/byoc/cuda)
;; ----------------------------------------------------------------------------------------------------------------
;; CFFI Bindings（NVRTC / CUDA Driver API）
;; ----------------------------------------------------------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (handler-case
      (progn
        (load-foreign-library "libnvrtc")
        (load-foreign-library "libcuda"))
    (t (c) (declare (ignore c)))))

(defclass CudaRuntime (GraphRuntime)
  ((device  :accessor cuda-device)
   (context :accessor cuda-context)
   (stream  :accessor cuda-stream)))
(defclass CudaBuffer (AbstractBuffer) nil)
(defclass Cuda-Renderer (CStyle-Renderer) nil)

(defclass CudaKernel (AbstractKernel)
  ((program  :accessor cuda-program)
   (module   :accessor cuda-module)
   (fxn      :accessor cuda-fxn)
   (caller   :accessor cuda-caller)))

(defclass Cuda-Program ()
  ((module    :accessor cp-module)
   (fxn       :accessor cp-function)
   (grid-size :accessor cp-grid-size)
   (argtypes  :accessor cp-argtypes)))

(defctype CUdevice       :int)
(defctype CUcontext      :pointer)
(defctype CUmodule       :pointer)
(defctype CUfunction     :pointer)
(defctype CUstream       :pointer)
(defctype CUdeviceptr    :pointer)
(defconstant +cuda-success+ 0)

(defun check-cuda (err msg)
  (unless (= err +cuda-success+)
    (error "CUDA Error (~a): ~a" err msg)))
;; -- Driver API ----------------------------------------------------------------
(defcfun ("cuInit"               cuInit)              :int (flags :uint))
(defcfun ("cuDeviceGet"          cuDeviceGet)         :int (device :pointer) (ordinal :int))
(defcfun ("cuCtxCreate_v2"       cuCtxCreate)         :int (pctx :pointer) (flags :uint) (dev CUdevice))
(defcfun ("cuCtxDestroy_v2"      cuCtxDestroy)        :int (ctx CUcontext))
(defcfun ("cuMemAlloc_v2"        cuMemAlloc)          :int (dptr :pointer) (bytes :size))
(defcfun ("cuMemFree_v2"         cuMemFree)           :int (dptr CUdeviceptr))
(defcfun ("cuMemcpyHtoD_v2"      cuMemcpyHtoD)        :int (dst CUdeviceptr) (src :pointer) (bytes :size))
(defcfun ("cuMemcpyDtoH_v2"      cuMemcpyDtoH)        :int (dst :pointer) (src CUdeviceptr) (bytes :size))
(defcfun ("cuModuleLoadDataEx"   cuModuleLoadDataEx)  :int (module :pointer) (image :pointer) (nopt :uint) (opts :pointer) (vals :pointer))
(defcfun ("cuModuleGetFunction"  cuModuleGetFunction) :int (func :pointer) (mod CUmodule) (name :string))
(defcfun ("cuLaunchKernel"       cuLaunchKernel)      :int
         (func CUfunction)
         (gx :uint) (gy :uint) (gz :uint)
         (bx :uint) (by :uint) (bz :uint)
         (shared :uint) (stream CUstream)
         (kernelParams :pointer) (extra :pointer))
(defcfun ("cuStreamCreate"       cuStreamCreate)      :int (pstream :pointer) (flags :uint))
(defcfun ("cuStreamDestroy_v2"   cuStreamDestroy)     :int (stream CUstream))
(defcfun ("cuStreamSynchronize"  cuStreamSynchronize) :int (stream CUstream))
;; -- NVRTC ---------------------------------------------------------------------
(defcfun ("nvrtcCreateProgram"   nvrtcCreateProgram)  :int (prog :pointer)
         (src :string) (name :string) (num-headers :int) (headers :pointer) (include-names :pointer))
(defcfun ("nvrtcCompileProgram"  nvrtcCompileProgram) :int (prog :pointer) (numOptions :int) (options :pointer))
(defcfun ("nvrtcDestroyProgram"  nvrtcDestroyProgram) :int (prog :pointer))
(defcfun ("nvrtcGetPTXSize"      nvrtcGetPTXSize)     :int (prog :pointer) (ptxSizeRet :pointer))
(defcfun ("nvrtcGetPTX"          nvrtcGetPTX)         :int (prog :pointer) (ptx :pointer))
(defcfun ("nvrtcGetProgramLogSize" nvrtcGetProgramLogSize) :int (prog :pointer) (logsize :pointer))
(defcfun ("nvrtcGetProgramLog"   nvrtcGetProgramLog)  :int (prog :pointer) (log :pointer))
;; ----------------------------------------------------------------------------------------------------------------
;; NVRTC Compilation Utility
;; ----------------------------------------------------------------------------------------------------------------
(defun cu-compile-source (source &key (arch "compute_75") (extra-options '("--std=c++14")))
  (with-foreign-objects ((prog :pointer))
    (check-cuda
     (nvrtcCreateProgram prog source "jit_kernel.cu" 0 (null-pointer) (null-pointer))
     "nvrtcCreateProgram")
    (let* ((opts (coerce (append (list (format nil "--gpu-architecture=~a" arch)) extra-options) 'vector))
           (n     (length opts)))
      (with-foreign-objects ((copts :pointer n))
        (dotimes (i n)
          (setf (mem-aref copts :pointer i) (foreign-string-alloc (aref opts i))))
        (let ((ret (nvrtcCompileProgram prog n copts)))
          (dotimes (i n) (foreign-string-free (mem-aref copts :pointer i)))
          (when (/= ret +cuda-success+)
            (with-foreign-objects ((sz :size))
              (nvrtcGetProgramLogSize prog sz)
              (let ((log-buf (make-array (mem-ref sz :size) :element-type '(unsigned-byte 8))))
                (with-pointer-to-vector-data (*log log-buf)
                  (nvrtcGetProgramLog prog *log))
                (error "NVRTC compilation failed:~%~a" (flexi-streams:octets-to-string log-buf))))))
      (with-foreign-objects ((size :size))
        (nvrtcGetPTXSize prog size)
        (let* ((nbytes (mem-ref size :size))
               (vec    (make-array nbytes :element-type '(unsigned-byte 8))))
          (with-pointer-to-vector-data (*dst vec)
            (nvrtcGetPTX prog *dst))
          (nvrtcDestroyProgram prog)
          vec))))))
;; ----------------------------------------------------------------------------------------------------------------
;;  CUDA Buffer / Runtime
;; ----------------------------------------------------------------------------------------------------------------

(defun dtype->cffi (dtype)
  (ecase dtype
    (:bool   :uint8)
    (:float32 :float)
    (:float64 :double)
    (:int64  :int64)
    (:uint64 :uint64)
    (:int32  :int32)
    (:uint32 :uint32)
    (:int16  :int16)
    (:uint16 :uint16)
    (:int8   :int8)
    (:uint8  :uint8)
    (:bfloat16 :uint16)))

(defmethod open-buffer ((runtime CudaRuntime) (buffer CudaBuffer))
  (let ((init (if (eql (buffer-dtype buffer) :bool)
                  0
                  (coerce 0 (dtype->lisp (buffer-dtype buffer))))))
    (if (= 0 (buffer-nrank buffer))
        (setf (buffer-value buffer) init)
        (let ((bytes (* (buffer-storage-size buffer) (dtype/size-of (buffer-dtype buffer)))))
          (with-foreign-objects ((dptr :pointer))
            (check-cuda (cuMemAlloc dptr bytes) "cuMemAlloc")
            (setf (buffer-value buffer) (mem-ref dptr :pointer)))))))

(defmethod close-buffer ((runtime CudaRuntime) (buffer CudaBuffer))
  (when (pointerp (buffer-value buffer))
    (cuMemFree (buffer-value buffer))
    (setf (buffer-value buffer) nil)))

(defmethod transfer-from-array ((runtime CudaRuntime) (buffer CudaBuffer) array)
  ;; CPU → GPU
  (assert (typep array 'array))
  (let* ((elts (length array))
         (bytes (* elts (dtype/size-of (buffer-dtype buffer)))))
    (with-pointer-to-vector-data (*src array)
      (check-cuda (cuMemcpyHtoD (buffer-value buffer) *src bytes) "cuMemcpyHtoD"))))

(defmethod transfer-into-array ((buffer CudaBuffer))
  ;; GPU → CPU
  (if (numberp (buffer-value buffer))
      (buffer-value buffer)
      (let* ((vec (make-array (buffer-storage-size buffer) :element-type (dtype->lisp (buffer-dtype buffer))))
             (bytes (* (length vec) (dtype/size-of (buffer-dtype buffer)))))
        (with-pointer-to-vector-data (*dst vec)
          (check-cuda (cuMemcpyDtoH *dst (buffer-value buffer) bytes) "cuMemcpyDtoH"))
        vec)))

(defmethod copy-buffer-value ((runtime CudaRuntime) (buffer CudaBuffer))
  (let ((clone (copy-buffer buffer)))
    (transfer-from-array runtime clone (transfer-into-array buffer))
    (buffer-value clone)))

(defmethod bref ((buffer CudaBuffer) idx)
  (let* ((dst (make-array 1 :element-type (dtype->lisp (buffer-dtype buffer))))
         (bytes (dtype/size-of (buffer-dtype buffer))))
    (with-pointer-to-vector-data (*p dst)
      (let ((offset (* idx bytes)))
        (check-cuda (cuMemcpyDtoH *p (inc-pointer (buffer-value buffer) offset) bytes) "cuMemcpyDtoH")))
    (aref dst 0)))
;; ----------------------------------------------------------------------------------------------------------------
;;  CudaRuntime
;; ----------------------------------------------------------------------------------------------------------------
(defmethod initialize-instance :after ((rt CudaRuntime) &key)
  (check-cuda (cuInit 0) "cuInit")
  (with-foreign-objects ((dev :int) (ctx :pointer) (strm :pointer))
    (check-cuda (cuDeviceGet dev 0) "cuDeviceGet")
    (check-cuda (cuCtxCreate ctx 0 (mem-ref dev :int)) "cuCtxCreate")
    (check-cuda (cuStreamCreate strm 0) "cuStreamCreate")
    (setf (cuda-device  rt) (mem-ref dev :int)
          (cuda-context rt) (mem-ref ctx :pointer)
          (cuda-stream  rt) (mem-ref strm :pointer))))

(defmethod finalize ((rt CudaRuntime))
  (cuStreamDestroy (cuda-stream rt))
  (cuCtxDestroy   (cuda-context rt)))
;; ----------------------------------------------------------------------------------------------------------------
;; Renderer / Kernel
;; ----------------------------------------------------------------------------------------------------------------
;; -- Auto‑Scheduler -------------------------------------------------------------------------------
(define-auto-scheduler Cuda-Auto-Scheduler
  :n-profile 1 :per-band-optrules 2
  :ptile-max-rank 3 :shared-max 49152)

(define-backend :cuda CudaBuffer CudaRuntime Cuda-Renderer CudaKernel Cuda-Auto-Scheduler t)
;; --（blockIdx / threadIdx） --------------------------------------------------------------------------------------
(defmethod %render-node ((renderer Cuda-Renderer) (id (eql :SPACE)) node)
  (let* ((lv  (ecase (getattr node :level) (:block "blockIdx") (:thread "threadIdx")))
         (dim (ecase (getattr node :rank)  (0 ".x") (1 ".y") (2 ".z"))))
    (format nil "~a~a" lv dim)))
;; -- Renderer -----------------------------------------------------------------------------------------------------
(defun cuda-header ()
  (format nil "#include <math.h>
#define _infinity INFINITY
#define _negative_infinity -INFINITY
#define _nan NAN
#define min(a,b) ((a)<(b)?(a):(b))
#define max(a,b) ((a)>(b)?(a):(b))

"))

(defun render-bp-cuda (graph out &aux (indent 0) (seen))
  (labels ((indent-line () (make-string indent :initial-element #\Space))
           (fmt (str &rest args) (apply #'format out (format nil "~a~a~%" (indent-line) str) args))
           (r (s &aux (v (id->value graph s)))
             (when (and v (null (find (node-id v) seen)))
               (f v) (push (node-id v) seen)))
           (e (id) (render-node (make-instance 'Cuda-Renderer :graph graph) id))
           (f (node)
             (case (node-type node)
               (:PROGN (fmt "{") (incf indent 2) (mapc #'r (node-reads node)) (decf indent 2) (fmt "}"))
               (:EXPR
                (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                    (fmt "~a;" (e (car (node-reads node))))
                    (let ((type (car (relay-writes (read-type-relay node)))))
                      (fmt "~a ~(~a~) = ~a;"
                           (->cdtype (tensor-relay-dtype type)) (car (node-writes node)) (e (car (node-reads node)))))))
               (:FOR
                (multiple-value-bind (range body) (apply #'values (node-reads node))
                  (setf range (id->value graph range))
                  (multiple-value-bind (bind size step) (values (getattr range :idx) (first (node-reads range)) (second (node-reads range)))
                    (when (symbolp size) (setf size (e (car (node-reads (id->value graph size))))))
                    (when (symbolp step) (setf step (e (car (node-reads (id->value graph step))))))
                    (fmt "for (int ~(~a~)=0; ~(~a~)<~(~a~); ~(~a~)+=~a)" bind bind size bind step))
                  (unless (eql (node-type (id->value graph body)) :PROGN) (incf indent 2))
                  (r body)
                  (unless (eql (node-type (id->value graph body)) :PROGN) (decf indent 2))))
               (:IF
                (multiple-value-bind (cond body) (apply #'values (node-reads node))
                  (fmt "if (~(~a~)) {" (e (car (node-reads (id->value graph cond)))))
                  (incf indent 2) (r body) (decf indent) (fmt "}")))
               (:BARRIER (fmt "__syncthreads();"))
               (:DEFINE-SHARED-MEMORY
                (fmt "__shared__ ~(~a~) ~(~a~)[~a];"
                     (->cdtype (getattr node :dtype)) (car (node-writes node)) (getattr node :size)))
               (:DEFINE-GLOBAL) (:RANGE) (:ALLOCATE)
               (otherwise (error "Unsupported node type ~a for CUDA renderer" (node-type node))))))
    (f (id->value graph (car (graph-outputs graph))))))

(defmethod %render-kernel ((renderer Cuda-Renderer) kernel)
  (let ((args (kernel-args kernel)))
    (setf (cuda-program kernel)
          (with-output-to-string (out)
            (format out "__global__ void ~(~a~)(" (kernel-name kernel))
            (dolist (arg args)
              (format out "~a ~(~a~) ~a~(~a~), "
                      (case (getattr arg :mode) (:read "const") (otherwise ""))
                      (->cdtype (getattr arg :dtype))
                      (if (getattr arg :pointer-p) "*" "&")
                      (car (node-writes arg))))
            (format out ") {~%")
            (render-bp-cuda (kernel-blueprint kernel) out)
            (format out "}~%~%")))))
;; ----------------------------------------------------------------------------------------------------------------
;;  Caller
;; ----------------------------------------------------------------------------------------------------------------
(defun make-cuda-caller (prog)
  `(lambda (runtime node &rest largs)
     (let* ((params  (map 'list #'cons (node-reads node) largs))
            (g-sz    (map 'list #'(lambda (x) (expr-realize-as-value (nth 0 x) params)) (cp-grid-size ,prog)))
            (l-sz    (map 'list #'(lambda (x) (expr-realize-as-value (nth 1 x) params)) (cp-grid-size ,prog)))
            (blocks  (map 'list #'(lambda (g l) (max 1 (ceiling g l))) g-sz l-sz))
            (nargs   (length largs)))
       (caten/runtime/profile:with-real-time
         (with-foreign-objects ((kparams :pointer nargs))
           (dotimes (i nargs)
             (let ((arg (nth i largs)))
               (setf (mem-aref kparams :pointer i)
                     (cond
                       ((typep arg 'CudaBuffer) (buffer-value arg))
                       ((and (buffer-p arg) (numberp (buffer-value arg)))
                        (with-foreign-object (tmp (dtype->cffi (getattr arg :dtype)))
                          (setf (mem-ref tmp (dtype->cffi (getattr arg :dtype))) (buffer-value arg))
                          tmp))
                       ((numberp arg)
                        (with-foreign-object (tmp :double)
                          (setf (mem-ref tmp :double) arg) tmp))
                       (t (error "Unsupported arg type ~a" arg))))))
           ;; cuLaunchKernel
           (check-cuda
            (cuLaunchKernel (cp-function ,prog)
                            (nth 0 blocks) (nth 1 blocks) (nth 2 blocks)
                            (nth 0 l-sz)    (nth 1 l-sz)   (nth 2 l-sz)
                            0 (cuda-stream runtime) kparams (null-pointer))
            "cuLaunchKernel")
           (check-cuda (cuStreamSynchronize (cuda-stream runtime)) "cuStreamSynchronize"))))))
;; ----------------------------------------------------------------------------------------------------------------
;;  %compile-kernel
;; ----------------------------------------------------------------------------------------------------------------
(defmethod %compile-kernel ((renderer Cuda-Renderer) items dir)
  (let* ((code (apply #'concatenate 'string (cons (cuda-header) (map 'list #'cuda-program items)))))
    (when (>= (ctx:getenv :JIT_DEBUG) 3)
      (format t "[CUDA Final Code]:~%~a~%" code))
    (let* ((ptx (cu-compile-source code))
           (module (with-foreign-objects ((mod :pointer))
                     (with-pointer-to-vector-data (*ptx ptx)
                       (check-cuda (cuModuleLoadDataEx mod *ptx 0 (null-pointer) (null-pointer))
                                   "cuModuleLoadDataEx"))
                     (mem-ref mod :pointer))))
      (dolist (item items)
        (with-foreign-objects ((func :pointer))
          (check-cuda (cuModuleGetFunction func module (string-downcase (princ-to-string (kernel-name item))))
                      "cuModuleGetFunction")
          (let* ((argtypes (map 'list #'(lambda (x) (getattr x :dtype)) (kernel-args item)))
                 (prog (make-instance 'Cuda-Program :module module :fxn (mem-ref func :pointer)
                                      :grid-size (caten/codegen/blueprint:blueprint-gather-grids (kernel-blueprint item))
                                      :argtypes argtypes)))
            (setf (cp-function prog) (mem-ref func :pointer))
            (setf (cuda-module item) module
                  (cuda-fxn    item) (mem-ref func :pointer)
                  (cuda-caller item) (compile nil (make-cuda-caller prog)))))))))

(defmethod kernel-call ((kernel CudaKernel) (runtime CudaRuntime) node args)
  (apply (cuda-caller kernel) runtime node args))
