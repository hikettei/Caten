(defpackage :caten/byoc/metal
  (:use :cl :caten/runtime/buffer :caten/runtime/runtime :caten/common.dtype :caten/codegen/backend :cffi :flexi-streams :float-features))

(in-package :caten/byoc/metal)

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
;; ~~ Extension ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass MetalBuffer (AbstractBuffer) nil)
(defclass MetalRuntime (GraphRuntime)
  ((device :accessor metal-runtime-device)))

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

(defmethod bref ((buffer MetalBuffer) idx)
  (let ((val (msg (buffer-value buffer) "contents" :pointer)))
    (mem-aref val (caten/codegen/helpers:->cffi-dtype (buffer-dtype buffer)) idx)))

(define-backend :metal MetalBuffer MetalRuntime nil nil t)
