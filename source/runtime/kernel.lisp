(defpackage :caten/runtime/kernel
  (:use :cl)
  (:export))

(in-package :caten/runtime/kernel)

(defclass Kernel () nil)

(defgeneric kernel-from-blueprint (kernel blueprint))
(defgeneric kernel-compile (kernel))
(defgeneric kernel-launch (kernel runtime args))
  
