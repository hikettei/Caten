(defpackage :caten/runtime/bring-your-own-backend
  (:documentation "Provides a useful macro for defining a new accelerator")
  (:use :cl)
  (:export
   #:define-buffer
   ))

(in-package :caten/runtime/bring-your-own-backend)

(defmacro define-buffer ((buffer-name runtime-name) direct-superclasses direct-slots
                         &key (open-buffer) (close-buffer) (transfer-from-array) (transfer-into-array) (bref))
  (flet ((ensure-lambda (n form name)
           (assert (listp (car form)))
           (assert (= n (length form)) () "the argument ~a excepts ((runtime buffer) body)" name)
           (apply
            #'values
            (loop for i upfrom 0 below n
                  collect (nth i form))
            (if (= 2 (length form))
                (cdr form)
                `(progn ,@(cdr form))))))
    `(progn
       (defclass ,buffer-name (,@direct-superclasses caten/runtime/buffer:AbstractBuffer)
         ,@direct-slots)
       ,(multiple-value-bind (runtime buffer form) (ensure-lambda 2 open-buffer "open_buffer")
          `(defmethod caten/runtime/buffer:open-buffer ((,runtime ,runtime-name) (,buffer ,buffer-name))
             ,@form))
       ,(multiple-value-bind (runtime buffer form) (ensure-lambda 2 close-buffer "close_buffer")
          `(defmethod caten/runtime/buffer:close-buffer ((,runtime ,runtime-name) (,buffer ,buffer-name))
             ,@form))
       ,(multiple-value-bind (runtime buffer array form) (ensure-lambda 3 transfer-from-array "transfer_from_array")
          `(defmethod caten/runtime/buffer:transfer-from-array ((,runtime ,runtime-name) (,buffer ,buffer-name) ,array)
             ,@form))
       ,(multiple-value-bind (buffer form) (ensure-lambda 1 transfer-into-array "transfer_into_array")
          `(defmethod caten/runtime/buffer:transfer-into-array ((,buffer ,buffer-name))
             ,@form))
       ,(multiple-value-bind (buffer index form) (ensure-lambda 2 bref "bref")
          `(defmethod caten/runtime/buffer:bref ((,buffer ,buffer-name) ,index) ,@form)))))

(defmacro define-runtime ())
(defmacro define-renderer ())
;; :ADD :SUBとかの必須のやつ一覧をRequiredにする
;; :alloc
;; :deallocとかあったほうがいいかも
