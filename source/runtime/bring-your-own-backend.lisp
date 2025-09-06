(defpackage :caten/runtime/bring-your-own-backend
  (:documentation "Provides a useful macro for defining a new accelerator")
  (:use :cl)
  (:export
   #:define-runtime
   #:define-buffer
   #:define-renderer
   #:define-kernel
   #:define-backend
   #:render
   ))

(in-package :caten/runtime/bring-your-own-backend)

(defmacro define-runtime (runtime-name direct-superclasses direct-slots)
  `(defclass ,runtime-name (,@direct-superclasses caten/ir:RuntimeGraph)
     ,direct-slots))
     
(defmacro define-buffer ((buffer-name runtime-name) direct-superclasses direct-slots
                         &key (open-buffer) (close-buffer) (transfer-from-array) (transfer-into-array) (bref))
  (flet ((ensure-lambda (n form name)
           (assert (listp (car form)))
           (assert (= n (length (car form))) () "the argument ~a excepts ~a arguments, getting ~a" n name form)
           (apply
            #'values
            (append
             (loop for i upfrom 0 below n
                   collect (nth i (car form)))
             (list
              (if (= 2 (length form))
                  (cdr form)
                  `(progn ,@(cdr form))))))))
    `(progn
       (defclass ,buffer-name (,@direct-superclasses caten/runtime/buffer:AbstractBuffer)
         ,direct-slots)
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

(defun render (x) (declare (ignore x)) (error "(render id) is only binded by define-renderer"))
;; [TODO] ここでSimplifier使えたほうが便利
(defmacro define-renderer (renderer-name direct-superclasses direct-slots &rest render-nodes)
  `(progn
     (defclass ,renderer-name (,@direct-superclasses caten/runtime/renderer:Renderer) ,direct-slots)
     ,@(loop for render-node in render-nodes
             for matcher = (car render-node)
             for form = (cdr render-node)
             for id = (car matcher)
             for args = (second matcher)
             for attrs = (cddr matcher)
             collect
             `(defmethod caten/runtime/renderer:%render-node ((renderer ,renderer-name) (node-id (eql ,id)) node)
                (flet ((render (id) (caten/runtime/renderer:render-node renderer id)))
                  (multiple-value-bind (,@args) (apply #'values (node-reads node))
                    (let* (,@(loop for i upfrom 0 below (length attrs) by 2
                                   for attr = (nth i attrs) for bind = (nth (1+ i) attrs)
                                   collect `(,bind (caten/graph:getattr node ,attr))))
                      ,@form)))))))

(defmacro define-kernel ((kernel-name renderer-name) direct-superclasses direct-slots &key (launch) (compile))
  `(progn
     (defclass ,kernel-name (,@direct-superclasses caten/runtime/kernel:Kernel) ,direct-slots)
     (defmethod caten/runtime/kernel:kernel-load-blueprint ((kernel ,kernel-name) (blueprint caten/ir:ASTGraph))
       (caten/runtime/kernel:%kernel-write-program
        blueprint ;; render ast
        kernel))
     (defmethod caten/runtime/kernel:kernel-compile ((kernel ,kernel-name) runtime)

       )
     (defmethod caten/runtime/kernel:kernel-launch ((kernel ,kernel-name) runtime)

       )))
