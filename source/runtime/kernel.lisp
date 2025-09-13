(defpackage :caten/runtime/kernel
  (:use :cl :caten/graph)
  (:export
   #:backend-get-kernel-cls
   #:Kernel
   #:kernel-name
   #:kernel-program
   #:%kernel-write-program
   #:kernel-args
   #:kernel-argtypes

   #:kernel-load-blueprint
   #:kernel-compile
   #:kernel-launch
   #:make-kernel
   #:render-kernel
   #:%render-kernel-op
   #:render-kernel-node

   #:ast-ensure-expr-before-range))

(in-package :caten/runtime/kernel)

(defgeneric backend-get-kernel-cls (backend-id))

(defclass Kernel ()
  ((name :initarg :name :reader kernel-name)
   (program :initarg :program :reader kernel-program :writer %kernel-write-program)
   (args :initarg :args :accessor kernel-args :initform nil :type list)
   (argtypes :initarg :argtypes :accessor kernel-argtypes :initform nil :type list)))

(defgeneric kernel-load-blueprint (kernel blueprint))
(defgeneric kernel-compile (kernel runtime))
(defgeneric kernel-launch (kernel runtime &rest args))

(defmethod kernel-launch :around ((kernel Kernel) runtime &rest args)
  ;; [TODO] Run type check (implement later)
  ;; (%check-kernel-args kernel args)
  (call-next-method))
;; Kernel-level rendering (top-level ASTGraph ops like :PROGN, :FOR, :IF)
(defgeneric %render-kernel-op (renderer node-id node))

(defun render-kernel-node (renderer node-id)
  (let* ((graph (caten/runtime/renderer::renderer-graph renderer))
         (node (caten/graph:id->value graph node-id)))
    (when node
      (%render-kernel-op renderer (caten/graph:node-type node) node))))

(defsimplifier
    (ast-ensure-expr-before-range :speed 0)
    ;; (RANGE 1 2) -> (RANGE (EXPR 1) (EXPR 2))
    ((:RANGE (x y) :dtype dtype :idx idx)
     ->
     ((node graph)
      (when (or (numberp x) (numberp y))
        (caten/ir:with-context-nodes
          (x (if (numberp x) (car (node-writes (caten/ir:%load (caten/ir:%salloc :dtype dtype) x))) x))
          (y (if (numberp y) (car (node-writes (caten/ir:%load (caten/ir:%salloc :dtype dtype) y))) y))
          (out (make-node :Render :RANGE (car (node-writes node)) (list x y) :dtype dtype :idx idx)))))))
       
(defun render-kernel (renderer blueprint)
  (let* ((outs (caten/graph:graph-outputs blueprint))
         (root (first outs)))
    (assert root () "Kernel blueprint must have a root output node.")
    (render-kernel-node renderer root)))

(defun make-kernel (name blueprint &key (backend (ctx:getenv :BACKEND)))
  (let* ((cls (backend-get-kernel-cls backend))
         (kernel (make-instance (or cls 'Kernel) :name name)))
    (kernel-load-blueprint kernel blueprint)
    kernel))
