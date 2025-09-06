(defpackage :caten/runtime/renderer
  (:use :cl)
  (:export
   #:Renderer
   #:render-node
   #:render-const
   #:%render-node
   #:%render-const))
(in-package :caten/runtime/renderer)

(defclass Renderer ()
  ((graph :initarg :graph :accessor renderer-graph)))

(defgeneric %render-node (renderer node-id node))
(defgeneric %render-const (renderer obj))

