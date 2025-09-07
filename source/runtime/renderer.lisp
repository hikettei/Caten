(defpackage :caten/runtime/renderer
  (:use :cl)
  (:export
   #:backend-get-renderer-cls
   #:Renderer
   #:render-node
   #:render-const
   #:%render-node
   #:%render-const))
(in-package :caten/runtime/renderer)

(defgeneric backend-get-renderer-cls (backend-id))

(defclass Renderer ()
  ((graph :initarg :graph :accessor renderer-graph)))

(defgeneric %render-node (renderer node-id node))
(defgeneric %render-const (renderer obj))

(defun render-node (renderer node)
  )

;; :IF :EXPR etc ...
;; - [ ] %render-nodeを使って実装可能？
