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
(defgeneric %render-const (renderer obj dtype))

(defun render-node (renderer node)
  (declare (type Renderer renderer) (type symbol node))
  (let ((node (caten/graph:id->value (renderer-graph renderer) node)))
    (assert node)
    (%render-node renderer (caten/graph:node-type node) node)))

;; :IF :EXPR etc ...
;; - [ ] %render-nodeを使って実装可能？
