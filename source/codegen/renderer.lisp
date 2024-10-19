(defpackage :caten/codegen/renderer
  (:use :cl)
  (:import-from #:caten/air #:node-type #:node-reads #:node-writes #:getattr #:id->value #:defnode)
  (:import-from #:caten/codegen/expr #:Expr #:expr-graph #:expr-out)
  (:export
   #:Renderer
   #:Default-Renderer
   #:renderer-graph
   #:render-expr
   #:render-node))

(in-package :caten/codegen/renderer)

(defnode (:Render :FOR) ()
         "TODO"
         :slots ((idx :type symbol)
                 (upfrom :type Expr)
                 (below :type Expr)
                 (by :type Expr)))

(defnode (:Render :ENDFOR) ()
         "TODO"
         :slots ((idx :type symbol)))

(defclass Renderer ()
  ((graph :initarg :graph :accessor renderer-graph))
  (:documentation "TODO"))

(defclass Default-Renderer (Renderer)
  nil
  (:documentation "Default Renderer used to print-object in repl"))

(defgeneric %render-node (renderer node-dispatcher node) (:documentation ""))

(defun render-expr (renderer-id expr)
  (render-node (make-instance renderer-id :graph (expr-graph expr)) (car (node-writes (expr-out expr)))))

(defun render-node (renderer id)
  (declare (type Renderer renderer))
  (when (numberp id)
    ;; [TODO] Rewrite as :Const Node
    (print id))
  (assert (symbolp id) () "render-node: id must be a symbol. getting ~a" id)
  (let ((val (id->value (renderer-graph renderer) id)))
    (assert val () "render-node: ~a is not found from the graph." val)
    (%render-node renderer (node-type val) val)))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :LOAD)) node)
  (format nil "~a" (getattr node :value)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-node renderer (nth 0 (node-reads node))) ,op (render-node renderer (nth 1 (node-reads node)))))))
  (def :ADD "+")
  (def :MUL "*")
  (def :AND " and ")
  (def :OR " or "))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a)" ,op (render-node renderer (nth 0 (node-reads node)))))))
  (def :NEG "-")
  (def :NOT "!"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-node renderer (nth 1 (node-reads node))) ,op (render-node renderer (nth 2 (node-reads node)))))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer Default-Renderer) id node)
  (format nil "~a~a" (node-type node) (map 'list #'(lambda (x) (render-node renderer x)) (node-reads node))))

(defmethod print-object ((expr expr) stream)
  (print-unreadable-object (expr stream :type t)
    (format stream "~a" (render-node (make-instance 'Default-Renderer :graph (expr-graph expr)) (car (node-writes (expr-out expr)))))))
