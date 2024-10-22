(defpackage :caten/codegen/renderer
  (:use :cl :caten/codegen/shape-inference)
  (:import-from #:caten/air #:node-type #:node-reads #:node-writes #:getattr #:id->value #:defnode #:make-node)
  (:import-from #:caten/codegen/expr #:Expr #:expr-graph #:expr-out)
  (:import-from :caten/avm :Buffer #:buffer-nrank)
  (:export
   #:Renderer
   #:Default-Renderer
   #:renderer-graph
   #:render-expr
   #:render-node
   #:%render-node
   #:%render-const
   #:make-aref))

(in-package :caten/codegen/renderer)

(defgeneric %render-node (renderer node-dispatcher node) (:documentation ""))
(defgeneric %render-const (renderer obj) (:documentation ""))

(defnode (:Render :FOR) ()
         "TODO"
         :slots ((idx :type symbol)
                 (upfrom :type Expr)
                 (below :type Expr)
                 (by :type Expr)))

(defnode (:Render :ENDFOR) ()
         "TODO"
         :slots ((idx :type symbol)))

(defnode (:Render :Aref) ()
         "TODO"
         :slots ((buffer :type Buffer)
                 (space :type Iteration-Space)))

(defun make-aref (name buffer space)
  (declare (type symbol name)
           (type buffer buffer)
           (type Iteration-Space space))
  (make-node :Render :Aref (list name) nil :buffer buffer :space space))

(defclass Renderer ()
  ((graph :initarg :graph :accessor renderer-graph))
  (:documentation "TODO"))

(defun render-expr (renderer-id expr)
  (render-node (make-instance renderer-id :graph (expr-graph expr)) (car (node-writes (expr-out expr)))))

(defun render-node (renderer id)
  (declare (type Renderer renderer))
  (when (numberp id)
    (return-from render-node (%render-const renderer id)))
  (assert (symbolp id) () "render-node: id must be a symbol. getting ~a" id)
  (let ((val (id->value (renderer-graph renderer) id)))
    (assert val () "render-node: ~a is not found from the graph.~%graph:~%~a" id (renderer-graph renderer))
    (%render-node renderer (node-type val) val)))

;; ~~ Default Renderer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Default-Renderer (Renderer)
  nil
  (:documentation "Default Renderer used to print-object in repl"))

(defmethod %render-const ((renderer Default-Renderer) obj)
  (format nil "~(~a~)" obj))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :LOAD)) node)
  (format nil "~a" (getattr node :value)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-node renderer (nth 0 (node-reads node))) ,op (render-node renderer (nth 1 (node-reads node)))))))
  (def :ADD "+")
  (def :MUL "*")
  (def :AND " and ")
  (def :OR " or ")
  (def :XOR " xor "))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a, ~a)" ,op (render-node renderer (nth 0 (node-reads node))) (render-node renderer (nth 1 (node-reads node)))))))
  (def :MAX "max"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a)" ,op (render-node renderer (nth 0 (node-reads node)))))))
  (def :NEG "-")
  (def :NOT "!")
  (def :SIN "sin")
  (def :log2 "log2")
  (def :exp2 "exp2")
  (def :RECIP "1/"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-node renderer (nth 1 (node-reads node))) ,op (render-node renderer (nth 2 (node-reads node)))))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Aref)) node)
  (let ((buffer (getattr node :buffer))
        (space  (getattr node :space)))
    (if (= -1 (buffer-nrank buffer))
        (format nil "~(~a~)" (car (node-writes node)))
        ;; [TODO],pass gids w/o using global variable ...
        (format nil "~a[...]" (car (node-writes node))))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Allocate)) node) (format nil "0"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :INDEX-COMPONENTS)) node)
  ;; [TODO]
  (format nil "<INDEX-COMPONENTS>"))

(defmethod %render-node ((renderer Default-Renderer) id node)
  (format nil "~a~a" (node-type node) (map 'list #'(lambda (x) (render-node renderer x)) (node-reads node))))

(defmethod print-object ((expr expr) stream)
  (print-unreadable-object (expr stream :type t)
    (format stream "~a" (render-node (make-instance 'Default-Renderer :graph (expr-graph expr)) (car (node-writes (expr-out expr)))))))
