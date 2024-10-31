(defpackage :caten/codegen/renderer
  (:use :cl :caten/codegen/shape-inference)
  (:import-from #:caten/air #:node-type #:node-reads #:node-writes #:getattr #:id->value #:defnode #:make-node #:graph-nodes)
  (:import-from #:caten/codegen/expr #:Expr #:expr-graph #:expr-out #:expr-p #:expr-add #:expr-mul #:expr-const #:expr-scalar-equivalent-p)
  (:import-from :caten/avm :Buffer #:buffer-nrank)
  (:import-from #:caten/codegen/helpers #:simplify-arithmetic-code #:->cdtype #:float-type-of)
  (:export
   #:get-default-renderer
   #:%render-kernel
   #:%compile-kernel
   #:Renderer
   #:Default-Renderer
   #:CStyle-Renderer
   #:renderer-graph
   #:renderer-index-space
   #:make-renderer
   #:render-expr
   #:render-aref
   #:render-node
   #:%render-node
   #:%render-const
   #:expr-index-components
   #:make-aref
   #:make-define-global))

(in-package :caten/codegen/renderer)

(defgeneric get-default-renderer (id))
(defgeneric %render-node (renderer node-dispatcher node) (:documentation ""))
(defgeneric %render-const (renderer obj) (:documentation ""))

(defgeneric %render-kernel (renderer schedule-item))
(defgeneric %compile-kernel (renderer schedule-items dir))

(defnode (:Render :FOR) ()
         "TODO"
         :slots ((idx :type symbol)
                 (upfrom :type Expr)
                 (below :type Expr)
                 (by :type Expr)))

(defnode (:Render :ENDFOR) ()
         "TODO"
         :slots ((idx :type symbol)))

(defnode (:Render :IF) ()
         "TODO"
         :slots ((condition :type Expr)))

(defnode (:Render :ENDIF) ()
         "TODO")

(defnode (:Render :Aref) ()
         "TODO"
         :slots ((buffer :type Buffer)
                 (space :type Iteration-Space)))

(defnode (:Render :DEFINE-GLOBAL) ()
         ""
         :slots ((dtype :type keyword)
                 (pointer-p :type boolean)
                 (type :type (member :input :output :shape))
                 (nrank :type integer)))

(defun make-define-global (id dtype pointer-p type nrank)
  (declare (type symbol id)
           (type keyword dtype)
           (type boolean pointer-p))
  (make-node :Render :DEFINE-GLOBAL (list id) nil :dtype dtype :pointer-p pointer-p :type type :nrank nrank))

(defun make-aref (name buffer space)
  (declare (type symbol name)
           (type buffer buffer)
           (type Iteration-Space space))
  (make-node :Render :Aref (list name) nil :buffer buffer :space space))

(defclass Renderer ()
  ((graph :initarg :graph :accessor renderer-graph)
   (index-space :initarg :index-space :type list :initform nil :accessor renderer-index-space))
  (:documentation "TODO"))

(defun make-renderer (renderer-name graph index-space &rest initargs)
  (assert (every #'expr-p index-space) () "index-space is a list of exprs!")
  (apply #'make-instance renderer-name :graph graph :index-space index-space initargs))

(defun render-expr (renderer-id expr &key (index-space) (initargs))
  (assert (every #'expr-p index-space) () "index-space is a list of exprs!")
  (render-node
   (apply #'make-instance renderer-id :graph (expr-graph expr) :index-space index-space initargs)
   (car (node-writes (expr-out expr)))))

(defun render-node (renderer id)
  (declare (type Renderer renderer))
  (when (numberp id)
    (return-from render-node (%render-const renderer id)))
  (assert (symbolp id) () "render-node: id must be a symbol. getting ~a" id)
  (let ((val (id->value (renderer-graph renderer) id)))
    (assert val () "render-node: ~a is not found from the graph.~%graph:~%~a" id (renderer-graph renderer))
    (or
     (%render-node renderer (node-type val) val)
     ;; Purged from the graph -> replace w/ 0 (TODO: Fix this)
     (%render-const renderer 0))))

(defun render-aref (renderer node)
  (assert (eql (node-type node) :AREF))
  (let ((buffer (getattr node :buffer))
        (space  (getattr node :space))
        (index-space (renderer-index-space renderer)))
    (when (and (null index-space) (> (buffer-nrank buffer) 0))
      (warn "render-aref: Cannot render :AREF without providing :index-space, thus replaced with ?."))
    (if (= -1 (buffer-nrank buffer))
        (format nil "~(~a~)" (car (node-writes node)))
        (if index-space
            (let ((expr (apply #'expr-add (iteration-space-expr-aref space buffer (renderer-index-space renderer)))))
              (setf (graph-nodes (renderer-graph renderer))
                    (append
                     (graph-nodes (expr-graph expr))
                     (graph-nodes (renderer-graph renderer))))
              (format nil "~(~a~)[~a]"
                      (%render-const renderer (car (node-writes node)))
                      (render-node renderer (car (node-writes (expr-out expr))))))
            (format nil "~(~a~)[?]" (car (node-writes node)))))))

(defun expr-index-components (is index-space)
  (reduce
   #'expr-add
   (map
    'list
    #'(lambda (shape stride gid)
        (if (expr-scalar-equivalent-p shape (expr-const 1 :int64))
            (expr-const 0 :int64)
            (expr-mul stride (expr-const gid :int64))))
    (iteration-space-shape is)
    (iteration-space-strides is)
    index-space)))
;; ~~ Default Renderer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Default-Renderer (Renderer)
  nil
  (:documentation "Default Renderer used to print-object in repl"))

(defmethod %render-const ((renderer Default-Renderer) obj)
  (format nil "~(~a~)" obj))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :LOAD)) node)
  (%render-const renderer (getattr node :value)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (let ((lhs (render-node renderer (nth 0 (node-reads node))))
                      (rhs (render-node renderer (nth 1 (node-reads node)))))
                  (simplify-arithmetic-code (format nil "(~a~a~a)" lhs ,op rhs))))))
  (def :ADD "+")
  (def :MUL "*")
  (def :AND " and ")
  (def :OR " or ")
  (def :XOR " xor "))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :WMMA)) node)
  (format nil "~a+~a*~a" (render-node renderer (nth 0 (node-reads node))) (render-node renderer (nth 1 (node-reads node))) (render-node renderer (nth 2 (node-reads node)))))

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
  (def :RECIP "1/")
  (def :SQRT "sqrt"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-node renderer (nth 1 (node-reads node))) ,op (render-node renderer (nth 2 (node-reads node)))))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Aref)) node)
  (render-aref renderer node))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Allocate)) node) (format nil "0"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :CAST)) node)
  (format nil "(~(~a~))~a" (getattr node :dtype) (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :INDEX-COMPONENTS)) node)
  (render-expr 'Default-Renderer (expr-index-components (car (relay-read-iters (read-type-relay node))) (renderer-index-space renderer))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :WHERE)) node)
  (format nil "~a ? ~a : ~a"
          (render-node renderer (car (node-reads node)))
          (render-node renderer (second (node-reads node)))
          (render-node renderer (third (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) id node)
  (format nil "~a~a" (node-type node) (map 'list #'(lambda (x) (render-node renderer x)) (node-reads node))))

(defmethod print-object ((expr expr) stream)
  (print-unreadable-object (expr stream :type t)
    (format stream "~a" (render-node (make-instance 'Default-Renderer :graph (expr-graph expr)) (car (node-writes (expr-out expr)))))))

;; ~~ CStyle Renderer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass CStyle-Renderer (Renderer)
  nil
  (:documentation ""))

(defmethod get-default-renderer ((id (eql :clang))) (make-instance 'CStyle-Renderer))
(defmethod %render-const ((renderer CStyle-Renderer) obj)
  (case (if (numberp obj)
             (float-type-of obj)
             t)
    (:inf "_infinity")
    (:-inf "_negative_infinity")
    (:nan "_nan")
    (otherwise (format nil "~(~a~)" obj))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :LOAD)) node)
  (%render-const renderer (getattr node :value)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (simplify-arithmetic-code (format nil "(~a~a~a)" (render-node renderer (nth 0 (node-reads node))) ,op (render-node renderer (nth 1 (node-reads node))))))))
  (def :ADD "+")
  (def :MUL "*")
  (def :IDIV "/")
  (def :AND " & ")
  (def :OR " | ")
  (def :XOR " ^ "))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :WMMA)) node)
  (format nil "~a+~a*~a" (render-node renderer (nth 0 (node-reads node))) (render-node renderer (nth 1 (node-reads node))) (render-node renderer (nth 2 (node-reads node)))))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a, ~a)" ,op (render-node renderer (nth 0 (node-reads node))) (render-node renderer (nth 1 (node-reads node)))))))
  (def :MAX "max"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a)" ,op (render-node renderer (nth 0 (node-reads node)))))))
  (def :NEG "-")
  (def :NOT "!")
  (def :SIN "sin")
  (def :log2 "log2")
  (def :exp2 "exp2")
  (def :RECIP "1/")
  (def :SQRT "sqrt"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-node renderer (nth 1 (node-reads node))) ,op (render-node renderer (nth 2 (node-reads node)))))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :Aref)) node)
  (render-aref renderer node))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :Allocate)) node))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :CAST)) node)
  (format nil "(~(~a~))~a" (->cdtype (getattr node :dtype)) (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :INDEX-COMPONENTS)) node)
  (render-expr 'CStyle-Renderer (expr-index-components (car (relay-read-iters (read-type-relay node))) (renderer-index-space renderer))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :WHERE)) node)
  (format nil "(~a ? ~a : ~a)"
          (render-node renderer (car (node-reads node)))
          (render-node renderer (second (node-reads node)))
          (render-node renderer (third (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) id node)
  (format nil "~a~a" (node-type node) (map 'list #'(lambda (x) (render-node renderer x)) (node-reads node))))
