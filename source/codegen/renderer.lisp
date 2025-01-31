(defpackage :caten/codegen/renderer
  (:use :cl :caten/codegen/type-relay :caten/runtime/buffer)
  (:import-from #:caten/air #:node-type #:node-reads #:node-writes #:getattr #:id->value #:defnode #:make-node #:graph-nodes)
  (:import-from #:caten/codegen/expr #:Expr #:expr-graph #:expr-out #:expr-p #:expr-add #:expr-mul #:expr-const #:expr-scalar-equivalent-p #:expr-from-graph)
  (:import-from #:caten/codegen/helpers #:simplify-arithmetic-code #:->cdtype #:float-type-of)
  (:export
   #:get-default-renderer
   #:%render-kernel #:%compile-kernel
   ;; Renderers
   #:Renderer
   #:Default-Renderer
   #:CStyle-Renderer
   ;; Utils
   #:renderer-graph
   #:renderer-index-space
   #:make-renderer
   #:render-expr
   #:render-aref
   #:render-node
   #:%render-node
   #:%render-const
   #:expr-index-components
   #:%renderer-get-auto-scheduler
   #:render-index
   #:render-aref-index))

(in-package :caten/codegen/renderer)

(defgeneric get-default-renderer (id))
(defgeneric %render-node (renderer node-dispatcher node) (:documentation ""))
(defgeneric %render-const (renderer obj) (:documentation ""))
(defgeneric %render-kernel (renderer schedule-item))
(defgeneric %compile-kernel (renderer schedule-items dir))

(defclass Renderer ()
  ((graph :initarg :graph :accessor renderer-graph)
   (index-space :initarg :index-space :type list :initform nil :accessor renderer-index-space))
  (:documentation "TODO"))

(defun make-renderer (renderer-name graph index-space &rest initargs)
  (assert (every #'expr-p index-space) () "index-space is a list of exprs!")
  (apply #'make-instance renderer-name :graph graph :index-space index-space initargs))

(defun render-expr (renderer-id expr &key (index-space) (initargs))
  (declare (type expr expr))
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

(defun render-aref-index (renderer node)
  (assert (eql (node-type node) :AREF))
  (let ((buffer (getattr node :buffer))
        (space  (getattr node :space))
        (index-space (renderer-index-space renderer))
        (id (getattr node :storage-id)))
    (when (and (null index-space) (> (caten/runtime:buffer-nrank buffer) 0))
      (warn "render-aref: Cannot render :AREF for ~a without providing :index-space, thus replaced with ?." id))
    (if (= -1 (buffer-nrank buffer))
        nil
        (if index-space
            (let ((expr (apply #'expr-add (iteration-space-expr-aref space buffer (renderer-index-space renderer)))))
              (setf (graph-nodes (renderer-graph renderer))
                    (append
                     (graph-nodes (expr-graph expr))
                     (graph-nodes (renderer-graph renderer))))
              (render-node renderer (car (node-writes (expr-out expr)))))
            (error "render-aref-index: Cannot render the node ~a without providing proper index-space." id)))))

(defun render-aref (renderer node)
  (assert (eql (node-type node) :AREF))
  (let ((buffer (getattr node :buffer))
        (space  (getattr node :space))
        (index-space (renderer-index-space renderer))
        (id (getattr node :storage-id)))
    (when (and (null index-space) (> (caten/runtime:buffer-nrank buffer) 0))
      (warn "render-aref: Cannot render :AREF for ~a without providing :index-space, thus replaced with ?." id))
    (if (= -1 (buffer-nrank buffer))
        (format nil "~(~a~)" id)
        (if index-space
            (let ((expr (apply #'expr-add (iteration-space-expr-aref space buffer (renderer-index-space renderer)))))
              (setf (graph-nodes (renderer-graph renderer))
                    (append
                     (graph-nodes (expr-graph expr))
                     (graph-nodes (renderer-graph renderer))))
              (format nil "~(~a~)[~a]"
                      (%render-const renderer id)
                      (render-node renderer (car (node-writes (expr-out expr))))))
            (format nil "~(~a~)[?]" id)))))

(defun render-index (renderer bp &key (nth 0))
  "Returns the renderer object for the nth write of the given bp. If that were scalar, returns nil."
  (let ((iterations (getattr bp :iterations))
        (is (nth nth (relay-write-iters (read-type-relay bp))))
        (b (nth nth (relay-writes (read-type-relay bp)))))
    (when (and is (not (= -1 (buffer-nrank b))) (> (length (iteration-space-shape is)) 0) (> (length iterations) 0))
      (render-expr
       renderer
       (apply
        #'expr-add
        (map
         'list
         #'(lambda (view stride i)
             (if view
                 (expr-mul stride (expr-add (expr-const (car view) :int64) (expr-mul (expr-const (third view) :int64) i)))
                 (expr-mul stride i)))
         (iteration-space-views is)
         (iteration-space-strides is)
         iterations))))))

(defun expr-index-components (renderer node index-space)
  (assert (eql (node-type node) :INDEX-COMPONENTS))
  (labels ((from-expr (shapes components)
             (reduce
              #'expr-add
              (map
               'list
               #'(lambda (size stride gid)
                   (if (expr-scalar-equivalent-p size (expr-const 1 :int64))
                       (expr-const 0 :int64)
                       (expr-mul (expr-const stride :int64) (expr-const gid :int64))))
               shapes
               components
               index-space)))
           (%expr-from-graph (id graph)
             (assert id)
             (if (symbolp id)
                 (expr-from-graph id graph)
                 (expr-const id :int64)))
           (merge-stride (proc list)
             (loop for p in proc
                   collect
                   (let ((strides (map 'list #'(lambda (x) (nth x list)) p)))
                     (%expr-from-graph (if (find 0 strides :test #'eql) 0 (car (last strides))) (renderer-graph renderer))))))
    (let* ((is (car (relay-write-iters (read-type-relay node))))
           (proc (iteration-space-procedure is))
           (components (merge-stride proc (cdr (node-reads node)))))
      (from-expr (iteration-space-shape is) components))))
;; ~~ Default Renderer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass Default-Renderer (Renderer)
  nil
  (:documentation "Default Renderer used to print-object in repl"))

(defmethod %render-const ((renderer Default-Renderer) obj)
  (format nil "~(~a~)" obj))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :LOAD)) node)
  (%render-const renderer (getattr node :value)))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :SPACE)) node)
  (let ((lv (ecase (getattr node :level) (:block "blockIdx") (:thread "threadIdx")))
        (dim (ecase (getattr node :rank) (0 "x") (1 "y") (2 "z"))))
    (format nil "~a.~a" lv dim)))

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
  (format nil "~a+~a*~a"
	  (render-node renderer (nth 0 (node-reads node)))
	  (render-node renderer (nth 1 (node-reads node)))
	  (render-node renderer (nth 2 (node-reads node)))))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a, ~a)"
			,op
			(render-node renderer (nth 0 (node-reads node)))
			(render-node renderer (nth 1 (node-reads node)))))))
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
                (format nil "(~a~a~a)"
			(render-node renderer (nth 1 (node-reads node)))
			,op
			(render-node renderer (nth 2 (node-reads node)))))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Aref)) node)
  (render-aref renderer node))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :STORE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Allocate)) node) (format nil "0"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :CAST)) node)
  (format nil "(~(~a~))~a" (getattr node :dtype) (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :INDEX-COMPONENTS)) node)
  (render-expr 'Default-Renderer (expr-index-components renderer node (renderer-index-space renderer))))

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
    (otherwise
     (if (typep obj 'double-float)
         (format nil "~,15f" obj)
         (format nil "~(~a~)" obj)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :LOAD)) node)
  (%render-const renderer (getattr node :value)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (simplify-arithmetic-code (format nil "(~a~a~a)"
						  (render-node renderer (nth 0 (node-reads node)))
						  ,op
						  (render-node renderer (nth 1 (node-reads node))))))))
  (def :ADD "+")
  (def :MUL "*")
  (def :MOD "%")
  (def :IDIV "/")
  (def :AND " & ")
  (def :OR " | ")
  (def :XOR " ^ "))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :WMMA)) node)
  (format nil "~a+~a*~a"
	  (render-node renderer (nth 0 (node-reads node)))
	  (render-node renderer (nth 1 (node-reads node)))
	  (render-node renderer (nth 2 (node-reads node)))))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a, ~a)"
			,op
			(render-node renderer (nth 0 (node-reads node)))
			(render-node renderer (nth 1 (node-reads node)))))))
  (def :MAX "max"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a)" ,op (render-node renderer (nth 0 (node-reads node)))))))
  (def :NEG "-")
  (def :NOT "!")
  (def :SIN "sin")
  (def :log2 "log2")
  (def :exp2 "exp2")
  (def :SQRT "sqrt"))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :RECIP)) node)
  (let ((dtype (caten/runtime:buffer-dtype (car (relay-writes (read-type-relay node))))))
    (if (caten/common.dtype:dtype/floatp dtype)
        (format nil "1.0/(~a)" (render-node renderer (nth 0 (node-reads node))))
        (format nil "1/(~a)" (render-node renderer (nth 0 (node-reads node)))))))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)"
			(render-node renderer (nth 1 (node-reads node)))
			,op
			(render-node renderer (nth 2 (node-reads node)))))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :Aref)) node)
  (render-aref renderer node))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :STORE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :Allocate)) node))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :CAST)) node)
  (format nil "(~(~a~))~a" (->cdtype (getattr node :dtype)) (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :INDEX-COMPONENTS)) node)
  (render-expr 'CStyle-Renderer (expr-index-components renderer node (renderer-index-space renderer))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :WHERE)) node)
  (format nil "(~a ? ~a : ~a)"
          (render-node renderer (car (node-reads node)))
          (render-node renderer (second (node-reads node)))
          (render-node renderer (third (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) id node)
  (format nil "~a~a" (node-type node) (map 'list #'(lambda (x) (render-node renderer x)) (node-reads node))))
