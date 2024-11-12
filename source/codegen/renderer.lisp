(defpackage :caten/codegen/renderer
  (:use :cl :caten/codegen/shape-inference)
  (:import-from #:caten/air #:node-type #:node-reads #:node-writes #:getattr #:id->value #:defnode #:make-node #:graph-nodes)
  (:import-from #:caten/codegen/expr #:Expr #:expr-graph #:expr-out #:expr-p #:expr-add #:expr-mul #:expr-const #:expr-scalar-equivalent-p #:expr-from-graph)
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
   #:make-define-global

   #:%renderer-get-auto-scheduler
   #:define-hook-auto-scheduler))

(in-package :caten/codegen/renderer)

(defgeneric get-default-renderer (id))
(defgeneric %render-node (renderer node-dispatcher node) (:documentation ""))
(defgeneric %render-const (renderer obj) (:documentation ""))

(defgeneric %render-kernel (renderer schedule-item))
(defgeneric %compile-kernel (renderer schedule-items dir))
(defgeneric %renderer-get-auto-scheduler (renderer) (:documentation "Gets the auto-scheduler for the renderer."))

(defmacro define-hook-auto-scheduler ((renderer-name auto-scheduler-name) &rest args)
  "Defines a hook for auto-scheduler for the renderer."
  `(defmethod %renderer-get-auto-scheduler ((renderer ,renderer-name))
     (,auto-scheduler-name ,@args)))

(defnode (:Render :FOR) ()
         "
```
for(int idx=upfrom, below, by)
```
"
         :slots ((idx :type symbol)
                 (upfrom :type Expr)
                 (below :type Expr)
                 (by :type Expr)))

(defnode (:Render :ENDFOR) ()
         "
```
} // idx
```"
         :slots ((idx :type symbol)))

(defnode (:Render :IF) ()
         "
```
if(condition)
```"
         :slots ((condition :type Expr)))

(defnode (:Render :ENDIF) ()
         "
```
} // endif
```
")

(defnode (:Render :Aref) ()
         ":AREF corresponds to the following code:
```
ID[*space]
```

The source ID is determined by the following rule:
- If the :storage-id is provided, ID is storage-id. (:storage-id is written by the memory-planner)
- If the :storage-id is nil, ID is (car (node-writes aref))

- storage-id[symbol or null] An index to the reference pointer optimized by the memory-planner.
- buffer[Buffer] The buffer to be accessed.
- space[Iteration-Space] The iteration space `:AREF` belongs to.
"
         :slots ((storage-id :type symbol)
                 (buffer :type Buffer)
                 (space :type Iteration-Space)))

(defnode (:Render :DEFINE-GLOBAL) ()
         "
The node :DEFINE-GLOBAL declares a global variable in the kernel. (it corresponds to the argument of the kernel.)
"
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
  (make-node :Render :Aref (list name) nil :buffer buffer :space space :storage-id name))

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

(defun render-child (renderer self nth)
  (declare (type Renderer renderer))
  (let ((child (id->value (renderer-graph renderer) (nth nth (node-reads self)))))
    (if (and child (eql (node-type child) :Aref))
        (let ((scalar-p (= -1 (buffer-nrank (getattr child :buffer)))))
          (render-aref renderer child
                       :scalar-p scalar-p
                       :buffer (nth nth (relay-reads (read-type-relay self)))
                       :space (nth nth (relay-read-iters (read-type-relay self)))))
        (render-node renderer (nth nth (node-reads self))))))

(defun render-aref (renderer node &key (scalar-p) (buffer) (space))
  (assert (eql (node-type node) :AREF))
  (let ((buffer (or buffer (getattr node :buffer)))
        (space  (or space (getattr node :space)))
        (index-space (renderer-index-space renderer))
        (id (or (getattr node :storage-id) (car (node-writes node)))))
    (when (and (null index-space) (> (buffer-nrank buffer) 0))
      (warn "render-aref: Cannot render :AREF for ~a without providing :index-space, thus replaced with ?." id))
    (if (or scalar-p (= -1 (buffer-nrank buffer)))
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

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (let ((lhs (render-child renderer node 0))
                      (rhs (render-child renderer node 1)))
                  (simplify-arithmetic-code (format nil "(~a~a~a)" lhs ,op rhs))))))
  (def :ADD "+")
  (def :MUL "*")
  (def :AND " and ")
  (def :OR " or ")
  (def :XOR " xor "))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :WMMA)) node)
  (format nil "~a+~a*~a" (render-child renderer node 0) (render-child renderer node 1) (render-child renderer node 2)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a, ~a)" ,op (render-child renderer node 0) (render-child renderer node 1)))))
  (def :MAX "max"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a)" ,op (render-child renderer node 0)))))
  (def :NEG "-")
  (def :NOT "!")
  (def :SIN "sin")
  (def :log2 "log2")
  (def :exp2 "exp2")
  (def :RECIP "1/")
  (def :SQRT "sqrt"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer Default-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-child renderer node 1) ,op (render-child renderer node 2)))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Aref)) node)
  (render-aref renderer node))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-child renderer node 1)))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :STORE)) node)
  (format nil "~a" (render-child renderer node 1)))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :Allocate)) node) (format nil "0"))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :CAST)) node)
  (format nil "(~(~a~))~a" (getattr node :dtype) (render-child renderer node 1)))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :INDEX-COMPONENTS)) node)
  (render-expr 'Default-Renderer (expr-index-components renderer node (renderer-index-space renderer))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :WHERE)) node)
  (format nil "~a ? ~a : ~a"
          (render-child renderer node 0)
          (render-child renderer node 1)
          (render-child renderer node 2)))

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
     ;; [TODO: Fix] (hikettei) This code is not ANSI-Portable because the way the sytem rendering double-float differs. This is why FP64+CCL fails.
     (if (typep obj 'double-float)
         (progn
           #-sbcl(progn (warn "FP64 Math w/o SBCL is not tested."))
           #+sbcl(cl-ppcre:regex-replace "d" (format nil "~a" obj) "e"))
         (format nil "~(~a~)" obj)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :LOAD)) node)
  (%render-const renderer (getattr node :value)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (simplify-arithmetic-code (format nil "(~a~a~a)" (render-child renderer node 0), op (render-child renderer node 1))))))
  (def :ADD "+")
  (def :MUL "*")
  (def :IDIV "/")
  (def :AND " & ")
  (def :OR " | ")
  (def :XOR " ^ "))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :WMMA)) node)
  (format nil "~a+~a*~a" (render-child renderer node 0) (render-child renderer node 1) (render-child renderer node 2)))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a, ~a)" ,op (render-child renderer node 0) (render-child renderer node 1)))))
  (def :MAX "max"))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "~a(~a)" ,op (render-child renderer node 0)))))
  (def :NEG "-")
  (def :NOT "!")
  (def :SIN "sin")
  (def :log2 "log2")
  (def :exp2 "exp2")
  (def :SQRT "sqrt"))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :RECIP)) node)
  (let ((dtype (caten/avm:buffer-dtype (car (relay-writes (read-type-relay node))))))
    (if (caten/common.dtype:dtype/floatp dtype)
        (format nil "1.0/(~a)" (render-child renderer node 0))
        (format nil "1/(~a)" (render-child renderer node 0)))))

(macrolet ((def (id op)
             `(defmethod %render-node ((renderer CStyle-Renderer) (id (eql ,id)) node)
                (format nil "(~a~a~a)" (render-child renderer node 1) ,op (render-child renderer node 2)))))
  (def :!= "!=")
  (def :< "<"))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :Aref)) node)
  (render-aref renderer node))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-child renderer node 1)))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :STORE)) node)
  (format nil "~a" (render-child renderer node 1)))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :Allocate)) node))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :CAST)) node)
  (format nil "(~(~a~))~a" (->cdtype (getattr node :dtype)) (render-child renderer node 1)))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :INDEX-COMPONENTS)) node)
  (render-expr 'CStyle-Renderer (expr-index-components renderer node (renderer-index-space renderer))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :WHERE)) node)
  (format nil "(~a ? ~a : ~a)"
          (render-child renderer node 0)
          (render-child renderer node 1)
          (render-child renderer node 2)))

(defmethod %render-node ((renderer CStyle-Renderer) id node)
  (format nil "~a~a" (node-type node) (map 'list #'(lambda (x) (render-node renderer x)) (node-reads node))))
