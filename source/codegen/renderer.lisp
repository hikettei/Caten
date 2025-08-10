(defpackage :caten/codegen/renderer
  (:use :cl :caten/codegen/iteration :caten/aasm :caten/runtime/buffer :caten/air)
  (:import-from #:caten/codegen/byoc #:Renderer #:%render-node #:%render-const #:%render-kernel #:renderer-graph #:renderer-index-space)
  (:import-from #:caten/air #:node-type #:node-reads #:node-writes #:getattr #:id->value #:defnode #:make-node #:graph-nodes)
  (:import-from #:caten/aasm/expr #:Expr #:expr-graph #:expr-out #:expr-p #:expr-add #:expr-mul #:expr-const #:expr-scalar-equivalent-p #:expr-from-graph)
  (:import-from #:caten/codegen/helpers #:simplify-arithmetic-code #:->cdtype #:float-type-of)
  (:export
   ;; Renderers
   #:Default-Renderer
   #:CStyle-Renderer

   #:make-renderer
   #:render-expr
   #:render-aref
   #:render-node
   #:expr-index-components
   
   #:render-index
   #:render-aref-index))

(in-package :caten/codegen/renderer)

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

(defmethod %render-node ((renderer Default-Renderer) (id (eql :RANGE)) node)
  (%render-const renderer (getattr node :idx)))

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
  (def :MOD "%")
  (def :IDIV "/")
  (def :AND " and ")
  (def :OR " or ")
  (def :XOR " xor "))

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
  (let ((p (id->value (renderer-graph renderer) (car (node-reads node)))))
    (if (and p (eql (node-type p) :BIND))
        (format nil "~(~a~)[~(~a~)]" (getattr p :value) (render-node renderer (second (node-reads node))))
        (format nil "~(~a~)[~(~a~)]" (car (node-reads node)) (render-node renderer (second (node-reads node)))))))

(defmethod %render-node ((renderer default-renderer) (id (eql :Swizzle)) node)
  (with-output-to-string (out)
    (format out "~a" (render-node renderer (car (node-reads node))))
    (dolist (r (cdr (node-reads node)))
      (format out "[~a]" (render-node renderer r)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :BIND)) node)
  (format nil "~a" (%render-const renderer (getattr node :value))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :SETF)) node)
  (format nil "~a = ~a" (render-node renderer (car (node-reads node))) (render-node renderer (second (node-reads node)))))

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

(defmethod %render-node ((renderer Default-Renderer) (id (eql :EXPR)) node)
  (%render-const renderer (car (node-writes node))))

(defmethod %render-node ((renderer Default-Renderer) (id (eql :DEFINE-LOCAL)) node)
  (%render-const renderer (car (node-writes node))))

(defmethod %render-node ((renderer Default-Renderer) id node)
  (warn "Renderer: Unknown node type: ~a" (node-type node))
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
  (let ((dtype (tensor-relay-dtype (car (relay-reads (read-type-relay node))))))
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
  (format nil "(*(~a+~a))" (render-node renderer (car (node-reads node))) (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :MOVE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :STORE)) node)
  (format nil "~a" (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :Allocate)) node))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :CAST)) node)
  (format nil "(~(~a~))~a" (->cdtype (getattr node :dtype)) (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :WHERE)) node)
  (format nil "(~a ? ~a : ~a)"
          (render-node renderer (car (node-reads node)))
          (render-node renderer (second (node-reads node)))
          (render-node renderer (third (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :SETF)) node)
  (format nil "~a = ~a" (render-node renderer (car (node-reads node))) (render-node renderer (second (node-reads node)))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :BIND)) node)
  (format nil "~a" (%render-const renderer (getattr node :value))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :EXPR)) node)
  (%render-const renderer (car (node-writes node))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :DEFINE-GLOBAL)) node)
  (%render-const renderer (car (node-writes node))))

(defmethod %render-node ((renderer CStyle-Renderer) (id (eql :RANGE)) node)
  (%render-const renderer (getattr node :idx)))

(defmethod %render-node ((renderer CStyle-Renderer) id node)
  (if (next-method-p)
      (call-next-method)
      (format nil "[Error Rendering(Not Defined): ~a~a]" (node-type node) (map 'list #'(lambda (x) (render-node renderer x)) (node-reads node)))))
;; ~~ Common behaviours ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod %render-node ((renderer Renderer) (id (eql :EXPR)) node)
  (%render-const renderer (car (node-writes node))))

(defmethod %render-node ((renderer Renderer) (id (eql :DEFINE-GLOBAL)) node)
  (%render-const renderer (car (node-writes node))))

(defmethod %render-node ((renderer Renderer) (id (eql :DEFINE-LOCAL)) node)
  (%render-const renderer (car (node-writes node))))

(defmethod %render-node ((renderer Renderer) (id (eql :RANGE)) node)
  (%render-const renderer (getattr node :idx)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass JSONStyle-Renderer (Renderer)
  ((count :initform 0 :accessor jr-cnt)
   (vars :initform (make-hash-table) :accessor jr-vars)))

(defmethod jr-gensym ((jr JSONStyle-Renderer) val)
  (if (symbolp val)
      (or
       (gethash val (jr-vars jr))
       (prog1
           (setf (gethash val (jr-vars jr)) (format nil "{\"symbol\": \"v~a\"}" (jr-cnt jr)))
         (incf (jr-cnt jr))))
      (format nil "{\"const\": ~a}" val)))

(defmethod %render-const ((renderer JSONStyle-Renderer) obj) (jr-gensym renderer obj))
(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql :LOAD)) node) (%render-const renderer (getattr node :value)))
;; UnaryOps
(macrolet ((def (op &rest attrs)
             `(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql ,op)) node)
                (format nil "{\"op\": \"~a\", \"x\": ~a~a}"
                        ,op
                        (render-node renderer (car (node-reads node)))
                        (with-output-to-string (out)
                          (when ',attrs
                            (loop for attr in ',attrs do
                              (format nil ", \"~a\": ~a" attr (getattr node attr)))))))))
  (def :NEG)
  (def :NOT)
  (def :SIN)
  (def :log2)
  (def :exp2)
  (def :SQRT)
  (def :RECIP))
;; BinaryOps
(macrolet ((def (op &rest attrs)
             `(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql ,op)) node)
                (format nil "{\"op\": \"~a\", \"lhs\": ~a, \"rhs\": ~a~a}"
                        ,op
                        (render-node renderer (car (node-reads node)))
                        (render-node renderer (second (node-reads node)))
                        (with-output-to-string (out)
                          (when ',attrs
                            (loop for attr in ',attrs do
                              (format nil ", \"~a\": ~a" attr (getattr node attr)))))))))
  (def :ADD :wrap-around)
  (def :MUL :wrap-around)
  (def :MOD) (def :IDIV) (def :AND)
  (def :OR) (def :XOR)
  (def :MAX) (def :AREF) (def :CAST :dtype)
  (def :MOVE) (def :STORE) (def :SETF))
;; TernaryOps
(macrolet ((def (op &rest attrs)
             `(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql ,op)) node)
                (format nil "{\"op\": \"~a\", \"x\": ~a, \"y\": ~a, \"z\": ~a, ~a}"
                        ,op
                        (render-node renderer (car (node-reads node)))
                        (render-node renderer (second (node-reads node)))
                        (render-node renderer (third (node-reads node)))
                        (with-output-to-string (out)
                          (when ',attrs
                            (loop for attr in ',attrs do
                              (format nil ", \"~a\": ~a" attr (getattr node attr)))))))))
  (def :!=)
  (def :<)
  (def :WHERE))
(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql :Allocate)) node) "\"<empty>\"")

(macrolet ((def (op)
             `(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql ,op)) node)
                (%render-const renderer (car (node-writes node))))))
  (def :EXPR) (def :DEFINE-GLOBAL) (def :DEFINE-LOCAL))

(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql :RANGE)) node)
  (%render-const renderer (getattr node :idx)))

(defmethod %render-node ((renderer JSONStyle-Renderer) (id (eql :BIND)) node)
  (%render-const renderer (getattr node :value)))

(defun sha256-hex (string)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :sha256 (babel:string-to-octets string :encoding :utf-8))))

(defun make-kernel-description (graph &key (version) (getraw nil) &aux (seen))
  (let ((renderer (make-instance 'JSONStyle-Renderer :graph graph)))
    (funcall
     (if getraw #'identity #'sha256-hex)
     (with-output-to-string (out)
       (format out "{\"version\": ~a," version)
       (format out "\"globals\":[")
       (loop for node in (graph-nodes graph)
             if (eql (node-type node) :DEFINE-GLOBAL) do
               (format out "{\"arg\":\"~a\",\"dtype\":~a_~a_~a},"
                       (jr-gensym renderer (car (node-writes node)))
                       (if (getattr node :pointer-p) "*" "")
                       (getattr node :mode)
                       (getattr node :dtype)))
       (format out "{\"op\":\"end\"}],")
       (labels ((r (s &aux (val (id->value graph s)))
                  (when (and val (null (find (node-id val) seen)))
                    (f val) (push (node-id val) seen))
                  s)
                (e (id) (render-node renderer id))
                (emit-array (items emit-fn)
                  (format out "[")
                  (loop for it in items
                        for i from 0 do
                          (when (> i 0) (format out ","))
                          (funcall emit-fn it))
                  (format out "]"))
                (f (node)
                  (case (node-type node)
                    (:PROGN
                      (format out "{\"progn\":")
                      (emit-array (node-reads node) #'r)
                      (format out "}"))
                    (:EXPR
                     (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                         (format out "{\"expr_store\":~a}" (e (car (node-reads node))))
                         (let ((type (car (relay-writes (read-type-relay node)))))
                           (format out "{\"expr\":{\"id\":~a,\"sym\":~a,\"value\":"
                                   (->cdtype (tensor-relay-dtype type))
                                   (jr-gensym renderer (car (node-writes node))))
                           (format out "~a" (e (car (node-reads node))))
                           (format out "}}"))))
                    (:FOR
                     (multiple-value-bind (range body) (apply #'values (node-reads node))
                       (setf range (id->value graph range))
                       (assert (and range (eql (node-type range) :RANGE)) () "The first argument of :FOR should be :RANGE, getting ~a" range)
                       (multiple-value-bind (bind size step) (values (jr-gensym renderer (getattr range :idx)) (first (node-reads range)) (second (node-reads range)))
                         (when (symbolp size)
                           (let ((val (id->value graph size)))
                             (assert (and val (eql (node-type val) :EXPR)) () "Range: The size must be specified as EXPR or fixnum, getting ~a" val)
                             (setf size (car (node-reads val)))))
                         (when (symbolp step)
                           (let ((val (id->value graph step)))
                             (assert (and val (eql (node-type val) :EXPR)) () "Range: The step must be specified as EXPR or fixnum, getting ~a" val)
                             (setf step (car (node-reads val)))))
                         (format out "{\"for\":{\"idx\":\"~(~a~)\",\"lower\":0,\"upper\":" bind)
                         (format out "~a" (e size))
                         (format out ",\"step\":~a,\"body\":" (e step))
                         (r body)
                         (format out "}}"))))
                    (:IF
                     (multiple-value-bind (cond body) (apply #'values (node-reads node))
                       (setf cond (id->value graph cond))
                       (assert (and cond (eql (node-type cond) :EXPR)) () "IF: the conditon must be EXPR.")
                       (format out "{\"if\":{\"cond\":~a,\"then\":" (e (car (node-reads cond))))
                       (r body)
                       (format out "}}")))
                    (otherwise
                     (warn "JSONStyleRenderer: Unknown op type ~a" (node-type node))
                     (format out "{\"unknown_op\":~a}" (node-type node))))))
         (format out ",\"body\":")
         (f (id->value graph (car (graph-outputs graph))))
         (format out "}"))))))
