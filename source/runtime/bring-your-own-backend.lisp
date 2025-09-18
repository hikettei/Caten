(defpackage :caten/runtime/bring-your-own-backend
  (:documentation "Provides a useful macro for defining a new accelerator")
  (:use :cl :caten/graph)
  (:nicknames :caten/runtime/byoc)
  (:export
   #:define-runtime
   #:define-buffer
   #:define-renderer
   #:define-kernel
   #:define-backend
   #:render
   #:const
   #:render-kernel
   #:get-renderer

   #:Default-Renderer
   #:Default-Kernel
   #:JSONStyle-Renderer
   #:JSONStyle-Kernel
   #:jr-gensym
   #:print-blueprint))

(in-package :caten/runtime/bring-your-own-backend)

(defmacro define-backend (backend-id &key runtime buffer renderer kernel)
  `(progn
     (defmethod caten/runtime/runtime:backend-get-runtime-cls ((id (eql ,backend-id))) ',runtime)
     (defmethod caten/runtime/buffer:backend-get-buffer-cls ((id (eql ,backend-id))) ',buffer)
     (defmethod caten/runtime/renderer:backend-get-renderer-cls ((id (eql ,backend-id))) ',renderer)
     (defmethod caten/runtime/kernel:backend-get-kernel-cls ((id (eql ,backend-id))) ',kernel)))

(defmacro define-runtime (runtime-name direct-superclasses direct-slots &key (open) (close))
  (flet ((ensure-lambda (n form name)
           (assert (listp (car form)))
           (assert (= n (length (car form))) () "the argument ~a excepts ~a arguments, getting ~a" n name form)
           (apply
            #'values
            (append
             (loop for i upfrom 0 below n
                   collect (nth i (car form)))
             (list
              (if (= 2 (length form))
                  (cdr form)
                  `(progn ,@(cdr form))))))))
    `(progn
       (defclass ,runtime-name (,@direct-superclasses caten/ir:RuntimeGraph)
         ,direct-slots)
       ,(multiple-value-bind (runtime-bind form) (ensure-lambda 1 open "open")
          `(defmethod caten/runtime/runtime:open-runtime ((,runtime-bind ,runtime-name)) ,@form))
       ,(multiple-value-bind (runtime-bind form) (ensure-lambda 1 close "close")
          `(defmethod caten/runtime/runtime:close-runtime ((,runtime-bind ,runtime-name)) ,@form)))))
     
(defmacro define-buffer ((buffer-name runtime-name) direct-superclasses direct-slots
                         &key (open-buffer) (close-buffer) (transfer-from-array) (transfer-into-array) (bref))
  (flet ((ensure-lambda (n form name)
           (assert (listp (car form)))
           (assert (= n (length (car form))) () "the argument ~a excepts ~a arguments, getting ~a" n name form)
           (apply
            #'values
            (append
             (loop for i upfrom 0 below n
                   collect (nth i (car form)))
             (list
              (if (= 2 (length form))
                  (cdr form)
                  `(progn ,@(cdr form))))))))
    `(progn
       (defclass ,buffer-name (,@direct-superclasses caten/runtime/buffer:AbstractBuffer)
         ,direct-slots)
       ,(multiple-value-bind (runtime buffer form) (ensure-lambda 2 open-buffer "open_buffer")
          `(defmethod caten/runtime/buffer:open-buffer ((,runtime ,runtime-name) (,buffer ,buffer-name))
             ,@form))
       ,(multiple-value-bind (runtime buffer form) (ensure-lambda 2 close-buffer "close_buffer")
          `(defmethod caten/runtime/buffer:close-buffer ((,runtime ,runtime-name) (,buffer ,buffer-name))
             ,@form))
       ,(multiple-value-bind (runtime buffer array form) (ensure-lambda 3 transfer-from-array "transfer_from_array")
          `(defmethod caten/runtime/buffer:transfer-from-array ((,runtime ,runtime-name) (,buffer ,buffer-name) ,array)
             ,@form))
       ,(multiple-value-bind (buffer form) (ensure-lambda 1 transfer-into-array "transfer_into_array")
          `(defmethod caten/runtime/buffer:transfer-into-array ((,buffer ,buffer-name))
             ,@form))
       ,(multiple-value-bind (buffer index form) (ensure-lambda 2 bref "bref")
          `(defmethod caten/runtime/buffer:bref ((,buffer ,buffer-name) ,index) ,@form)))))

(defun render (x) (declare (ignore x)) (error "(render id) is only binded by define-renderer"))
(defun const (x type) (declare (ignore x type)) (error "(const id type) is only binded by define-renderer"))
(defun render-kernel (x) (declare (ignore x)) (error "(render-kernel x) is only binded by define-kernel"))
(defun get-renderer () (error "(get-renderer) is only binded by define-renderer/define-kernel."))
(defmacro define-renderer (renderer-name direct-superclasses direct-slots &rest patterns)
  (alexandria:with-gensyms (renderer node)
    `(prog1
         (defclass ,renderer-name (,@direct-superclasses caten/runtime/renderer:Renderer) ,direct-slots)
       ,@(loop for pattern in patterns
               do (assert (and (listp pattern) (keywordp (car pattern))) () "define-renderer: pattern := `(,node_id ,@(pattern_match_rules))")
               collect
               `(defmethod caten/runtime/renderer:%render-node ((,renderer ,renderer-name) (node-id (eql ,(car pattern))) ,node)
                  (flet ((render (id) (caten/runtime/renderer:render-node ,renderer id))
                         (get-renderer () ,renderer)
                         (const (id dtype) (caten/runtime/renderer:%render-const ,renderer id dtype)))
                    #'render #'get-renderer #'const ;; to supress defined but not used warnings
                    (caten/graph:node-ematch (,node :extra-graph (caten/runtime/renderer::renderer-graph ,renderer)) ,@(cdr pattern))))))))

(defmacro define-kernel ((kernel-name renderer-name) direct-superclasses direct-slots &key (launch) (compile) (specs))
  (flet ((ensure-lambda (n form name)
           (assert (listp (car form)))
           (assert (>= (length (car form)) n) () "the argument ~a expects at least ~a arguments, getting ~a" name n form)
           (apply
            #'values
            (append
             (loop for i upfrom 0 below n collect (nth i (car form)))
             (list (if (= 2 (length form)) (cdr form) `(progn ,@(cdr form))))))))
    (alexandria:with-gensyms (renderer node)
      `(progn
         (defclass ,kernel-name (,@direct-superclasses caten/runtime/kernel:Kernel) ,direct-slots)
         (defmethod caten/runtime/kernel:kernel-load-blueprint ((kernel ,kernel-name) (blueprint caten/ir:ASTGraph))
           (let ((renderer (make-instance ',renderer-name :graph blueprint)))
             ;; Derive argument names and types from :DEFINE-GLOBAL nodes
             (let* ((globals (remove-if-not #'(lambda (n) (eql (node-type n) :DEFINE-GLOBAL)) (graph-nodes blueprint)))
                    (argnames (map 'list #'(lambda (n) (getattr n :name)) globals))
                    (argtypes (map 'list #'(lambda (n) (cons (getattr n :dtype) (getattr n :pointer-p))) globals))
                    (program (caten/runtime/kernel:render-kernel renderer blueprint)))
               (setf (caten/runtime/kernel:kernel-args kernel) argnames
                     (caten/runtime/kernel:kernel-argtypes kernel) argtypes)
               (caten/runtime/kernel:%kernel-write-program program kernel))))
         ,(when compile
            (multiple-value-bind (kernel-bind runtime-bind form) (ensure-lambda 2 compile "compile")
              `(defmethod caten/runtime/kernel:kernel-compile ((,kernel-bind ,kernel-name) ,runtime-bind)
                 ,@form)))
         ,(when launch
            (multiple-value-bind (kernel-bind runtime-bind form) (ensure-lambda 2 launch "launch")
              `(defmethod caten/runtime/kernel:kernel-launch ((,kernel-bind ,kernel-name) ,runtime-bind &rest args)
                 (declare (ignorable args))
                 ,@form)))
         ,@(loop for pattern in specs
                 do (assert (and (listp pattern) (keywordp (car pattern))) () "define-kernel: spec := `(,op_id ,@(pattern_match_rules))")
                 collect
                 `(defmethod caten/runtime/kernel:%render-kernel-op ((,renderer ,renderer-name) (op-id (eql ,(car pattern))) ,node)
                    (flet ((render (id) (caten/runtime/renderer:render-node ,renderer id))
                           (const (id dtype) (caten/runtime/renderer:%render-const ,renderer id dtype))
                           (get-renderer () ,renderer)
                           (render-kernel (id) (caten/runtime/kernel:render-kernel-node ,renderer id)))
                      #'render #'const #'get-renderer #'render-kernel ;; to supress defined but not used warnings
                      (caten/graph:node-ematch (,node :extra-graph (caten/runtime/renderer::renderer-graph ,renderer)) ,@(cdr pattern)))))))))
;; ~~ Default Renderers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-renderer Default-Renderer () nil
  (:LOAD ((:LOAD (_) :value x) -> ((node graph) (const x (caten/ir:tensor-relay-dtype (car (relay-writes (read-type-relay node))))))))
  (:RANGE ((:RANGE (~ _) :idx idx :dtype dtype) -> (const idx dtype)))
  (:ADD ((:ADD (lhs rhs)) -> (format nil "~a+~a" (render lhs) (render rhs))))
  (:MUL ((:MUL (lhs rhs)) -> (format nil "~a*~a" (render lhs) (render rhs))))
  (:MOD ((:MOD (lhs rhs)) -> (format nil "~a%~a" (render lhs) (render rhs))))
  (:IDIV ((:IDIV (lhs rhs)) -> (format nil "~a/~a" (render lhs) (render rhs))))
  (:AND ((:AND (lhs rhs)) -> (format nil "~a and ~a" (render lhs) (render rhs))))
  (:OR ((:OR (lhs rhs)) -> (format nil "~a or ~a" (render lhs) (render rhs))))
  (:XOR ((:XOR (lhs rhs)) -> (format nil "~a xor ~a" (render lhs) (render rhs))))
  (:MAX ((:MAX  (lhs rhs)) -> (format nil "max(~a, ~a)" (render lhs) (render rhs))))
  (:NEG ((:NEG (x)) -> (format nil "-(~a)" (render x))))
  (:NOT ((:NOT (x)) -> (format nil "!(~a)" (render x))))
  (:SIN ((:SIN (x)) -> (format nil "sin(~a)" (render x))))
  (:LOG2 ((:LOG2 (x)) -> (format nil "log2(~a)" (render x))))
  (:EXP2 ((:EXP2 (x)) -> (format nil "exp2(~a)" (render x))))
  (:RECIP ((:RECIP (x)) -> (format nil "1/(~a)" (render x))))
  (:SQRT ((:SQRT (x)) -> (format nil "sqrt(~a)" (render x))))
  (:!= ((:!= (_ x y)) -> (format nil "(~a!=~a)" (render x) (render y))))
  (:< ((:< (_ x y)) -> (format nil "(~a<~a)" (render x) (render y))))
  (:DEFINE-GLOBAL ((:DEFINE-GLOBAL () :name name :dtype dtype) -> ((node graph) (const name dtype))))
  (:AREF ((:AREF (name idx)) -> (format nil "~a[~a]" (render name) (render idx))))
  ;; (:PolyAref ((:PolyAref (name idx)) -> (format nil "~a[~a]" (render name) (render idx))))
  (:MOVE ((:MOVE (_ y)) -> (render y)))
  (:BIND ((:BIND (_) :value x) -> (const x :float32))) ;; [TODO] Determine the type of BIND
  (:SETF ((:SETF (x y)) -> (format nil "~a = ~a" (render x) (render y))))
  (:CAST ((:CAST (_ y) :dtype dtype) -> (format nil "(~(~a~))~a" dtype (render y))))
  (:WHERE ((:WHERE (x y z)) -> (format nil "~a ? ~a : ~a" (render x) (render y) (render z))))
  (:EXPR ((:EXPR (x)) -> ((node graph) (const x (caten/ir:tensor-relay-dtype (car (relay-writes (read-type-relay (id->value graph x)))))))))
  )

(defmethod caten/runtime/renderer:%render-const ((renderer Default-Renderer) obj dtype)
  (case (if (numberp obj)
            (uiop:symbol-call :caten/api :float-type-of obj)
            t)
    (:inf "_infinity")
    (:-inf "_negative_infinity")
    (:nan "_nan")
    (otherwise
     (if (eql dtype :float64)
         (format nil "~,15f" obj)
         (format nil "~(~a~)" obj)))))

;; Indentation control for blueprint printers
(defparameter *indent* 0)
;; Default C-style Kernel printer using Default-Renderer for expressions
(defun indent () (make-string *indent* :initial-element #\Space))
(define-kernel
    (Default-Kernel Default-Renderer) () nil
    :specs
    ((:FUNCTION ((:FUNCTION (body) :name fname)
                 ->
                 ((node graph)
                  (let* ((globals (loop for node in (graph-nodes graph)
                                        if (eql (node-type node) :DEFINE-GLOBAL)
                                          collect node))
                         (params (with-output-to-string (o)
                                   (loop for g in globals
                                         for dtype = (getattr g :dtype)
                                         for ptrp  = (getattr g :pointer-p)
                                         for mode  = (getattr g :mode)
                                         for name  = (getattr g :name)
                                         for i upfrom 0 do
                                           (format o "~a~a~a ~(~a~)~a"
                                                   (if (eql mode :read) "const " "")
                                                   dtype
                                                   (if ptrp "*" "")
                                                   name
                                                   (if (< i (1- (length globals))) ", " "")))))
                         (head (indent))
                         (body-str (render-kernel body)))
                    (format nil "~avoid ~(~a~)(~a)~%~a" head fname params body-str)))))
     (:PROGN ((:PROGN (~ _))
              ->
              ((node graph)
               (let ((head (indent)) (*indent* (+ 2 *indent*)))
                 (let ((body (map 'list #'render-kernel (node-reads node))))
                   (format nil "~a{~%~{~a~^~%~}~%~a}" head body head))))))
     (:IF ((:IF ((:EXPR (cond)) body))
           ->
           ((node graph)
            (let ((head (indent)) (*indent* (+ 2 *indent*)))
              (let* ((cond-str (render cond)) (body (render-kernel body)))
                (format nil "~aif (~a)~%~a" head cond-str body))))))
     (:FOR ((:FOR ((:RANGE ((:EXPR (size)) (:EXPR (step))) :dtype dtype :idx idx) body))
            ->
            ((node graph)
             (let ((head (indent)) (*indent* (+ 2 *indent*)))
               (let* ((size (render size)) (step (render step))
                      (body (render-kernel body)))
                 (format nil "~afor (~(~a~) ~(~a~)=0; ~(~a~)<~a; ~(~a~)+=~a)~%~a"
                         head dtype idx idx size idx step body))))))
    (:EXPR ((:EXPR ((:SETF (_ _))))
             ->
             ((node graph)
              (format nil "~a~a; // EXPR(STORE) {ID: ~a}" (indent) (render (car (node-reads node))) (node-id node))))
            ((:EXPR (_))
             ->
             ((node graph)
              (format nil "~a~(~a~) ~(~a~) = ~a; // expr {ID: ~a}"
                      (indent) (caten/ir:tensor-relay-dtype (car (relay-writes (read-type-relay node))))
                      (car (node-writes node))
                      (render (car (node-reads node)))
                      (node-id node))))))
    :compile nil :launch nil)

;; JSONStyle-Renderer (moved from old/source/codegen/renderer.lisp)
(define-renderer JSONStyle-Renderer ()
  ((count :initform 0 :accessor jr-cnt)
   (vars  :initform (make-hash-table) :accessor jr-vars))
  ;; Load constant
  (:LOAD ((:LOAD (_) :value x)
          ->
          ((node graph)
           (const x :float32))))
  ;; Unary
  (:NEG  ((:NEG  (x)) -> (format nil "{\"op\": \"neg\",  \"x\": ~a}" (render x))))
  (:NOT  ((:NOT  (x)) -> (format nil "{\"op\": \"not\",  \"x\": ~a}" (render x))))
  (:SIN  ((:SIN  (x)) -> (format nil "{\"op\": \"sin\",  \"x\": ~a}" (render x))))
  (:LOG2 ((:LOG2 (x)) -> (format nil "{\"op\": \"log2\", \"x\": ~a}" (render x))))
  (:EXP2 ((:EXP2 (x)) -> (format nil "{\"op\": \"exp2\", \"x\": ~a}" (render x))))
  (:SQRT ((:SQRT (x)) -> (format nil "{\"op\": \"sqrt\", \"x\": ~a}" (render x))))
  (:RECIP((:RECIP(x)) -> (format nil "{\"op\": \"recip\",\"x\": ~a}" (render x))))
  ;; Binary
  (:ADD ((:ADD (lhs rhs)) -> ((node graph) (format nil "{\"op\": \"add\", \"lhs\": ~a, \"rhs\": ~a, \"wrap-around\": ~a}"
                                     (render lhs) (render rhs) (getattr node :wrap-around :allow-undefined t)))))
  (:MUL ((:MUL (lhs rhs)) -> ((node graph) (format nil "{\"op\": \"mul\", \"lhs\": ~a, \"rhs\": ~a, \"wrap-around\": ~a}"
                                     (render lhs) (render rhs) (getattr node :wrap-around :allow-undefined t)))))
  (:MOD ((:MOD (lhs rhs)) -> ((node graph) (format nil "{\"op\": \"mod\", \"lhs\": ~a, \"rhs\": ~a}" (render lhs) (render rhs)))))
  (:IDIV ((:IDIV (lhs rhs)) -> (format nil "{\"op\": \"idiv\", \"lhs\": ~a, \"rhs\": ~a}" (render lhs) (render rhs))))
  (:AND ((:AND (lhs rhs)) -> (format nil "{\"op\": \"and\", \"lhs\": ~a, \"rhs\": ~a}" (render lhs) (render rhs))))
  (:OR  ((:OR  (lhs rhs)) -> (format nil "{\"op\": \"or\",  \"lhs\": ~a, \"rhs\": ~a}" (render lhs) (render rhs))))
  (:XOR ((:XOR (lhs rhs)) -> (format nil "{\"op\": \"xor\", \"lhs\": ~a, \"rhs\": ~a}" (render lhs) (render rhs))))
  (:MAX ((:MAX (lhs rhs)) -> (format nil "{\"op\": \"max\", \"lhs\": ~a, \"rhs\": ~a}" (render lhs) (render rhs))))
  (:AREF ((:AREF (name idx)) -> (format nil "{\"op\": \"aref\", \"lhs\": ~a, \"rhs\": ~a}" (render name) (render idx))))
  (:CAST ((:CAST (_ y) :dtype dtype) -> (format nil "{\"op\": \"cast\", \"x\": ~a, \"dtype\": \"~(~a~)\"}" (render y) dtype)))
  (:MOVE ((:MOVE (_ y)) -> (format nil "{\"op\": \"move\", \"x\": ~a}" (render y))))
  (:SETF ((:SETF (x y)) -> (format nil "{\"op\": \"setf\", \"lhs\": ~a, \"rhs\": ~a}" (render x) (render y))))
  ;; Ternary
  (:!= ((:!= (_ x y)) -> (format nil "{\"op\": \"!=\", \"x\": ~a, \"y\": ~a}" (render x) (render y))))
  (:<  ((:<  (_ x y)) -> (format nil "{\"op\": \"<\",  \"x\": ~a, \"y\": ~a}" (render x) (render y))))
  (:WHERE ((:WHERE (x y z)) -> (format nil "{\"op\": \"where\", \"x\": ~a, \"y\": ~a, \"z\": ~a}" (render x) (render y) (render z))))
  ;; Others
  (:Allocate ((:Allocate (_)) -> "\"<empty>\""))
  (:EXPR ((:EXPR (x)) -> ((node graph) (const (car (node-writes node)) :float32))))
  (:DEFINE-GLOBAL ((:DEFINE-GLOBAL () :name name) -> (const name :float32)))
  (:DEFINE-LOCAL  ((:DEFINE-LOCAL  (_)) -> ((node graph) (const (car (node-writes node)) :float32))))
  (:RANGE ((:RANGE (~ _) :idx idx) -> (const idx :int64)))
  (:BIND  ((:BIND  (_) :value v) -> (const v :float32)))
  (:PolyAref
   ((:PolyAref (list* arr args))
    ->
    ((node graph)
     (let ((args (map 'list #'render (cdr (node-reads node)))))
       (format nil "<PAref~(~a~)[~{~a~^, ~}]>" (render arr) args))))))

(defmethod jr-gensym ((renderer JSONStyle-Renderer) val)
  (if (symbolp val)
      (or (gethash val (jr-vars renderer))
          (prog1
              (setf (gethash val (jr-vars renderer)) (format nil "{\"symbol\": \"v~a\"}" (jr-cnt renderer)))
            (incf (jr-cnt renderer))))
      (format nil "{\"const\": ~a}" val)))

(defmethod caten/runtime/renderer:%render-const ((renderer JSONStyle-Renderer) obj dtype)
  (jr-gensym renderer obj))
;; JSON-style kernel: serialize blueprint for dbcache identity
(define-kernel (JSONStyle-Kernel JSONStyle-Renderer) () nil
  :specs
  ((:FUNCTION ((:FUNCTION (body))
               ->
               ((node graph)
                (let* ((globals
                         (loop for node in (graph-nodes graph)
                               if (eql (node-type node) :DEFINE-GLOBAL)
                                 collect node))
                       (gstr (with-output-to-string (o)
                               (loop for g in globals
                                     for i from 0
                                     for name = (car (node-writes g))
                                     for dtype = (getattr g :dtype)
                                     for mode  = (getattr g :mode)
                                     for ptrp  = (getattr g :pointer-p) do
                                       (when (> i 0) (format o ","))
                                       (format o "{\"arg\":~a,\"dtype\":\"~a_~a_~(~a~)\"}"
                                               (const name :float32)
                                               (if ptrp "*" "") mode dtype)))))
                  (format nil "{\"globals\":[~a],\"body\":~a}" gstr (render-kernel body))))))
   (:PROGN ((:PROGN (~ _))
            ->
            ((node graph)
             (let ((items (map 'list #'render-kernel (node-reads node))))
               (format nil "{\"progn\":[~{~a~^,~}]}" items)))))
   (:IF ((:IF ((:EXPR (cond)) body))
         ->
         ((node graph)
          (format nil "{\"if\":{\"cond\":~a,\"then\":~a}}"
                  (render cond) (render-kernel body)))))
   (:FOR ((:FOR ((:RANGE ((:EXPR (size)) (:EXPR (step))) :dtype dtype :idx idx) body))
          ->
          ((node graph)
           (format nil "{\"for\":{\"idx\":~a,\"lower\":0,\"upper\":~a,\"step\":~a,\"body\":~a}}"
                   (const idx :int64) (render size) (render step) (render-kernel body)))))
   (:EXPR ((:EXPR ((:SETF (_ _))))
           ->
           ((node graph)
            (format nil "{\"expr_store\":~a}" (render (car (node-reads node))))))
          ((:EXPR (_))
           ->
           ((node graph)
            (let* ((type (car (relay-writes (read-type-relay node))))
                   (ctype (caten/ir:tensor-relay-dtype type))
                   (sym   (const (car (node-writes node)) :float32))
                   (val   (render (car (node-reads node)))))
              (format nil "{\"expr\":{\"id\":\"~a\",\"sym\":~a,\"value\":~a}}" ctype sym val)))))))

(defun print-blueprint (blueprint stream &key (cls 'Default-Kernel))
  (declare (type caten/ir:ASTGraph blueprint))
  (let ((program
          (caten/runtime/kernel:kernel-program
           (caten/runtime/kernel:make-kernel blueprint :cls cls))))
    (if stream
        (format stream "~a" program)
        program)))
