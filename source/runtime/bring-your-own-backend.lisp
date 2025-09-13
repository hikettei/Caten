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
   #:default-renderer
   ))

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
(defmacro define-renderer (renderer-name direct-superclasses direct-slots &rest patterns)
  (alexandria:with-gensyms (renderer node)
    `(prog1
         (defclass ,renderer-name (,@direct-superclasses caten/runtime/renderer:Renderer) ,direct-slots)
       ,@(loop for pattern in patterns
               do (assert (and (listp pattern) (keywordp (car pattern))) () "define-renderer: pattern := `(,node_id ,@(pattern_match_rules))")
               collect
               `(defmethod caten/runtime/renderer:%render-node ((,renderer ,renderer-name) (node-id (eql ,(car pattern))) ,node)
                  (flet ((render (id) (caten/runtime/renderer:render-node ,renderer id))
                         (const (id dtype) (caten/runtime/renderer:%render-const ,renderer id dtype)))
                    (caten/graph:node-ematch ,node ,@(cdr pattern))))))))

(defmacro define-kernel ((kernel-name renderer-name) direct-superclasses direct-slots &key (launch) (compile) (specs))
  (flet ((ensure-lambda (n form name)
           (assert (listp (car form)))
           (assert (>= (length (car form)) n) () "the argument ~a expects at least ~a arguments, getting ~a" name n form)
           (apply
            #'values
            (append
             (loop for i upfrom 0 below n collect (nth i (car form)))
             (list (if (= 2 (length form)) (cdr form) `(progn ,@(cdr form))))))))
    `(progn
       (defclass ,kernel-name (,@direct-superclasses caten/runtime/kernel:Kernel) ,direct-slots)
       (defmethod caten/runtime/kernel:kernel-load-blueprint ((kernel ,kernel-name) (blueprint caten/ir:ASTGraph))
         (let ((renderer (make-instance ',renderer-name :graph blueprint)))
           ;; Derive argument names and types from :DEFINE-GLOBAL nodes
           (let* ((globals (remove-if-not #'(lambda (n) (eql (node-type n) :DEFINE-GLOBAL)) (graph-nodes blueprint)))
                  (argnames (map 'list #'(lambda (n) (car (node-writes n))) globals))
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
               `(defmethod caten/runtime/kernel:%render-kernel-op ((renderer ,renderer-name) (op-id (eql ,(car pattern))) node)
                  (flet ((render (id) (caten/runtime/renderer:render-node renderer id))
                         (const (id dtype) (caten/runtime/renderer:%render-const renderer id dtype))
                         (render-kernel (id) (caten/runtime/kernel:render-kernel-node renderer id)))
                    (caten/graph:node-ematch node ,@(cdr pattern))))))))
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
