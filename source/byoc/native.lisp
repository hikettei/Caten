(defpackage :caten/byoc/lisp
  (:use :cl :cffi :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime
        :caten/codegen/byoc :caten/codegen/renderer :caten/air
        :caten/aasm :caten/aasm/expr :caten/codegen/helpers :caten/codegen/iteration)
  (:import-from :caten/byoc/lisp #:LispBuffer))

(in-package :caten/byoc/lisp)

(defclass NativeRuntime (GraphRuntime) nil)
(defclass NativeKernel (AbstractKernel)
  ((code :accessor native-code)
   (caller :accessor native-caller)))
(define-auto-scheduler Native-Auto-Scheduler :use-parallel 1)
(defclass LispStyle-Renderer (Renderer) nil)
(define-backend :native LispBuffer NativeRuntime LispStyle-Renderer NativeKernel Native-Auto-Scheduler t)

(defun const (obj)
  (if (symbolp obj)
      (intern (string-upcase (princ-to-string obj)))
      obj))

(defun global-type-spec (node)
  (declare (type node node))
  (assert (eql (node-type node) :DEFINE-GLOBAL))
  `(type
    ,(if (getattr node :pointer-p)
         `(simple-array ,(dtype->lisp (getattr node :dtype)) (*))
         (dtype->lisp (getattr node :dtype)))
       ,(const (car (node-writes node)))))

(defun wrap-with-caller (kernel body &aux (args (gensym)))
  `(lambda (&rest ,args &aux (lparallel:*kernel* ,kernel))
     (caten/runtime/profile:with-real-time
       (apply ,body (map 'list #'(lambda (m) (if (buffer-p m) (buffer-value m) m)) ,args)))))

(defmethod %render-kernel ((renderer LispStyle-Renderer) (item NativeKernel))
  (let* ((args (kernel-args item)))
    (setf (native-code item)
          `(lambda (,@(map 'list #'(lambda (x) (const (car (node-writes x)))) args))
             (declare (optimize (speed 3) (safety 0)) ,@(map 'list #'global-type-spec args))
             ,(recursive-render-bp (kernel-blueprint item))))))

(defmethod %compile-kernel ((renderer LispStyle-Renderer) items dir)
  (when (>= (ctx:getenv :JIT_DEBUG) 3)
    (format t "[Final Code]:~%")
    (dolist (item items)
      (format t "~a"
              (with-output-to-string (tmp)
                ;; (format tmp "~%[Blueprint: ~A]:~%~A~%Disassembly for ~a:~%```~%" (getattr item :name) (getattr item :rendered-object) (getattr item :name))
                (disassemble (compile nil (native-code item)) :stream tmp)
                (format tmp "~%```~%")))))
  (let ((kernel (lparallel:make-kernel (cl-cpus:get-number-of-processors))))
    (dolist (item items)
      (setf (native-caller item) (compile nil (wrap-with-caller kernel (native-code item)))))))

(defmethod %render-const ((renderer LispStyle-Renderer) object) (const object))
;; Binary
(macrolet ((def (id op &optional (offset 0))
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((lhs (render-node renderer (nth ,(+ 0 offset) (node-reads node))))
                      (rhs (render-node renderer (nth ,(+ 1 offset) (node-reads node))))
                      (wrap-around-p (getattr node :wrap-around :allow-undefined t))
                      (max (caten/common.dtype:dtype/max (tensor-relay-dtype (car (relay-writes (read-type-relay node))))))
                      (outtype (dtype->lisp (tensor-relay-dtype (car (relay-writes (read-type-relay node))))))
                      (lhstype (dtype->lisp (tensor-relay-dtype (nth ,(+ 0 offset) (relay-reads (read-type-relay node))))))
                      (rhstype (dtype->lisp (tensor-relay-dtype (nth ,(+ 1 offset) (relay-reads (read-type-relay node)))))))
                  `(the ,outtype 
                        ,(if wrap-around-p
                             (list 'mod (list ',op `(the ,lhstype ,lhs) `(the ,rhstype rhs)) (1+ max))
                             (list ',op `(the ,lhstype ,lhs) `(the ,rhstype ,rhs))))))))
  (def :ADD +)
  (def :MUL *)
  (def :IDIV floor)
  (def :MOD mod)
  (def :MAX max)
  (def :< < 1)) ;; < is a TernaryOps where <(out_placeholder, x, y)

(macrolet ((def (id op-number op-boolean)
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((lhs (render-node renderer (nth 0 (node-reads node))))
                      (rhs (render-node renderer (nth 1 (node-reads node))))
                      (dtype (tensor-relay-dtype (car (relay-writes (read-type-relay node))))))
                  (if (eql dtype :bool)
                      `(,',op-boolean ,lhs ,rhs)
                      `(,',op-number ,lhs ,rhs))))))
  (def :AND logand and)
  (def :OR logior or)
  (def :XOR logxor alexandria:xor))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :!=)) node)
  (let ((lhs (render-node renderer (nth 1 (node-reads node))))
        (rhs (render-node renderer (nth 2 (node-reads node))))
        (type-map (map 'list (alexandria:compose #'dtype->lisp #'tensor-relay-dtype) (relay-reads (read-type-relay node)))))
    `(the boolean (not (= (the ,(nth 1 type-map) ,lhs) (the ,(nth 2 type-map) ,rhs))))))
;; Unary
(declaim (inline log2 exp2))
(defun log2 (x) (log x 2))
(defun exp2 (x) (expt 2 x))
(macrolet ((def (id op)
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((x (render-node renderer (nth 0 (node-reads node))))
                      (rt (dtype->lisp (tensor-relay-dtype (car (relay-reads (read-type-relay node))))))
                      (wt (dtype->lisp (tensor-relay-dtype (car (relay-writes (read-type-relay node)))))))
                  (when (and (eql id :SQRT) (eql rt 'single-float))
                    (setf rt `(,rt 0.0)))
                  (list 'the wt (list ',op (list 'the rt x)))))))
  (def :NEG -)
  (def :NOT not)
  (def :SIN sin)
  (def :log2 log2)
  (def :exp2 exp2)
  (def :RECIP /)
  (def :sqrt sqrt))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :LOAD)) node) (const (getattr node :value)))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Aref)) node)
  (let* ((p (id->value (renderer-graph renderer) (car (node-reads node))))
         (idx (if (or (null p) (not (eql (node-type p) :BIND))) (car (node-reads node)) (getattr p :value)))
         (access (render-node renderer (second (node-reads node)))))
    `(aref ,(const idx) ,access)))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :SETF)) node)
  `(setf ,(render-node renderer (car (node-reads node))) ,(render-node renderer (second (node-reads node)))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :BIND)) node) (const (getattr node :value)))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Move)) node) (render-node renderer (second (node-reads node))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Store)) node) (render-node renderer (second (node-reads node))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Allocate)) node) nil)
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Cast)) node)
  (let ((dtype-from (tensor-relay-dtype (second (relay-reads (read-type-relay node)))))
        (dtype-to (getattr node :dtype))
        (dtype-to-lisp (caten/common.dtype:dtype->lisp (getattr node :dtype)))
        (x (render-node renderer (second (node-reads node)))))
    (ecase dtype-from
      (:bool `(if ,x ,(coerce 1 dtype-to-lisp) ,(coerce 0 dtype-to-lisp))) ;; bool -> int/float
      ((:float64 :float32)
       (if (find dtype-to `(:float64 :float32))
           `(coerce ,x ',dtype-to-lisp) ;; float -> float
           `(coerce (truncate ,x) ',dtype-to-lisp))) ;; float -> int
      ((:int64 :int32 :int16 :int8 :uint64 :uint32 :uint16 :uint8)
       ;; int -> float/int
       `(coerce ,x ',dtype-to-lisp)))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :LET)) node) (const (car (node-writes node))))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :WHERE)) node)
  (let ((types (map 'list (alexandria:compose #'dtype->lisp #'tensor-relay-dtype) (relay-reads (read-type-relay node)))))
    `(the ,(second types) (if (the ,(car types) ,(render-node renderer (nth 0 (node-reads node)))) (the ,(second types) ,(render-node renderer (nth 1 (node-reads node)))) (the ,(third types) ,(render-node renderer (nth 2 (node-reads node))))))))

(defnode (:LispRender :LET) () "" :slots ((dtype)))
(defun %let (form value rest-body dtype)
  (declare (type list rest-body))
  (emit (make-node :LispRender :LET (list form) (append (list value) rest-body) :dtype dtype)))

(defun recursive-render-bp (graph &aux (seen) (graph (->fast-graph (copy-graph graph))))
  (funcall
   (Simplifier
       ()
       ((:PROGN (~ _))
        ->
        ((node graph)
         (let* ((found-p)
                (exprs
                  (loop for r in (node-reads node) for v = (id->value graph r) for s = (when v (id->value graph (car (node-reads v))))
                        if (and v (eql (node-type v) :EXPR) (not (eql (node-type s) :SETF)))
                          collect (progn (setf found-p t) (cons v (tensor-relay-dtype (car (relay-reads (read-type-relay v))))))
                        else
                          collect r)))
           (when found-p
             (with-context-nodes
                 (out (let ((stack))
                        (loop for e in (reverse exprs)
                              if (consp e)
                                do (setf stack (list (node->id (%let (car (node-writes (car e))) (car (node-reads (car e))) stack (cdr e)))))
                              else
                                do (push e stack))
                        (apply #'%progn stack)))))))))
   graph)
  (labels ((r (s &aux (val (id->value graph s)))
             (if (and val (null (find (node-id val) seen)))
                 (prog1 (f val) (push (node-id val) seen))
                 s))
           (e (id)
             (let ((renderer (make-instance 'LispStyle-Renderer :graph graph)))
               (render-node renderer id)))
           (f (node)
             (case (node-type node)
               (:PROGN
                 `(progn ,@(map 'list #'r (node-reads node))))
               (:EXPR
                (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                    (e (car (node-reads node)))
                    (error "EXPR w/o SETF should be rewritten as LET by LispStyleRenderer.")))
               (:LET
                 `(let ((,(const (car (node-writes node))) ,(e (car (node-reads node)))))
                    (declare (type ,(dtype->lisp (getattr node :dtype)) ,(const (car (node-writes node)))))
                    ,@(map 'list #'r (cdr (node-reads node)))))
               (:DEFINE-GLOBAL) (:RANGE) (:ALLOCATE) ;; [TODO] Add a simplifier which removes :DEFINE-GLOBAL, RANGE, ALLOCATE from :PROGN.reads
               (:FOR
                (multiple-value-bind (range body) (apply #'values (node-reads node))
                  (setf range (id->value graph range))
                  (assert (and range (eql (node-type range) :RANGE)) () "The first argument of :FOR should be :RANGE, getting ~a" range)
                  (multiple-value-bind (bind size step) (values (getattr range :idx) (first (node-reads range)) (second (node-reads range)))
                    (when (symbolp size)
                      (let ((val (id->value graph size)))
                        (assert (and val (eql (node-type val) :EXPR)) () "Range: The size must be specified as EXPR or fixnum, getting ~a" val)
                        (setf size (e (car (node-reads val))))))
                    (when (symbolp step)
                      (let ((val (id->value graph step)))
                        (assert (and val (eql (node-type val) :EXPR)) () "Range: The step must be specified as EXPR or fixnum, getting ~a" val)
                        (setf step (e (car (node-reads val))))))
                    ;; [TODO] parallel!
                    (if (eql step 1)
                        `(dotimes (,(const bind) ,size) ,(r body))
                        (let ((tmp (gensym)))
                          ;; for (i=0; i<125; i+=64) 64,
                          `(dotimes (,tmp (floor ,size ,step))
                             (let ((,(const bind) (* ,tmp ,step)))
                               ,(r body))))))))
               (:IF
                (multiple-value-bind (cond body) (apply #'values (node-reads node))
                  (setf cond (id->value graph cond))
                  (assert (and cond (eql (node-type cond) :EXPR)) () "IF: the conditon must be EXPR.")
                  `(if ,(e (car (node-reads cond))) ,(r body))))
               (:BARRIER (error "thread barrier is not supported on clang"))
               (:DEFINE-SHARED-MEMORY (error "shared memory is not supported on clang"))
               (otherwise (error "The node ~a is not a supported renderop by clang" node)))))
    (f (id->value graph (car (graph-outputs graph))))))

(defmethod kernel-call ((kernel NativeKernel) (runtime NativeRuntime) node args)
  (apply (native-caller kernel) args))
