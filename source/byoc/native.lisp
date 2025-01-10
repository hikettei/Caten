(defpackage :caten/byoc/native
  (:documentation "BACKEND=NATIVE to use Lisp JIT")
  (:use :cl :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime
   :caten/codegen/backend :caten/codegen/renderer :caten/air
   :caten/codegen/expr :caten/codegen/helpers :caten/codegen/shape-inference)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler)
  (:import-from
   :caten/byoc/lisp
   #:LispBuffer))

(in-package :caten/byoc/native)
;; [TODO]
;; - [ ] Inline all mathematical functions! (compile nil generated-code) will produce no errors. (PRs are welcome)
;;   - [ ] Add proper type declarations in %render-node, from given read-type-relay.
(define-auto-scheduler (Native-Auto-Scheduler ()) :n-global-loop 1)
(defclass NativeRuntime (GraphRuntime) nil)
(define-backend :native LispBuffer NativeRuntime LispStyle-Renderer Native-Auto-Scheduler t)
(defclass LispStyle-Renderer (Renderer) nil)

(defun global-type-spec (node)
  (declare (type node node))
  (assert (eql (node-type node) :DEFINE-GLOBAL))
  `(type
    ,(if (getattr node :pointer-p)
         `(simple-array ,(dtype->lisp (getattr node :dtype)) (*))
         (dtype->lisp (getattr node :dtype)))
       ,(const (car (node-writes node)))))

(defmethod %render-kernel ((renderer LispStyle-Renderer) schedule-item)
  (let* ((args (schedule-item-args schedule-item)))
    `(lambda (,@(map 'list #'(lambda (x) (const (car (node-writes x)))) args))
       (declare (optimize (speed 3) (safety 0)) ,@(map 'list #'global-type-spec args))
       ,(recursive-render-bp (getattr schedule-item :blueprint)))))

(defun wrap-with-caller (kernel body &aux (args (gensym)))
  `(lambda (&rest ,args &aux (lparallel:*kernel* ,kernel))
     (apply ,body (map 'list #'(lambda (m) (if (buffer-p m) (buffer-value m) m)) ,args))))

(defmethod %compile-kernel ((renderer LispStyle-Renderer) items dir)
  (when (>= (ctx:getenv :JIT_DEBUG) 3)
    (format t "[Final Code]:~%")
    (dolist (item items)
      (when (getattr item :rendered-object)
        (format t "~a"
                (with-output-to-string (tmp)
                  ;; (format tmp "~%[Blueprint: ~A]:~%~A~%Disassembly for ~a:~%```~%" (getattr item :name) (getattr item :rendered-object) (getattr item :name))
                  (disassemble (compile nil (getattr item :rendered-object)) :stream tmp)
                  (format tmp "~%```~%"))))))
  (let ((kernel (lparallel:make-kernel (cl-cpus:get-number-of-processors))))
    (dolist (item items)
      (when (getattr item :rendered-object)
        (setf (getattr item :compiled-object) (wrap-with-caller kernel (getattr item :rendered-object))
              (getattr item :rendered-object) (princ-to-string (getattr item :rendered-object)))))))

(defun const (obj)
  (if (symbolp obj)
      (intern (string-upcase (princ-to-string obj)))
      obj))

(defmethod %render-const ((renderer LispStyle-Renderer) object) (const object))
;; Binary
(macrolet ((def (id op &optional (offset 0))
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((lhs (render-node renderer (nth ,(+ 0 offset) (node-reads node))))
                      (rhs (render-node renderer (nth ,(+ 1 offset) (node-reads node))))
                      (wrap-around-p (getattr node :wrap-around :allow-undefined t))
                      (max (caten/common.dtype:dtype/max (buffer-dtype (car (relay-writes (read-type-relay node)))))))
                  (if wrap-around-p
                      (list 'mod (list ',op lhs rhs) (1+ max))
                      (list ',op lhs rhs))))))
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
                      (dtype (buffer-dtype (car (relay-writes (read-type-relay node))))))
                  (if (eql dtype :bool)
                      `(,',op-boolean ,lhs ,rhs)
                      `(,',op-number ,lhs ,rhs))))))
  (def :AND logand and)
  (def :OR logior or)
  (def :XOR logxor alexandria:xor))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :!=)) node)
  (let ((lhs (render-node renderer (nth 1 (node-reads node))))
        (rhs (render-node renderer (nth 2 (node-reads node)))))
    `(not (= ,lhs ,rhs))))
;; Unary
(declaim (inline log2 exp2))
(defun log2 (x) (log x 2))
(defun exp2 (x) (expt 2 x))
(macrolet ((def (id op)
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((x (render-node renderer (nth 0 (node-reads node))))
                      (rt (dtype->lisp (buffer-dtype (car (relay-reads (read-type-relay node))))))
                      (wt (dtype->lisp (buffer-dtype (car (relay-writes (read-type-relay node)))))))
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
  (let ((idx (render-aref-index renderer node)))
    (if idx
        `(aref ,(const (getattr node :storage-id)) ,idx)
        (const (getattr node :storage-id)))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Move)) node) (render-node renderer (second (node-reads node))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Store)) node) (render-node renderer (second (node-reads node))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Allocate)) node) nil)
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Cast)) node)
  (let ((dtype-from (buffer-dtype (second (relay-reads (read-type-relay node)))))
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

(defmethod  %render-node ((renderer LispStyle-Renderer) (id (eql :Index-Components)) node)
  (let ((out-dtype (buffer-dtype (car (relay-writes (read-type-relay node)))))
        (components (render-expr 'LispStyle-Renderer (expr-index-components renderer node (renderer-index-space renderer)))))
    (case out-dtype
      ((:float64 :float32) `(coerce ,components ',(dtype->lisp out-dtype)))
      (otherwise components))))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :WHERE)) node)
  `(if ,(render-node renderer (nth 0 (node-reads node))) ,(render-node renderer (nth 1 (node-reads node))) ,(render-node renderer (nth 2 (node-reads node)))))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :WMMA)) node)
  (let ((x (render-node renderer (nth 0 (node-reads node))))
        (y (render-node renderer (nth 1 (node-reads node))))
        (z (render-node renderer (nth 2 (node-reads node)))))
    `(+ ,x (* ,y ,z))))

(defun extract-scop-from-loop (for)
  (declare (type node for))
  (assert (eql (node-type for) :FOR))
  (let ((below (expr-detach-loop-bound (getattr for :below) :allow-failed t)))
    (when (and below (expr-equal-to (getattr for :upfrom) 0) (expr-equal-to (getattr for :by) 1))
      below)))

(defun recursive-render-bp (rest-blueprints)
  (let ((bp (car rest-blueprints)))
    (when (null bp) (return-from recursive-render-bp nil))
    (ecase (node-type bp)
      (:FOR
       (let* ((endfor (position-if #'(lambda (x) (and (eql (node-type x) :ENDFOR) (equal (getattr x :idx) (getattr bp :idx)))) rest-blueprints)))
         (assert endfor () "recursive-render-bp: :FOR without :ENDFOR is not allowed. Malformed blueprint?")
         (let ((below (extract-scop-from-loop bp)))
           (if below
               `(progn
                  (,(if (eql (getattr bp :scope) :local) 'dotimes 'lparallel:pdotimes) (,(const (intern (princ-to-string (getattr bp :idx)))) ,(render-expr 'LispStyle-Renderer below))
                    ,(recursive-render-bp (subseq rest-blueprints 1 endfor)))
                  ,(recursive-render-bp (subseq rest-blueprints (1+ endfor))))
               (progn
                 (when (eql (getattr bp :scope) :global) (warn "recursive-render-bp: The node ~a is scheduled as global but scheduled as local because the upfrom/below/by is too complicated to handle.~%Thus this loop is not parallelized." bp))
                 `(progn
                    (loop with ,(const (intern (princ-to-string (getattr bp :idx)))) fixnum = ,(render-expr 'LispStyle-Renderer (getattr bp :upfrom))
                          while ,(render-expr 'LispStyle-Renderer (getattr bp :below))
                          do ,(recursive-render-bp (subseq rest-blueprints 1 endfor))
                             (incf ,(const (intern (princ-to-string (getattr bp :idx)))) ,(render-expr 'LispStyle-Renderer (getattr bp :by))))
                    ,(recursive-render-bp (subseq rest-blueprints (1+ endfor)))))))))
      (:ENDFOR
       (error ":ENDFOR should not be appeared here. Malformed blueprint?"))
      (:IF
       ;; [TODO] ENDIF does not have :idx so cannot determine the pair of :IF and :ENDIF
       ;; This is why LispStyle Renderer does not support IF statement yet.
       (error "LispStyle Renderer currently does not support IF statement."))
      (:ENDIF
       (error "LispStyle Renderer currently does not support IF statement."))
      (:EXPR
       (let ((write-index (render-index 'LispStyle-Renderer bp :nth 0))
             (id (const (car (node-writes bp))))
             (dtype (buffer-dtype (car (relay-writes (read-type-relay bp)))))
             (decl-p (car (getattr bp :declare-type))))
         `(,@(if decl-p `(let ((,id ,(render-expr 'LispStyle-Renderer (getattr bp :EXPR) :index-space (getattr bp :iterations))))) '(progn))
           ,@(if decl-p `((declare (type ,(dtype->lisp dtype) ,id))))
           ,(when (null decl-p)
              `(setf ,(if write-index `(aref ,id ,write-index) id) ,(render-expr 'LispStyle-Renderer (getattr bp :EXPR) :index-space (getattr bp :iterations))))
           ,(recursive-render-bp (cdr rest-blueprints)))))
      (:DEFINE-GLOBAL
       (recursive-render-bp (cdr rest-blueprints))))))
