(defpackage :caten/byoc/lisp
  (:use :cl :caten/runtime/buffer :caten/common.dtype :caten/runtime/runtime
   :caten/codegen/backend :caten/codegen/renderer :caten/air
        :caten/codegen/expr :caten/codegen/helpers :caten/codegen/shape-inference)
  (:import-from
   :caten/codegen/config
   #:define-auto-scheduler)
  (:export :LispBuffer))

(in-package :caten/byoc/lisp)

(defclass LispBuffer (AbstractBuffer) nil)

(defmethod open-buffer (runtime (buffer LispBuffer))
  (let ((initial-value (if (eql (buffer-dtype buffer) :bool)
                           nil
                           (coerce 0 (dtype->lisp (buffer-dtype buffer))))))
    (if (= 0 (buffer-nrank buffer))
        (setf (buffer-value buffer) initial-value)
        (setf (buffer-value buffer) (make-array (apply #'* (buffer-shape buffer)) :element-type (dtype->lisp (buffer-dtype buffer)) :initial-element initial-value)))))

(defmethod close-buffer (runtime (buffer LispBuffer)) (setf (buffer-value buffer) nil))
(defmethod transfer-from-array (runtime (buffer LispBuffer) array) (setf (buffer-value buffer) array))
(defmethod transfer-into-array (runtime (buffer LispBuffer)) (buffer-value buffer))
(defmethod copy-buffer-value (runtime (buffer LispBuffer)) (copy-seq (buffer-value buffer)))
(defmethod bref ((buffer LispBuffer) idx) (aref (buffer-value buffer) idx))

(define-backend :lisp LispBuffer GraphRuntime nil nil nil)
;; ~~~~ Lisp JIT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(define-auto-scheduler (Lisp-Auto-Scheduler ()) :n-global-loop 1)
(define-backend :lisp_jit LispBuffer GraphRuntime LispStyle-Renderer Lisp-Auto-Scheduler t)
(defclass LispStyle-Renderer (Renderer) nil)

(defun global-type-spec (node)
  (declare (type node node))
  (assert (eql (node-type node) :DEFINE-GLOBAL))
  `(type
    ,(if (getattr node :pointer-p)
         `(simple-array ,(dtype->lisp (getattr node :dtype)) (*))
         (dtype->lisp (getattr node :dtype)))
    ,(car (node-writes node))))

(defmethod %render-kernel ((renderer LispStyle-Renderer) schedule-item)
  (let* ((args (schedule-item-args schedule-item)))
    (print
    `(lambda (,@(map 'list #'(lambda (x) (car (node-writes x))) args))
       (declare (optimize (speed 3) (safety 1)) ,@(map 'list #'global-type-spec args))
       ,(recursive-render-bp (getattr schedule-item :blueprint))))))

(defmethod %compile-kernel ((renderer LispStyle-Renderer) items dir)
  (dolist (item items)
    (when (getattr item :rendered-object)
      (setf (getattr item :compiled-object) (getattr item :rendered-object)
            (getattr item :rendered-object) (princ-to-string (getattr item :rendered-object))))))

(defmethod %render-const ((renderer LispStyle-Renderer) object) object)
;; Binary
(macrolet ((def (id op)
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((lhs (render-node renderer (nth 0 (node-reads node))))
                      (rhs (render-node renderer (nth 1 (node-reads node)))))
                  (list ',op lhs rhs)))))
  (def :ADD +)
  (def :MUL *)
  (def :MAX max)
  (def :< <))

(macrolet ((def (id op-number op-boolean)
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((lhs (render-node renderer (nth 0 (node-reads node))))
                      (rhs (render-node renderer (nth 1 (node-reads node))))
                      (ph (gensym)))
                  ;; [TODO] Dispatch at compilation time...
                  `(let ((,ph ,lhs))
                     (if (numberp ,ph)
                         (,',op-number ,ph ,rhs)
                         (,',op-boolean ,ph ,rhs)))))))
  (def :AND logand and)
  (def :OR logior or)
  (def :XOR logxor alexandria:xor))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :!=)) node)
  (let ((lhs (render-node renderer (nth 0 (node-reads node))))
        (rhs (render-node renderer (nth 1 (node-reads node)))))
    `(not (= ,lhs ,rhs))))
;; Unary
(macrolet ((def (id op &rest args)
             `(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql ,id)) node)
                (let ((x (render-node renderer (nth 0 (node-reads node)))))
                  (list ',op x ,@args)))))
  (def :NEG -)
  (def :NOT not)
  (def :SIN sin)
  (def :log2 log 2)
  (def :exp2 expt 2)
  (def :RECIP /)
  (def :sqrt sqrt))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :LOAD)) node) (getattr node :value))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Aref)) node)
  (let ((idx (render-aref-index renderer node)))
    (if idx
        `(aref ,(getattr node :storage-id) ,idx)
        (getattr node :storage-id))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Move)) node) (render-node renderer (second (node-reads node))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Store)) node) (render-node renderer (second (node-reads node))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Allocate)) node) nil)
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :Cast)) node)
  (let ((x (render-node renderer (car (node-reads node)))))
    ;; [TODO] Inline dtype/cast
    `(dtype/cast ,x ,(getattr node :dtype))))
(defmethod  %render-node ((renderer LispStyle-Renderer) (id (eql :Index-Components)) node)
  (render-expr 'LispStyle-Renderer (expr-index-components renderer node (renderer-index-space renderer))))
(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :WHERE)) node)
  `(if ,(render-node renderer (nth 0 (node-reads node))) ,(render-node renderer (nth 1 (node-reads node))) ,(render-node renderer (nth 2 (node-reads node)))))

(defmethod %render-node ((renderer LispStyle-Renderer) (id (eql :WMMA)) node)
  (let ((x (render-node renderer (nth 0 (node-reads node))))
        (y (render-node renderer (nth 1 (node-reads node))))
        (z (render-node renderer (nth 2 (node-reads node)))))
    `(+ ,x (* ,y ,z))))

(defun recursive-render-bp (rest-blueprints)
  (let ((bp (car rest-blueprints)))
    (when (null bp) (return-from recursive-render-bp nil))
    (ecase (node-type bp)
      (:FOR
       (let* ((endfor (position-if #'(lambda (x) (and (eql (node-type x) :ENDFOR) (equal (getattr x :idx) (getattr bp :idx)))) rest-blueprints)))
         (assert endfor () "recursive-render-bp: :FOR without :ENDFOR is not allowed. Malformed blueprint?")
         (when (eql (getattr bp :scope) :global)
           (warn "LispStyle-Renderer: global loop is not supported yet."))
         ;; [TODO] Simplify the loop code and use lparallel
         `(progn
            (loop with ,(intern (getattr bp :idx)) fixnum = ,(render-expr 'LispStyle-Renderer (getattr bp :upfrom))
                  while ,(render-expr 'LispStyle-Renderer (getattr bp :below))
                  do ,(recursive-render-bp (subseq rest-blueprints 1 endfor))
                     (incf ,(intern (getattr bp :idx)) ,(render-expr 'LispStyle-Renderer (getattr bp :by))))
            ,(recursive-render-bp (subseq rest-blueprints (1+ endfor))))))
      (:ENDFOR
       (warn "ENDFOR SHOULD BE SKIPPED?"))
      (:IF
       ;; [TODO] ENDIF does not have :idx so cannot determine the pair of :IF and :ENDIF
       ;; This is why LispStyle Renderer does not support IF statement yet.
       (error "LispStyle Renderer currently does not support IF statement."))
      (:ENDIF
       (error "LispStyle Renderer currently does not support IF statement."))
      (:EXPR
       (let ((write-index (render-index 'LispStyle-Renderer bp :nth 0))
             (id (car (node-writes bp)))
             (dtype (buffer-dtype (car (relay-writes (read-type-relay bp)))))
             (decl-p (car (getattr bp :declare-type))))
         `(,@(if decl-p `(let ((,id ,(render-expr 'LispStyle-Renderer (getattr bp :EXPR) :index-space (getattr bp :iterations))))) '(progn))
           ,@(if decl-p `((declare (type ,(dtype->lisp dtype) ,id))))
           ,(when (null decl-p)
              `(setf ,(if write-index `(aref ,id ,write-index) id) ,(render-expr 'LispStyle-Renderer (getattr bp :EXPR) :index-space (getattr bp :iterations))))
           ,(recursive-render-bp (cdr rest-blueprints)))))
      (:DEFINE-GLOBAL
       (recursive-render-bp (cdr rest-blueprints))))))
