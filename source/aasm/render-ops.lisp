(in-package :caten/aasm)

(defmacro with-blueprint ((&key (noopt nil)) &body body)
  `(let* ((*ctx* (make-graph))
          (out (progn ,@body)))
     (assert (node-p out) () "The last form must be a node.")
     (setf (graph-outputs *ctx*) (node-writes out))
     (let ((graph (->fast-graph *ctx*)))
       (unless ,noopt (setf graph (simplify-ast graph)))
       graph)))
;; ~~ Control Flows ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %range (bind size body &key (step 1) (dtype *default-int*) (out (gensym "RANGE")))
  (declare (type (or node symbol) bind) (type (or node symbol) body) (type (or symbol node fixnum) size step) (type keyword dtype) (type symbol out))
  (let ((bind (if (symbolp bind)
                  (%bind bind (%iconst bind :dtype dtype))
                  bind)))
    (emit (make-node :Render :RANGE (list out) (map 'list #'node->id1 (list bind size step body))))))

(defun %if (condition body &key (out (gensym "IF")))
  (declare (type (or symbol node) condition body) (type symbol out))
  (emit (make-node :Render :IF (list out) (map 'list #'node->id1 (list condition body)))))

(defun %when (condition body &key (out (gensym "IF"))) (%if condition body :out out))

(defun %progn (&rest body &aux (out (gensym "PROGN")))
  (assert (every #'(lambda (x) (or (symbolp x) (node-p x))) body) () "%progn: The body must be a list of symbols or nodes.")
  (emit (make-node :Render :PROGN (list out) (map 'list #'node->id1 body))))

(defun %expr (&rest nodes &aux (out (gensym "EXPR")))
  (assert (every #'(lambda (x) (or (symbolp x) (node-p x))) nodes) () "%expr: The body must be a list of symbols or nodes.")
  (emit (make-node :Render :EXPR (list out) (map 'list #'node->id1 nodes))))

(defun %defglobal (name)
  (emit (make-node :Render :DEFINE-GLOBAL (list name) nil)))

(defun %barrier (&key (out (gensym "BARRIER"))) (emit (make-node :Render :BARRIER (list out) nil)))

(defun %bind (name node)
  (declare (type symbol name) (type node node))
  (assert (= 1 (length (node-writes node))) () "%bind: The node must have exactly one read.")
  (setf (node-writes node) (list name))
  node)

(defun %aref (name idx &key (out (gensym "AREF")))
  (declare (type (or symbol node) name idx))
  (emit (make-node :Render :Aref (list out) (map 'list #'node->id1 (list name idx)))))
;; ~~ AST Simplification ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defsimplifier
    (simplify-control-flow :speed 0)
    ((:PROGN (~ args))
     ->
     ((node graph)
      (let ((args-new (map 'list #'(lambda (x) (id->value graph x)) args)))
        (when (some #'(lambda (x) (and (node-p x) (eql :PROGN (node-type x)))) args-new)
          (make-node :Render :PROGN (node-writes node)
                     (loop for arg in args
                           for arg-new in args-new
                           if (and (node-p arg-new) (eql :PROGN (node-type arg-new)))
                             append (node-reads arg-new)
                           else
                             collect arg))))))
    ((:IF (cond1 (:IF (cond2 body))))
     ->
     ((node graph)
      (with-context-nodes
        (new-cond (%and cond1 cond2))
        (out (%if new-cond body :out (car (node-writes node)))))))
    ;; [TODO] Tile the range
    ;; [TODO] Remove the ISL dependencies
    ;; If the size==1 -> remove the range
    ;; 
;;    ((:RANGE (bind size step body)) -> ((node graph))
    )

(defun simplify-ast (graph)
  (declare (type graph graph))
  (setf graph (optimize-aasm graph :heavy-opt-threshold 0))
  (simplify-control-flow graph))

(defun print-ast (graph)
  (print graph)
  (pprint-graph graph)
  ;;(caten/air:->dot graph :pathname "/tmp/graph.dot")
  (viz-ast graph)
  )
;; [TODO] Decompose 3 -> 1 + 1 + 1 and optimize the indexing?
(defun viz-ast (graph &aux (indent 0) (seen))
  (print
   (with-output-to-string (out)
     (labels ((indent () (make-string indent :initial-element #\space))
              (fmt (desig &rest args) (apply #'format out (format nil "~a~a~%" (indent) desig) args))
              (r (s &aux (val (id->value graph s)))
                (when (and val (null (find (node-id val) seen)))
                  (f val)
                  (push (node-id val) seen))
                s)
              (f (node)
                (case (node-type node)
                  (:PROGN
                    (fmt "{")
                    (incf indent 2) (mapc #'r (node-reads node)) (decf indent 2)
                    (fmt "}"))
                  (:DEFINE-GLOBAL (fmt "defglobal ~a;" (car (node-writes node))))
                  (:RANGE
                      (multiple-value-bind (bind size step body) (apply #'values (node-reads node))
                        (fmt "for (~(~a~)=0; ~(~a~)<~(~a~); ~(~a~)+=~a)" (r bind) (r bind) (r size) (r bind) (r step))
                        (r body)))
                  (:ALLOCATE (fmt "~(~a~) ~(~a~);" (getattr node :dtype) (car (node-writes node))))
                  (:LOAD (r (car (node-reads node))) (fmt "~(~a~) = ~(~a~);" (car (node-writes node)) (getattr node :value)))
                  (:Aref
                   (multiple-value-bind (name idx) (apply #'values (node-reads node))
                     (r name) (r idx)
                     (fmt "~(~a~) = ~(~a~)[~(~a~)];" (car (node-writes node)) name idx)))
                  (:IF
                   (multiple-value-bind (cond body) (apply #'values (node-reads node))
                     (r cond)
                     (fmt "if (~(~a~)) {" cond)
                     (incf indent 2) (r body) (decf indent)
                     (fmt "}")))
                  (otherwise (mapc #'r (node-reads node)) (fmt "~(~a~) = ~(~a~)(~(~a~));" (car (node-writes node)) (node-type node) (render-list (node-reads node)))))))            
       (f (id->value graph (car (graph-outputs graph))))))))
;; [TODO] OpFusion
;; PROGN+PROGN -> PROGN
;; IndexingをもっとSimplifyしたい。RANGEの外に出す方法？
;; イメージ:
;; ./caten/codegen -> caten/ir/render-ops.lispの機能を使って色々AST変形を実施する
;; - Remove :GLOBAL :LOCAL If Guard (which is rebundant only)
;; - Remove :LOAD is an args of buffer, instead, use :DEFINE-GLOBAL
(print-ast
 (with-blueprint ()
   (%progn ;; [TODO] Introduce %Function instead of PROGN?
    (%defglobal 'a)
    (%defglobal 'b)
    (%defglobal 'm)
    (%defglobal 'n)
    (%range
     'gid0 (%add (%iconst 'm) (%iconst 'n))
     (%progn
      (%progn
       (let ((idx1 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst 'gid0)))
             (idx2 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst 'gid0))))
         (%when (%< nil :row (%iconst 'gid0) (%add (%iconst 'm) (%iconst 'n)))
                (%when (%< nil :row (%iconst 'gid0) (%add (%iconst 'm) (%iconst 'n)))
                       (%progn
                        (%add (%aref 'a (%iconst 0)) (%aref 'b idx2))
                        (%add (%aref 'a (%add idx1 (%iconst 1))) (%aref 'b (%add idx2 (%iconst 1))))
                        (%add (%aref 'a (%add idx1 (%iconst 2))) (%aref 'b (%add idx2 (%iconst 2))))
                        (%add (%aref 'a (%add idx1 (%iconst 3))) (%aref 'b (%add idx2 (%iconst 3))))))))))))))
;; [TODO]
;; - OpFusion
;; - TileBands
;;  - Vectorize
;;  - Parallelize
;;  - It is IR's obligation to provide the information to opfusion
