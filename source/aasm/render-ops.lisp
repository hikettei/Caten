(in-package :caten/aasm)

(defmacro with-blueprint ((&key (noopt nil)) &body body)
  `(let* ((*ctx* (make-graph))
          (out (progn ,@body)))
     (assert (node-p out) () "The last form must be a node.")
     (setf (graph-outputs *ctx*) (node-writes out))
     (let ((graph *ctx*))
       (unless ,noopt (simplify-ast graph))
       graph)))
;; ~~ Control Flows ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %range (bind body &key (start 0) (end 1) (step 1) (dtype *default-int*) (out (gensym "RANGE")))
  (declare (type (or node symbol) bind) (type (or node symbol) body) (type (or symbol node fixnum) start end step) (type keyword dtype) (type symbol out))
  (let ((bind (if (symbolp bind)
                  (%bind bind (%iconst bind :dtype dtype))
                        bind)))
    (emit (make-node :Render :RANGE (list out) (map 'list #'node->id1 (list bind start end step body))))))

(defun %if (condition body &key (out (gensym "IF")))
  (declare (type (or symbol node) condition body) (type symbol out))
  (emit (make-node :Render :IF (list out) (map 'list #'node->id1 (list condition body)))))

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

(defun print-ast (graph)
  (print graph)
  (pprint-graph graph))

(defsimplifier (simplify-control-flow :speed 0))

(defun simplify-ast (graph)
  (declare (type graph graph))
  (let ((graph (->graph graph)))
    (optimize-aasm graph :heavy-opt-threshold 0)
    (->fast-graph graph)))
;; PROGN+PROGN -> PROGN
(print-ast
 (with-blueprint ()
   (%progn
    (%defglobal 'a)
    (%defglobal 'b)
    (%range
     'gid0
     (%progn
      (let ((idx1 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst 'gid0)))
            (idx2 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst 'gid0))))
        (%if (%< nil :row (%iconst 'gid0) (%add (%iconst 'm) (%iconst 'n)))
             (%add (%aref 'a idx1) (%aref 'b idx2)))))
     :start 0 :end (%add (%iconst 'm) (%iconst 'n))))))
