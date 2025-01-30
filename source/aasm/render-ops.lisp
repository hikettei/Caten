(in-package :caten/aasm)

(defmacro with-blueprint (&body body)
  `(let* ((*ctx* (make-graph))
          (out (progn ,@body)))
     (assert (node-p out) () "The last form must be a node.")
     (setf (graph-outputs *ctx*) (node-writes out))
     (->fast-graph *ctx*)))

(defun %range (bind body &key (start 0) (end 1) (step 1) (dtype *default-int*) (out (gensym "RANGE")))
  (declare (type symbol bind) (type (or node symbol) body) (type (or symbol node fixnum) start end step) (type keyword dtype) (type symbol out))
  (emit (make-node :Render :RANGE (list out) (map 'list #'node->id1 (list (%bind bind (%iconst bind :dtype dtype)) start end step body)))))

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

(defun print-ast (graph)
  (print graph)
  (pprint-graph graph))

(print-ast
 (with-blueprint
   (%progn
    (%defglobal 'a)
    (%defglobal 'b)
    (%range
     'gid0
     (%progn
      (let ((a (%add (%iconst 1) (%iconst 2)))
            (b (%add (%iconst 2) (%iconst 3))))
        (%mul a b)))
     :start 0 :end (%iconst 'n)))))
