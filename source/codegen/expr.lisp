(defpackage :caten/codegen/expr
  (:documentation "`Expr` is a syntax sugar for the ops in `caten/aasm`")
  (:use :cl :caten/air :caten/aasm)
  (:import-from
   :caten/codegen/helpers
   :nodes-write-to)
  (:export
   #:with-expr
   #:expr-graft-after
   #:expr-graph
   #:Expr
   #:expr-out
   #:make-expr
   #:expr-from-graph
   #:expr-const
   #:expr-scalar-equivalent-p
   #:expr-add
   #:expr-sub
   #:expr-mul
   #:expr-div
   #:expr-idiv
   #:expr-and
   #:expr-max
   #:expr-min
   #:expr-mod
   #:expr-or
   #:expr-<
   #:expr-<=
   #:expr->
   #:expr->=
   #:expr-=
   #:expr-where
   #:expr-neg
   #:expr-not
   #:with-expr-cache))

(in-package :caten/codegen/expr)

(defstruct Expr
  "Expr is a graph wrapper that reprensents a computation whose node leaves are scalar (scalar number, aref from the tensor) and each computation is a scalar."
  (graph (error "graph must occur") :type Graph)
  (out (error "out must occur") :type node))

(defsimplifier
    (%get-scalar)
    ((:Load ((:Allocate () :nrank 0 :dtype dtype)) :value value)
     ->
     (:_TmpScalarConst (value) :dtype dtype)))

(defmethod expr-graft-after ((expr Expr) id new-expr)
  (assert (not (typep (expr-graph expr) 'FastGraph)) () "remnode not tested for Graph? [todo: fix]")
  (let* ((tmp->id (make-hash-table))
         (id->tmp (make-hash-table))
         (written
           (append
            (loop for e in (list expr new-expr)
                  append
                  (loop for node in (graph-nodes (expr-graph e))
                        if (not (eql (node-type node) :Aref))
                          append (node-writes node)
                        if (eql (node-type node) :Aref)
                          do (setf (gethash (car (node-writes node)) tmp->id) (getattr node :storage-id)
                                   (gethash (getattr node :storage-id) id->tmp) (car (node-writes node)))))))
         (written (map 'list #'(lambda (x) (gethash x id->tmp x)) written)))
    ;; wanna assert (every #'identity written) ?
    ;; Possible duplicated writing case: (written is a candidate of the duplicated writing)
    ;; EXPR      : x = y + z
    ;; NEW_EXPR  : x_jit_tmp = x[...];  // remove this aref, and replace x_jit_tmp with x.
    ;;           : out = sin(x_jit_tmp)
    (labels ((rewrite-p (x) (find x written))
             (newid (x) (if (rewrite-p x) (or (gethash x tmp->id) x) x))
             (rewrite-node (node)
               (when (not (eql (node-type node) :Aref))
                 (setf (node-reads node) (map 'list #'newid (node-reads node))
                       (node-writes node) (map 'list #'newid (node-writes node))))))
      (mapc #'rewrite-node (graph-nodes (expr-graph new-expr)))
      (mapc #'rewrite-node (graph-nodes (expr-graph expr)))
      (setf (graph-nodes (expr-graph expr)) (append (graph-nodes (expr-graph new-expr)) (graph-nodes (expr-graph expr)))))))

(defmethod run-expr-with-vars ((expr Expr) vars)
  (let ((graph
           (apply
            #'make-graph
            (loop for node in (graph-nodes (expr-graph expr))
                  if (eql (node-type node) :Load)
                    collect
                    (let ((cp (copy-node node)))
                      (setf (getattr cp :value) (or (gethash (getattr cp :value) vars) (getattr cp :value)))
                      cp)
                  else
                    collect node))))
    (setf (graph-outputs graph) (list (car (node-writes (expr-out expr))))
          graph (->fast-graph graph))
    (fold-constant graph)
    (when (= (length (graph-nodes graph)) 2)
      (%get-scalar graph)
      (when (and (= (length (graph-nodes graph)) 1)
		 (eql :_TmpScalarConst (node-type (car (graph-nodes graph)))))
	(car (node-reads (car (graph-nodes graph))))))))

(defmethod expr-scalar-equivalent-p ((expr1 Expr) (expr2 Expr))
  "Returns T if expr1 and expr2 performs the equivalent computation.
Only supports the scalar computation because it is intended to identify the same and dynamic stride computation"
  (flet ((enumerate (x key)
           (loop for node in (graph-nodes (expr-graph x))
                 if (and (eql (node-type node) :Load) (funcall key (getattr node :value)))
                   collect (getattr node :value))))
    ;; Dynamic Shape is a positive integer.
    ;; For all x[uint32], f_expr1(...) and f_expr2(...) plots the same graph -> they are equivalent.
    (let* ((allsyms (append (enumerate expr2 #'symbolp) (enumerate expr1 #'symbolp)))
           ;; It is just a "sufficient condition" though...
           ;; Only works if the graph is consisted of simple arithmetic operations.
           (lower
             (+ 1 (apply #'max (or (append (enumerate expr2 #'numberp) (enumerate expr1 #'numberp)) (list 3)))))
           (realized-nums
             (loop for a in allsyms collect (incf lower)))
           (vars (make-hash-table)))
        (loop for sym in allsyms
              do (setf (gethash sym vars) (pop realized-nums)))
      ;; a/b returns nil if failed.
      (multiple-value-bind (a b) (values (run-expr-with-vars expr1 vars) (run-expr-with-vars expr2 vars))
        (and a b (eql a b))))))

(defmethod simplify-expr ((expr Expr))
  ;; [TODO] Use FastGraph
  (optimize-aasm (expr-graph expr))
  expr)

(defun %connect-expr (grh args out)
  (declare (type graph grh))
  (assert (every #'expr-p args))
  (let ((graph (apply #'make-graph (append (apply #'append (map 'list (alexandria:compose #'graph-nodes #'expr-graph) args)) (graph-nodes grh)))))
    (setf (graph-outputs graph) (list out))
    (simplify-expr (make-expr :graph graph :out (id->value graph out)))))

(defun expr-from-graph (id graph)
  "Create an Expr that is a subgraph of `graph` with the output `id`."
  (declare (type graph graph))
  (assert (symbolp id))
  ;; Get a subgraph from `id`
  (let ((g (apply #'make-graph (graph-nodes graph))))
    (setf (graph-outputs g) (list id))
    (setf g (->fast-graph g))
    (verify-graph g)
    (if (null (id->value g id))
        (progn
          (assert (null (graph-nodes g)))
          ;; Assuming ID=Dynamic Shape
          (expr-const id :int64))
        (make-expr :graph (apply #'make-graph (graph-nodes g)) :out (id->value g id)))))

(defun expr-const (value dtype &aux (out (gensym "w")))
  (when (expr-p value) (return-from expr-const value))
  (assert (or (numberp value) (symbolp value)) () "expr-const: the value must be a number or a symbol. getting ~a" value)
  (let ((nodes (graph-nodes (with-context (_ (%fconst value :dtype dtype))))))
    (setf (node-writes (or (find :LOAD nodes :key #'node-type) (error "?"))) (list out))
    (assert (eql (node-type (car nodes)) :Allocate))
    (%connect-expr
     (make-graph (second nodes))
     (list (%connect-expr (make-graph (car nodes)) nil (car (node-writes (car nodes)))))
     out)))

(macrolet ((def (name op)
             `(defun ,name (a b &aux (out (gensym "w")))
                (declare (type Expr a b))
                (let ((grh (with-context (_ (,op (expr-out a) (expr-out b) :id out)))))
                  (%connect-expr grh (list a b) out)))))
  (def expr-add-binary %add)
  (def expr-sub-binary %sub)
  (def expr-mul-binary %mul)
  (def expr-div-binary %div)
  (def expr-idiv-binary %idiv)
  (def expr-and-binary %and)
  (def expr-or-binary %or)
  (def expr-max-binary %max)
  (def expr-mod-binary %mod))

(macrolet ((def (name op)
             `(defun ,name (a b &aux (out (gensym "w")))
                (declare (type Expr a b))
                (let ((grh (with-context (_ (,op nil :row (expr-out a) (expr-out b) :id out)))))
                  (%connect-expr grh (list a b) out)))))
  (def expr-<-binary %<)
  (def expr-<=-binary %<=)
  (def expr->-binary %>)
  (def expr->=-binary %>=)
  (def expr-=-binary %=))

(macrolet ((def (name op)
             `(defun ,name (x &aux (out (gensym "w")))
                (declare (type Expr x))
                (let ((grh (with-context (_ (,op (expr-out x) :id out)))))
                  (%connect-expr grh (list x) out)))))
  (def expr-neg %neg)
  (def expr-not %not))

(defun expr-add (&rest args)
  (if (= (length args) 0)
      (expr-const 0 :int64)
      (reduce #'expr-add-binary args)))
(defun expr-sub (&rest args) (reduce #'expr-sub-binary args))
(defun expr-mul (&rest args)
  (if (= (length args) 0)
      (expr-const 1 :int64)
      (reduce #'expr-mul-binary args)))
(defun expr-div (&rest args) (reduce #'expr-div-binary args))
(defun expr-idiv (&rest args) (reduce #'expr-idiv-binary args))
(defun expr-and (&rest args) (reduce #'expr-and-binary args))
(defun expr-or (&rest args) (reduce #'expr-or-binary args))
(defun expr-max (&rest args) (reduce #'expr-max-binary args))
(defun expr-binary-min (a b)
  (expr-neg (expr-max (expr-neg a) (expr-neg b))))
(defun expr-min (&rest args) (apply #'expr-binary-min args))
(defun expr-mod (&rest args) (apply #'expr-mod-binary args))

(defun expr-< (&rest args) (reduce #'expr-<-binary args))
(defun expr-<= (&rest args) (reduce #'expr-<=-binary args))
(defun expr-> (&rest args) (reduce #'expr->-binary args))
(defun expr->= (&rest args) (reduce #'expr->=-binary args))
(defun expr-= (&rest args) (reduce #'expr-=-binary args))
(defun expr-where (condition then else &aux (out (gensym "w")))
  (declare (type Expr condition then else))
  (let ((grh (with-context (_ (%where (expr-out condition) (expr-out then) (expr-out else) :id out)))))
    (%connect-expr grh (list condition then else) out)))
  

