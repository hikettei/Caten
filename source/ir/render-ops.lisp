;;;; render-ops.lisp
;;;; This file includes the following features which is required to generate the optimized code.
;;;; - ASTGraph Creation
;;;; - ASTGraph Simplification
;;;; - ASTGraph Optimization (e.g.: Tile, Unroll, Microkernel, etc)
(in-package :caten/ir)

(defclass ASTGraph (FastGraph) nil)

(defmacro with-blueprint ((&key (noopt nil)) &body body)
  `(let* ((*ctx* (make-graph))
          (out (progn ,@body)))
     (assert (node-p out) () "The last form must be a node.")
     (setf (graph-outputs *ctx*) (node-writes out))
     (let ((graph (->fast-graph *ctx* :cls 'ASTGraph)))
       (unless ,noopt (setf graph (%simplify-ast graph)))
       graph)))
;; ~~ Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %expr (name &key (out (gensym "EXPR"))) (emit (make-node :Render :EXPR (list out) (list (node->id1 name)))))

(defun %range (bind size body &key (step 1) (dtype *default-int*) (out (gensym "RANGE")) (range) (rid bind))
  "
```
(%range bind size body &key (step 1) (dtype *default-int*) (out (gensym \"RANGE\")) (mark :noopt))
```
Constraints:
- SIZE/STEP is always an EXPR, that is, must not include an control flow.
"
  (declare (type symbol bind) (type (or node symbol) body) (type (or symbol node fixnum) size step) (type keyword dtype) (type symbol out))
  (when (node-p size) (setf size (%expr (node->id1 size))))
  (when (node-p step) (setf step (%expr (node->id1 step))))
  (let ((range (or range (emit (make-node :Render :RANGE (list rid) (map 'list #'node->id1 (list size step)) :idx bind :dtype dtype)))))
    (emit (make-node :Render :FOR (list out) (map 'list #'node->id1 (list range body))))))

(defun %if (condition body &key (out (gensym "IF")))
  "
Constraints:
- condition is always an EXPR, that is, must not include an control flow."
  (declare (type (or symbol node) condition body) (type symbol out))
  (when (node-p condition) (setf condition (%expr (node->id1 condition))))
  (emit (make-node :Render :IF (list out) (map 'list #'node->id1 (list condition body)))))

(defun %when (condition body &key (out (gensym "IF")))
  (%if condition body :out out))

(defun %progn (&rest body &aux (out (gensym "PROGN")))
  (setf body (flatten body))
  (assert (every #'(lambda (x) (or (symbolp x) (node-p x))) body) () "%progn: The body must be a list of symbols or nodes.")
  (emit (make-node :Render :PROGN (list out) (map 'list #'node->id1 (loop for b in body if b collect b)))))

(defun %function (prog &key (name (gensym "FNAME")) (out (gensym "FUNCTION")))
  (declare (type (or symbol node) prog) (type symbol name out))
  (emit (make-node :Render :FUNCTION (list out) (list (node->id1 prog)) :name name)))

(defun %global (write-to name dtype pointer-p &key (mode :io))
  (declare (type dtype-t dtype) (type boolean pointer-p) (type symbol name write-to))
  (emit (make-node :Render :DEFINE-GLOBAL (list write-to) nil :name name :dtype dtype :pointer-p pointer-p :mode mode)))

(defun %local (name size dtype)
  (declare (type dtype-t dtype) (type list size) (type symbol name))
  (emit (make-node :Render :DEFINE-LOCAL (list name) size :dtype dtype)))

(defun %swizzle (name indices &key (out (gensym "SWIZZLE")))
  "name[indices[0]][indices[1]][...]"
  (declare (type symbol name) (type list indices))
  (let ((indices (map 'list #'node->id1 indices)))
    (emit (make-node :Render :SWIZZLE (list out) (append (list name) indices)))))

(defun %vector (from indices &key (out (gensym "VECTOR")))
  (declare (type symbol from))
  (emit (make-node :JIT :VECTOR (list out) (append (list from) indices))))

(defun %barrier (&key (out (gensym "BARRIER"))) (emit (make-node :Render :BARRIER (list out) nil)))

(defun %bind (name node)
  (declare (type symbol name) (type node node))
  (assert (= 1 (length (node-writes node))) () "%bind: The node must have exactly one read.")
  (setf (node-writes node) (list name))
  node)

(defun %aref (name idx &key (out (gensym "AREF")))
  (declare (type (or symbol node) name idx))
  (emit (make-node :JIT :Aref (list out) (map 'list #'node->id1 (list name idx)))))

(defun %polyaref (name strides affs &key (out (gensym "PAREF")))
  (declare (type (or symbol node) name) (type list strides affs))
  (assert (= (length strides) (length affs)))
  (emit (make-node :JIT :PolyAref (list out) (map 'list #'node->id1 (append (list name) strides affs)) :nrank (length strides))))

(defun %setf (tgt value &key (out (gensym "SETF")))
  (declare (type (or symbol node) tgt value))
  (emit (make-node :JIT :SETF (list out) (map 'list #'node->id1 (list tgt value)))))

(defun %empty (dtype) (make-node :Buffer :Allocate (list (gensym)) nil :dtype dtype :nrank 0))
;; ~~ ControlFlow Simplifiers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun Empty! (node)
  (assert (typep (node-attr node) 'RenderOps))
  (let ((node (copy-node node)))
    (setf (getattr node :is-empty) t)
    node))

(defpattern EConst (x)
  `(or
    (= ,x)
    (<Rule> :EXPR ((Var (= ,x) _)))))

(defsimplifier
    (simplify-control-flow :speed 0)
    ;; (PROGN (PROGN X)) -> (PROGN X))
    ((:PROGN (~ args))
     ->
     ((node graph)
      (flet ((empty-p (x) (and (node-p x) (typep (node-attr x) 'RenderOps) (getattr x :is-empty))))
        (let ((args-new (map 'list #'(lambda (x) (id->value graph x)) args)))
          ;; Removes Empty RenderOps, Merges nested progns
          (when (or
                 (some #'(lambda (x) (and (node-p x) (eql :PROGN (node-type x)))) args-new)
                 (some #'empty-p args-new))
            (make-node :Render :PROGN (node-writes node)
                       (loop for arg in args for arg-new in args-new
                             if (and (node-p arg-new) (eql :PROGN (node-type arg-new)))
                               append (node-reads arg-new)
                             else if (null (empty-p arg-new))
                                    collect arg)))))))
    ;; [Note] This must be applied after exprify is applied. because progn is a trigger.
    ((:IF ((:EXPR (cond1)) (:IF ((:EXPR (cond2)) body))))
     ->
     ((node graph)
      (with-context-nodes
        (out (%if (%and cond1 cond2) body :out (car (node-writes node)))))))
    ;; Removing Empty IF/Range
    ((:FOR ((:Range (_ _)) (:PROGN ())) :is-empty (guard x (null x))) -> ((node graph) (Empty! node)))
    ((:IF (_ (:PROGN ())) :is-empty (guard x (null x))) -> ((node graph) (Empty! node)))
    ((:FOR ((:Range (_ _)) (:FOR ((:RANGE (_ _)) _) :is-empty (guard x (identity x)))) :is-empty (guard y (null y))) -> ((node graph) (Empty! node)))
    ((:IF (_ (:IF (_ _) :is-empty (guard x (identity x)))) :is-empty (guard y (null y))) -> ((node graph) (Empty! node)))
    ((:FOR ((:RANGE ((EConst 0) _)) _)) -> ((node graph) (Empty! node)))
    ;; [TODO] (RANGE (+ A B) (+ A B)) should be also removable.
    ((:FOR ((:RANGE (a b) :idx idx :dtype dtype) body))
     ->
     ((node graph)
      (flet ((r (x &aux (val (id->value graph x)))
               (if val
                   (if (eql (node-type val) :EXPR)
                       (let ((const (id->value graph (car (node-reads val)))))
                         (if (and const (eql (node-type const) :LOAD))
                             (getattr const :value)
                             x))
                       x)
                   x)))
        (multiple-value-bind (a b) (values (r a) (r b))
          (when (if (and (numberp a) (numberp b))
                    (<= a b)
                    (eql a b))
            (let* ((range (id->value graph (car (node-reads node))))
                   (tmp (gensym))
                   (=0 (with-context-nodes (out (%bind tmp (%iconst 0 :dtype dtype)))))
                   (expr (%bind (car (node-writes range)) (%expr tmp)))
                   (body (id->value graph body)))
              (loop for node in (graph-nodes graph)
                    if (and (eql (node-type node) :RANGE) (eql (getattr node :idx) idx)) do
                      (let ((load (with-context-nodes (out (%bind (car (node-writes node)) (%iconst 0 :dtype dtype))))))
                        (dolist (load (graph-nodes graph))
                          (when (and (eql (node-type load) :LOAD) (eql (getattr load :value) (getattr node :idx)))
                            (setf (getattr load :value) 0)))
                        (insert-nodes graph load)))
              (insert-nodes graph (append =0 (list expr)))
              body))))))
    )

(defun ast-simplify-constant (graph &aux (seen))
  "Simplifies the load of constants"
  (declare (type ASTGraph graph))
  (labels ((no-reassign-p (node)
             (loop for tgt in (graph-nodes graph)
                   if (and (eql (node-type tgt) :BIND) (eql (getattr tgt :value) (car (node-writes node)))) do
                     (return-from no-reassign-p nil))
             t)
           (explore (x path-expr-p &aux (node (id->value graph x)))
             (when (or (null node) (find x seen)) (return-from explore))
             (push x seen)
             (let ((reads (map 'list #'(lambda (x) (id->value graph x)) (node-reads node)))
                   (path-expr-p (or path-expr-p (eql (node-type node) :EXPR))))
               (when (and path-expr-p (not (eql (node-type node) :RANGE)))
                 (loop for r in reads for nth upfrom 0
                       for load = (and r (eql (node-type r) :EXPR) (id->value graph (car (node-reads r))))
                       for alloc = (and load (eql (node-type load) :LOAD) (id->value graph (car (node-reads load))))
                       when (and r load alloc (null (node-reads alloc)) (no-reassign-p r))
                         do (let ((new-load (copy-node load))
                                  (new-alloc (copy-node alloc)))
                              (setf (node-id new-load) (gensym) (node-id new-alloc) (gensym)
                                    (node-writes new-alloc) (list (gensym))
                                    (node-reads new-load) (list (car (node-writes new-alloc)))
                                    (node-writes new-load) (list (gensym)))
                              (insert-nodes graph (list new-alloc new-load))
                              (setf (nth nth (node-reads node)) (car (node-writes new-load))))))
               (mapc #'(lambda (x) (explore x path-expr-p)) (node-reads node)))))
    (mapc #'(lambda (x) (explore x nil)) (graph-outputs graph)))
  graph)

(defun ast-purge-unused-expr (graph)
  (declare (type ASTGraph graph) (optimize (speed 3)))
  (loop for node in (graph-nodes graph)
        if (eql (node-type node) :PROGN) do
          (let ((users (map 'list #'(lambda (x) (id->value graph x)) (node-reads node)))
                (unused-expr))
            (loop for user in users
                  for r = (id->value graph (car (node-reads user)))
                  if (and (eql (node-type user) :EXPR) r (not (eql (node-type r) :SETF))) do
                    (let ((deps (id->users graph (car (node-writes user)))))
                      (declare (type list deps unused-expr))
                      (when (= (length deps) 1)
                        (assert (eql (node-type (car deps)) :PROGN))
                        (push (car (node-writes user)) unused-expr))))
            (when unused-expr
              (assert (every #'symbolp unused-expr))
              (setf (node-reads node)
                    (loop for r in (node-reads node)
                          unless (find (the symbol r) unused-expr) collect r)))))
  graph)
;; ~~ Exprify (OpFusion) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-descendants-graph (graph outputs &key (only-surface nil) (seen) (result) (stop-at (make-hash-table)))
  (declare (type ASTGraph graph) (type list outputs))
  (let ((out-ids (remove-duplicates (apply #'append (map 'list #'node-writes outputs)))))
    (labels ((explore (x &aux (node (id->value graph x)))
               (when (or (null node) (find x seen)) (return-from explore))
               (when (gethash x stop-at) (return-from explore))
               (when (eql (node-class node) :Render) (return-from explore))
               (when (eql (node-type node) :BIND) (return-from explore))
               (push x seen)
               (push node result)
               (unless only-surface (mapc #'explore (node-reads node)))))
      (mapc #'explore out-ids))
    (let* ((g (apply #'make-graph (remove-duplicates result :key #'node-id)))
           (reads (apply #'append (map 'list #'node-reads (graph-nodes g)))))
      (setf (graph-outputs g) (loop for o in out-ids if (null (find o reads)) collect o))
      (values (->fast-graph g) seen))))

(defun ast-make-sink-map (dg &aux (seen) (sink-map (make-hash-table)))
  (declare (type ASTGraph dg))
  (labels ((explore (x &aux (node (id->value dg x)))
             (when (or (null node) (find x seen)) (return-from explore))
             (push x seen)
             (when (> (length (id->users dg x)) 1)
               (setf (gethash x sink-map) t))
             (mapc #'explore (node-reads node))))
    (mapc #'explore (graph-outputs dg)))
  (mapc #'(lambda (x) (setf (gethash x sink-map) t)) (graph-outputs dg))
  sink-map)

(defun ast-tpsort-exprs-order-on-graph (base-graph exprs)
  "Givens exprs return a valid order"
  (let ((in-degree (make-hash-table)) (out-degree (make-hash-table)) (seen (make-hash-table))
        (queue) (sorted))
    (loop for node in (graph-nodes base-graph)
          when (and (eql (node-type node) :EXPR) (null (find (node-id node) exprs :key #'node-id))) do
            (setf (gethash (node-id node) seen) t))
    (loop for expr in exprs
          for expr-graph = (ast-expr-graph base-graph expr :include-expr t) do
            (setf (gethash (node-id expr) in-degree)
                  (loop for node in (graph-nodes expr-graph)
                        if (and (eql (node-type node) :EXPR) (null (gethash (node-id node) seen)))
                          collect node)
                  (gethash (node-id expr) in-degree) (remove-duplicates (gethash (node-id expr) in-degree) :key #'node-id))
            (loop for r in (gethash (node-id expr) in-degree)
                  if (null (find (node-id r) (gethash (node-id r) out-degree) :key #'node-id)) do
                    (push expr (gethash (node-id r) out-degree))))
    (loop for expr in exprs
          if (null (gethash (node-id expr) in-degree)) do (push expr queue))
    (loop while queue for expr = (pop queue) do
      (push expr sorted)
      (dolist (adj (gethash (node-id expr) out-degree))
        (setf (gethash (node-id adj) in-degree) (remove (node-id expr) (gethash (node-id adj) in-degree) :key #'node-id))
        (when (null (gethash (node-id adj) in-degree)) (push adj queue)))
      (remhash (node-id expr) out-degree))
    (assert (= (length sorted) (length exprs)))
    (reverse sorted)))

(defun ast-exprify-tensor-graph (base-graph dg sink-map &aux (exprs))
  (declare (type ASTGraph dg) (type hash-table sink-map))
  (labels ((exprify (id &aux (name (gensym "E")))
             (let ((expr (%expr name :out id))
                   (out-node (id->value dg id)))
               (push expr exprs)
               (setf (node-writes out-node) (list name))
               (insert-nodes base-graph (list expr out-node)))))
    (mapc #'exprify (hash-table-keys sink-map)))
  (ast-tpsort-exprs-order-on-graph base-graph exprs))

(defun exprify-ast (graph &aux (seen nil))
  "Groups multiple strongly connected ops into a single Expr. Expr and Expr are also mergeable."
  (declare (type ASTGraph graph) (optimize (speed 3)) (type list seen))
  ;; Find sink points
  (labels ((render-p (node) (eql (node-class node) :Render))
           ;; Note: Should I set :only-surface=T to optimize GeLU?
           (sort-progn-body (parents &aux (dg (ast-descendants-graph graph parents :only-surface nil)) (m (ast-make-sink-map dg)))
             ;; The descendant of parents is asseted not to have RenderOps.
             (assert (null (some #'render-p (graph-nodes dg))))
             (ast-exprify-tensor-graph graph dg m))
           (split-parent (parents &aux (results) (tmp))
             (declare (type list parents results tmp))
             (loop for p in parents
                   ;; Split the PROGN w/ RenderNode (Except for RANGE, RANGE should be always paired w/ FOR but INDEX-COMPONENTS also uses it)
                   if (and (render-p p) (not (eql (node-type p) :RANGE))) do (when tmp (push (reverse tmp) results)) (push p results) (setf tmp nil)
                     else do (push p tmp))
             (when tmp (push (reverse tmp) results))
             (reverse results))
           (exprify-from-list (ids)
             (loop with parents = (split-parent (map 'list #'(lambda (x) (id->value graph x)) ids))
                   for p in parents
                   if (listp p) append (let ((p (sort-progn-body p))) (insert-nodes graph p) p)
                     else collect p))
           (explore (id &aux (node (id->value graph id)))
             (when (or (null node) (find (the symbol id) seen)) (return-from explore))
             (push id seen)
             ;; A Expr is only mergeable with descendants w/ current PROGN.
             (case (node-type node)
               (:PROGN
                 (let ((new-progn (apply #'%progn (exprify-from-list (node-reads node)))))
                   (setf (node-writes new-progn) (node-writes node))
                   (insert-nodes graph (list new-progn)))))
             (mapc #'explore (node-reads (id->value graph id)))))
    (mapc #'explore (graph-outputs graph)))
  ;; [TODO]ここでPrognのChildがEXPRじゃないとError
  graph)
;; ~~ Expr Domain Simplifier  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Expr Domain Simplifier is responsible for:
;; - Remove Common Expressions in the blueprint (Node is singleton)
;; - Relocate EXPR if it is independent of loop
;; - Topological Sort of PROGN
;; - Reexprify
(defun ast-ensure-progn (graph &aux (visited (make-hash-table)))
  "Inserts extra PROGN undernearth :FOR and :IF"
  (declare (type ASTGraph graph))
  (labels ((explore (x &aux (node (id->value graph x)))
             (when (null node) (return-from explore x))
             (when (gethash (node-id node) visited) (return-from explore (car (node-writes node))))
             (setf (gethash (node-id node) visited) t)
             (case (node-type node)
               ((:FOR :IF)
                (assert (= 2 (length (node-reads node))))
                (let* ((new-body-idx (gensym))
                       (body (id->value graph (second (node-reads node))))
                       (new-body (when (and body (not (eql :PROGN (node-type body))))
                                   (with-context (_ (%bind new-body-idx (%progn (explore (second (node-reads node))))))))))
                  (when new-body
                    (insert-nodes graph (graph-nodes new-body))
                    (setf (second (node-reads node)) new-body-idx))
                  (mapc #'explore (node-reads node))))
               (otherwise
                (mapc #'explore (node-reads node))))
             (car (node-writes node))))
    (mapc #'explore (graph-outputs graph))
    graph))

(defun ast-rewrite-expr-as-ssa-style (graph &key (simplify-load nil)
                                      &aux
                                        (visited (make-hash-table)) (toplevel (id->value graph (car (graph-outputs graph))))
                                        (whitelist `(:DEFINE-GLOBAL :DEFINE-LOCAL :ALLOCATE :RANGE :LOAD :BIND :SETF :VECTOR :SWIZZLE)))
  "ast-rewrite-expr-as-ssa-style decomposes all EXPRs in the graph as:
```
A <- EXPR(a+b*c)
```
==>
```
K <- EXPR(b*c)
L <- EXPR(K+a)
A <- L
```
"
  (declare (type ASTGraph graph))
  (when (not simplify-load) (push :AREF whitelist))
  (assert (= 1 (length (graph-outputs graph))))
  (assert (and toplevel (or (eql (node-type toplevel) :PROGN))) () "The ASTGraph should always start with :PROGN")
  (labels ((explore (id &key (expr-subgraph-p nil) (scope) &aux (node (id->value graph id)))
             (when (or (null node) (gethash (node-id node) visited)) (return-from explore))
             (setf (gethash (node-id node) visited) t)
             (case (node-type node)
               (:PROGN
                 (map 'list #'(lambda (x) (explore x :expr-subgraph-p expr-subgraph-p :scope node)) (node-reads node))) ;; Bodies
               (:EXPR
                ;; EXPR -> EXPR ==> STOP
                (when expr-subgraph-p (return-from explore nil))
                (explore (car (node-reads node)) :expr-subgraph-p t :scope scope))
               ((:IF :FOR) (mapc #'(lambda (x) (explore x :expr-subgraph-p expr-subgraph-p :scope scope)) (node-reads node)))
               (otherwise
                (when expr-subgraph-p
                  ;; Rewrite the node as EXPR
                  (assert (= 1 (length (node-writes node))))
                  (when (null (find (node-type node) whitelist)) ;; 0 FLOPs, they are allowed to duplicated
                    (assert scope () "The ASTGraph should start with :PROGN, rewrite graph w/ ast-ensure-progn first!~%~a")
                    (let ((copied (copy-node node)) (tmpid (gensym)) (dst (car (node-writes node))))
                      (setf (node-id copied) (gensym "NID")
                            (node-writes copied) (list tmpid))
                      (insert-nodes graph (list copied))
                      (insert-nodes graph (list (%expr tmpid :out dst)))
                      (push dst (node-reads scope))))
                  (map 'list #'(lambda (x) (explore x :expr-subgraph-p t :scope scope)) (node-reads node)))))))
    (explore (car (graph-outputs graph)))))

(defun ast-ensure-expr-is-singleton (graph &aux (cached) (rewrite-map (make-hash-table)) (ecache (make-hash-table)))
  (declare (type ASTGraph graph) (optimize (speed 3)))
  (multiple-value-bind (node-to-loops all-loops exprs) (%make-parse-ctx graph)
    (declare (ignore all-loops exprs))
    (labels ((expr-conds (expr)
               (loop for cond in (gethash (node-id expr) node-to-loops)
                     if (eql (getf cond :type) :if)
                       collect (node-id (getf cond :if-node))))
             (expr-search-key (expr)
               (when (gethash (node-id expr) ecache) (return-from expr-search-key (gethash (node-id expr) ecache)))
               (let ((c (id->value graph (car (node-reads expr))))
                     (seen (make-hash-table)))
                 (assert (and (eql (node-type expr) :EXPR) c))
                 (labels ((e (id &aux (node (id->value graph id)))
                            ;; [TODO] 足りないKeyがないか？
                            (when (null node) (return-from e id))
                            (when (gethash (node-id node) seen) (return-from e `(:SEEN ,id)))
                            (setf (gethash (node-id node) seen) t)
                            (case (node-type node)
                              ;; [TODO] Float4 Support etc...
                              (:SPACE `(:SPACE ,(getattr node :level) ,(getattr node :rank) ,(getattr node :dtype)))
                              (:EXPR
                               (let ((id (gethash (node-id node) rewrite-map)))
                                 `(:EXPR ,(if id (node-id id) (node-id node)))))
                              (:BIND `(:BIND ,(e (car (node-reads node))) :as ,(getattr node :value)))
                              (:DEFINE-GLOBAL `(:DEFINE-GLOBAL ,(car (node-writes node)) ,(getattr node :pointer-p)))
                              (:DEFINE-LOCAL  `(:DEFINE-LOCAL ,(car (node-writes node)) ,(node-reads node)))
                              (:ALLOCATE `(:ALLOCATE ,@(map 'list #'e (node-reads node)) :dtype ,(getattr node :dtype)))
                              (:LOAD
                               (let ((alloc (id->value graph (car (node-reads node)))))
                                 (if (and (eql (node-type alloc) :ALLOCATE) (null (node-reads alloc)))
                                     `(:Var ,(getattr node :value) ,(getattr alloc :dtype))
                                     `(:LOAD ,(e (car (node-reads node))) :value ,(getattr node :value)))))
                              (:RANGE `(:Var ,(getattr node :idx) ,(getattr node :dtype)))
                              (otherwise `(,(node-type node) (,@(map 'list #'e (node-reads node))))))))
                   (setf (gethash (node-id expr) ecache) `(:COND ,(expr-conds expr) ,(e (car (node-writes c))))))))
             (expr-eq (a b &aux (as (expr-search-key a)) (bs (expr-search-key b)))
               ;; TODO:
               ;; - :ADD :MULはInterchangeできる。
               ;; ^ 一意に定まるようにSortする
               (equal as bs)))
      (loop for node in (reverse (tpsort-graph graph))
            if (eql (node-type node) :EXPR) do
              (let ((cache (find node cached :test #'expr-eq)))
                (if cache
                    (setf (gethash (node-id node) rewrite-map) cache)
                    (push node cached)))) ;; first seen
      (let ((newid-cache (make-hash-table)) (changed-p nil))
        (flet ((newid (x)
                 (when (gethash x newid-cache) (return-from newid (gethash x newid-cache)))
                 (let ((val (id->value graph x)))
                   (when (null val) (return-from newid (setf (gethash x newid-cache) x)))
                   (when (not (eql (node-type val) :EXPR)) (return-from newid (setf (gethash x newid-cache) x)))
                   (let ((replacements (gethash (node-id val) rewrite-map)))
                     (if replacements
                         (progn
                           (setf changed-p t)
                           (setf (gethash x newid-cache) (car (node-writes replacements))))
                         (setf (gethash x newid-cache) x))))))
          (loop for node in (graph-nodes graph) do
            (setf (node-reads node) (map 'list #'newid (node-reads node))))
          (verify-graph graph)
          (if changed-p (ast-ensure-expr-is-singleton graph) graph))))))

(defun %make-parse-ctx (graph)
  (let* ((ctx (uiop:symbol-call :caten/codegen/polyhedral :make-scop-ctx-from-blueprint graph :allow-if t))
         (node-to-loops (uiop:symbol-call :caten/codegen/polyhedral :ctx-node-to-loops ctx))
         (allloops (uiop:symbol-call :caten/codegen/polyhedral :ctx-all-loops ctx))
         (exprs (uiop:symbol-call :caten/codegen/polyhedral :ctx-exprs ctx)))
    (values node-to-loops allloops exprs)))

(defun ast-rewrite-ssa-style-as-tree (graph)
  (declare (type ASTGraph graph))
  (loop for node in (graph-nodes graph)
        if (eql (node-type node) :EXPR) do
          (let* ((users (id->users graph (car (node-writes node))))
                 (prgn (find :PROGN users :key #'node-type))
                 (user (find :PROGN users :key #'node-type :test-not #'eql)))
            (when (and (= 2 (length users)) prgn user (not (eql (node-class user) :Render)))
              (setf (node-reads user) (loop for r in (node-reads user)
                                            if (eql r (car (node-writes node))) collect (car (node-reads node))
                                              else collect r))))))

(defstruct %TCtx
  (i    (make-hash-table)) ;; S(1~i)
  (i+1  (make-hash-table)) ;; S(i+1)
  (conditions (make-hash-table))
  (variables nil))

(defun ast-fixup-scope (graph &aux (expr-to-condition (make-hash-table)) (range-ids) (id-cache (make-hash-table)))
  ;; Create cache in advance
  (dolist (n (graph-nodes graph))
    (when (eql (node-type n) :RANGE)
      (push (getattr n :idx) range-ids))
    (when (eql (node-type n) :SETF)
      (setf (gethash (car (node-reads n)) id-cache) t)))
  (verify-graph graph)
  (multiple-value-bind (node-to-loops all-loops exprs) (%make-parse-ctx graph)
    (declare (ignore all-loops))
    (labels ((expr-depend-vars (expr &aux (seen (make-hash-table)) (ids))
               (assert (and expr (eql (node-type expr) :EXPR)))
               ;; [TODO] cache it!
               (labels ((e (id &aux (node (id->value graph id)))
                          (when (or (null node) (gethash (node-id node) seen))
                            (return-from e))
                          (setf (gethash (node-id node) seen) t)
                          (case (node-type node)
                            (:EXPR  (push (car (node-writes node)) ids))
                            (:RANGE (push (getattr node :idx) ids))
                            (:LOAD  (when (find (getattr node :value) range-ids) (push (getattr node :value) ids)))
                            (:BIND  (mapc #'e (node-reads node)))
                            (otherwise (mapc #'e (node-reads node))))))
                 (e (car (node-reads expr))))
               (assert (every #'symbolp ids))
               (remove-duplicates ids))
             (expr-depend-vectors (expr &aux (seen (make-hash-table)) (ids))
               (assert (and expr (eql (node-type expr) :EXPR)))
               (labels ((e (id &aux (node (id->value graph id)))
                          (when (or (null node) (gethash (node-id node) seen))
                            (return-from e))
                          (setf (gethash (node-id node) seen) t)
                          (case (node-type node)
                            (:EXPR nil)
                            (:VECTOR
                             (let ((val (id->value graph (car (node-reads node)))))
                               (if (eql (node-type val) :BIND)
                                   (push (car (node-reads val)) ids)
                                   (push (car (node-reads node)) ids))))
                            (otherwise (mapc #'e (node-reads node))))))
                 (e (car (node-reads expr))))
               (remove-duplicates ids))
             (get-loops (ls) (loop for l in ls if (eql :loop (getf l :type)) collect l))
             (expr-schedule-write (expr)
               ;; for reduction definition
               (when (not (id-is-reduction-p (car (node-writes expr))))
                 (return-from expr-schedule-write nil))
               (let* ((dom (map 'list #'(lambda (x) (getf x :idx)) (get-loops (gethash (node-id expr) node-to-loops))))
                      (user-doms
                        (remove-duplicates
                         (loop for user in exprs
                               if (find (car (node-writes expr)) (expr-depend-vars user))
                                 append (map 'list #'(lambda (x) (getf x :idx)) (get-loops (gethash (node-id user) node-to-loops)))))))
                 (list :should-seen dom
                       :should-unseen (loop for u in user-doms if (null (find u dom)) collect u)
                       :should-after nil)))
             (expr-schedule-read (current-dom id &aux (node (id->value graph id)))
               ;; reduced variable users
               (when (null node)
                 (return-from expr-schedule-read))
               (when (not (id-is-reduction-p id))
                 (let ((maybe-setf (id->value graph (car (node-reads node)))))
                   (when (eql :SETF (node-type maybe-setf))
                     (return-from expr-schedule-read (expr-schedule-read current-dom (car (node-reads maybe-setf)))))))
               (let* ((sched (expr-schedule-write node)) ;; back to the definition
                      (dom-ids (map 'list #'(lambda (x) (getf x :idx)) (get-loops (gethash (node-id current-dom) node-to-loops))))
                      (aft (loop for id in (getf sched :should-unseen) unless (find id dom-ids) collect id)))
                 (when aft
                   (list :should-seen nil
                         :should-unseen nil
                         :should-after aft))))
             (expr-vector-schedule (reader-expr defid &aux (node (id->value graph defid)))
               (when (null node) (return-from expr-vector-schedule))
               (let* ((reader-ids (map 'list #'(lambda (x) (getf x :idx)) (get-loops (gethash (node-id reader-expr) node-to-loops))))
                      (defids (map 'list #'(lambda (x) (getf x :idx)) (get-loops (gethash (node-id node) node-to-loops))))
                      (aft (loop for d in defids if (null (find d reader-ids)) collect d)))
                 (when aft
                   (list :should-seen nil :should-unseen nil :should-after aft))))
             (id-is-reduction-p (id) (gethash id id-cache))      
             (expr-depends-on (expr)
               ;; Constructs a dependency object which is used to sort nodes
               (let* ((depends-on (expr-depend-vars expr))
                      (vectors (loop for id in (expr-depend-vectors expr)
                                     collect (expr-vector-schedule expr id)))
                      (w  (expr-schedule-write expr))
                      (rs (loop for r in depends-on for s = (expr-schedule-read expr r)
                                if s collect s)))
                 (list :schedule (append rs (if w (list w)) vectors)
                       :reads depends-on
                       :conditions (loop for scp in (gethash (node-id expr) node-to-loops)
                                         if (eql (getf scp :type) :if) collect (node-id (getf scp :if-node))))))
             (satisfy-sched-p (ctx sched)
               (let ((seen (getf sched :should-seen))
                     (unseen (getf sched :should-unseen))
                     (aft (getf sched :should-after)))
                 (and
                  (every #'(lambda (x) (gethash x (%tctx-i ctx))) seen)
                  (every #'(lambda (x) (and (null (gethash x (%tctx-i ctx))) (null (gethash x (%tctx-i+1 ctx))))) unseen)
                  (every #'(lambda (x) (gethash x (%tctx-i+1 ctx))) aft))))
             (ctx-satisfies-cnd-p (ctx cnd)
               (and
                (every #'(lambda (x) (satisfy-sched-p ctx x)) (getf cnd :schedule))
                (every #'(lambda (x) (find x (%tctx-variables ctx))) (getf cnd :reads)) ;; all read vars are defined
                ;; conditons must be completely match
                (= (length (getf cnd :conditions)) (count-if #'identity (hash-table-values (%tctx-conditions ctx))) )
                (every #'(lambda (x) (gethash x (%tctx-conditions ctx))) (getf cnd :conditions)))))
      (dolist (expr exprs) ;; exprs is tpsorted
        (setf (gethash (node-id expr) expr-to-condition) (expr-depends-on expr)))
      (let ((visited (make-hash-table)) (stashed) (queue (copy-list exprs)))
        (labels ((get-ready-exprs (ctx)
                   (loop for expr in queue
                         if (ctx-satisfies-cnd-p ctx (gethash (node-id expr) expr-to-condition))
                           do (setf stashed (remove (node-id expr) stashed :key #'node-id))
                           and collect expr))
                 (expr-relocate (expr progn-to-relocate position)
                   (assert (and (eql (node-type expr) :EXPR) (eql (node-type progn-to-relocate) :PROGN)))
                   (let* ((expr-old-body (id->users graph (car (node-writes expr)))))
                     ;; (assert (>= (count :PROGN expr-old-body :key #'node-type) 1))
                     (loop for us in expr-old-body do
                       (when (eql (node-type us) :PROGN)
                         (setf (node-reads us) (remove (car (node-writes expr)) (node-reads us)))))
                     (setf (node-reads progn-to-relocate) (append (subseq (node-reads progn-to-relocate) 0 position) (node-writes expr) (subseq (node-reads progn-to-relocate) position)))))
                 (explore (ctx id &key (prgn) &aux (node (id->value graph id)))
                   (when (or (null node) (gethash (node-id node) visited)) (return-from explore))
                   (setf (gethash (node-id node) visited) t)
                   (ecase (node-type node)
                     (:FOR
                      (let ((range (id->value graph (car (node-reads node)))))
                        (assert (and range (eql (node-type range) :RANGE)))
                        (setf (gethash (getattr range :idx) (%tctx-i ctx)) t)
                        (push (getattr range :idx) (%tctx-variables ctx))
                        (explore ctx (second (node-reads node)))
                        (setf (%tctx-variables ctx) (remove (getattr range :idx) (%tctx-variables ctx))
                              (gethash (getattr range :idx) (%tctx-i ctx)) nil
                              (gethash (getattr range :idx) (%tctx-i+1 ctx)) t)))
                     (:IF
                      ;; set condition
                      (setf (gethash (node-id node) (%tctx-conditions ctx)) t)
                      (mapc #'(lambda (x) (explore ctx x)) (cdr (node-reads node)))
                      (setf (gethash (node-id node) (%tctx-conditions ctx)) nil))
                     (:PROGN
                       ;; Insert ready-for-insert exprs first until it saturates
                       (labels ((sync (offset)
                                  (loop for query = (get-ready-exprs ctx) while query do
                                    (dolist (q query)
                                      (setf queue (remove (node-id q) queue :key #'node-id))
                                      (expr-relocate q node offset) (incf offset)
                                      (setf (gethash (node-id q) visited) t)
                                      (push (car (node-writes q)) (%tctx-variables ctx))))))
                         (sync 0)
                         (mapc
                          #'(lambda (x)
                              (sync (position x (node-reads node)))
                              (explore ctx x :prgn node))
                          (node-reads node))
                         (sync (length (node-reads node)))))
                     (:DEFINE-LOCAL ;; [TODO] Allow relocating define-local?
                      (push (car (node-writes node)) (%tctx-variables ctx))
                      ) ;; [TODO] Relocate?
                     (:EXPR
                      (assert prgn)
                      (let ((cnd (gethash (node-id node) expr-to-condition)))
                        (assert cnd)
                        (if (ctx-satisfies-cnd-p ctx cnd)
                            (progn
                              (push (car (node-writes node)) (%tctx-variables ctx))
                              (setf queue (remove (node-id node) queue :key #'node-id)
                                    stashed (remove (node-id node) stashed :key #'node-id))
                              (return-from explore))
                            (progn
                              (setf (node-reads prgn) (remove (car (node-writes node)) (node-reads prgn)))
                              (push node stashed))))))))
          (let ((ctx (make-%tctx)))
            (assert (= 1 (length (graph-outputs graph))))
            (explore ctx (car (graph-outputs graph)))
            (assert (null stashed))
            graph))))))

(defsimplifier
    (ast-simplify-progn)
    ((:FOR (range (:PROGN (body))) :directive directive :band band)
     ->
     (:FOR (range body) :directive directive :band band))
    ((:IF  (condition (:PROGN (body)))) -> (:IF (condition body))))

(defun ast-apply-cse (graph)
  "Applies CSE (Common Subexpression Elimination) to the given graph.
Note that 99% of our transformation does not expect cse applied graph,
so this rule should be applied JUST BEFORE RENDERING THE FINAL CODE."
  (declare (type ASTGraph graph))
  (ast-ensure-progn graph) ;; Ensure :PROGN is inserted undernearth :FOR/:IF (required by ast-collapse-expr-tree)
  (ast-rewrite-expr-as-ssa-style graph) ;; Rewrite graph as ssa style first
  (ast-ensure-expr-is-singleton graph) ;; ensure all expr is singleton
  (ast-fixup-scope graph) ;; rewrite and fixup scopes, sort topologically
  (ast-rewrite-ssa-style-as-tree graph) ;; and then construct tree-style expr again
  (simplify-ast (simplify-ast graph))
  (ast-simplify-progn graph)) ;; simplify and that's it!
;; ~~~~ Rewriters(Verification) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-expr-graph (graph expr &key (include-expr nil) &aux (seen nil) (nodes))
  (declare (type ASTGraph graph) (type node expr))
  (assert (eql :EXPR (node-type expr)))
  (labels ((explore (id &aux (node (id->value graph id)))
             (when (or (null node) (find id seen)) (return-from explore))
             (when (eql (node-type node) :EXPR) (when include-expr (push node nodes)) (return-from explore))
             (push id seen)
             (push node nodes)
             (mapc #'explore (node-reads node))))
    (explore (car (node-reads expr))))
  (let ((g (apply #'make-graph nodes)))
    (setf (graph-outputs g) (node-reads expr))
    (->fast-graph g)))

(defun ast-make-subgraph (graph id &key (expr-depth nil) &aux (seen nil) (nodes))
  (declare (type Graph graph) (type symbol id))
  (labels ((explore (expr-count id &aux (node (id->value graph id)))
             (when (or (null node) (find id seen)) (return-from explore))
             (when (or (null expr-depth) (>= expr-count expr-depth))
               (when (eql (node-type node) :EXPR) (return-from explore)))
             (when (eql (node-type node) :EXPR) (incf expr-count))
             (push id seen)
             (push node nodes)
             (mapc #'(lambda (x) (explore expr-count x)) (node-reads node))))
    (explore 0 id))
  (let ((g (apply #'make-graph nodes)))
    (setf (graph-outputs g) (list id))
    (->fast-graph g)))

(defun ast-simplify-expr (graph &aux (seen1) (seen2))
  "The first argument of MOVE in the EXPRBlock does not use the first argument and thus removed."
  (declare (type ASTGraph graph))
  (labels ((Purge (node)
             (unless (find (car (node-writes node)) seen2)
               (push (car (node-writes node)) seen2)
               (let ((typed (car (relay-reads (read-type-relay node)))))
                 (assert typed () "ast-simplify-expr: Cannot deduce the first src of ~a" node)
                 (list (%bind (car (node-reads node)) (%empty (tensor-relay-dtype typed))) node))))
           (simplify-expr (expr &aux (expr-graph (ast-expr-graph graph expr)))
             ;; Rewriting MUL(MOVE(A, AREF(B)), C) -> MUL(AREF(B), C)
             (funcall (Simplifier () ((:MOVE (_ b)) -> b))  expr-graph)
             ;; Rewrite the path that are not rendered with Allocate.
             ;; 1. LOAD(ALLOCATE(X))
             ;; 2. TernaryOps(Allocate(_), X, Y)
             ;; 3. Cast(ALLOCATE(X), Y)
             (funcall
              (Simplifier
                  ()
                  ((:MOVE (_ _)) -> ((node graph) (Purge node)))
                  ((:LOAD   (_)) -> ((node graph) (Purge node)))
                  ((:<  (_ _ _)) -> ((node graph) (Purge node)))
                  ((:!= (_ _ _)) -> ((node graph) (Purge node)))
                  ((:Cast (_ _)) -> ((node graph) (Purge node))))
              expr-graph)
             (insert-nodes graph (graph-nodes expr-graph))
             (list expr)))
    (funcall
     (compose
      (Simplifier () ((:EXPR (id)) -> ((node graph) (unless (find id seen1) (push id seen1) (simplify-expr node)))))
      #'(lambda (x) (graph-infer-type-relay x) x))
     graph)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %simplify-ast (graph
                      &key
                        (opts
                         (list
                          #'fold-constant
                          #'fuse-duplicated-store
                          #'simplify-control-flow
                          #'exprify-ast
                          #'ast-simplify-expr
                          ;; 1. purge reduction (this will remove an extra aref etc)
                          ;; 2. exprify again
                          #'ast-simplify-constant
                          #'ast-purge-unused-expr
                          #'ast-synchronize-read-write
                          #'(lambda (x) (graph-infer-type-relay x) x))))
  "Simplifies the AST"
  (declare (type ASTGraph graph))
  (let ((g (funcall (apply #'compose (reverse opts)) graph)))
    (verify-graph g)
    g))

(defun simplify-ast (graph)
  (%simplify-ast graph :opts (list #'fold-constant #'fuse-duplicated-store #'simplify-control-flow #'ast-simplify-expr #'ast-simplify-constant #'ast-purge-unused-expr #'ast-synchronize-read-write #'(lambda (x) (graph-infer-type-relay x) x))))

(defun ast-simplify-expr-subgraph (graph &aux (simplified-subgraphs))
  (loop for node in (graph-nodes graph)
        if (eql (node-type node) :EXPR) do
          (let ((expr-graph (ast-expr-graph graph node)))
            (setf expr-graph (fold-constant (optimize-aasm expr-graph :heavy-opt-threshold 0)))
            ;;(print expr-graph)
            (push expr-graph simplified-subgraphs)))
  (loop for sb in simplified-subgraphs do
    (insert-nodes graph (graph-nodes sb)))
  graph)

(defun ast-synchronize-read-write (graph &aux (id->state (make-hash-table)) (seen (make-hash-table)))
  (labels ((e (id mode &aux (node (id->value graph id)))
             (when (null node) (return-from e))
             (when (gethash (node-id node) seen)
               (when (null (find (node-type node) `(:AREF :DEFINE-GLOBAL)))
                 (return-from e)))
             (setf (gethash (node-id node) seen) t)
             (case (node-type node)
               (:SETF
                (e (car (node-reads node)) :write)
                (e (second (node-reads node)) :read))
               (:DEFINE-GLOBAL
                (assert mode)
                (let ((state (gethash (car (node-writes node)) id->state)))
                  (if (null state)
                      (setf (gethash (car (node-writes node)) id->state) mode)
                      (unless (eql state mode)
                        (setf (gethash (car (node-writes node)) id->state) :io)))))
               (otherwise
                (mapc #'(lambda (x) (e x mode)) (node-reads node))))))
    (e (car (graph-outputs graph)) :read))
  (loop for node in (graph-nodes graph)
        if (eql (node-type node) :DEFINE-GLOBAL) do
          (let ((state (gethash (car (node-writes node)) id->state)))
            (when state (setf (getattr node :mode) state))))
  graph)

(defun graph-rewrite-setf-is-expr (graph filter)
  "
Rewrites filter subgraph to ensure value in SETF(place, value) is always an expr.
```
val_0[i] = sin(val_0[i])
```
=>
```
val_0_tmp = sin(val_0[i]) // EXPR
val_0[i] = val_0_tmp // EXPR(STORE)
```
"
  (declare (type ASTGraph graph))
  (assert (find (node-type filter) `(:EXPR :PROGN)))
  (let ((nodes (if (eql (node-type filter) :PROGN)
                   (node-reads filter)
                   (node-writes filter)))
        (id (gensym)))
    (values
     id
     (with-blueprint (:noopt t)
       (%bind
        id
        (%progn
         (loop for node in nodes
               for expr = (id->value graph node) for entry = (id->value graph (car (node-reads expr)))
               collect
               (if (and (eql (node-type entry) :SETF)
                        (let ((x (id->value graph (car (node-reads entry))))
                              (place (id->value graph (second (node-reads entry)))))
                          (and
                           (null (find (node-type place) `(:BIND :EXPR)))
                           (eql (node-type x) :AREF))))
                   (let ((tmpid (gensym)))
                     (list
                      (%expr (second (node-reads entry)) :out tmpid)
                      (%expr (node->id1 (%setf (car (node-reads entry)) tmpid)))))
                   node))))))))

(defun ast-band-collapse (graph bands &key (dtype :int64))
  "
for i in range(M):
  for j in range(N):
    for k in range(K):
      A(i, j, k)
===>
for x in range(M*N*K):
  i = x % M
  j = x / N
  k = ?
  A(i, j, k)
"
  (declare (type FastGraph graph) (type list bands))
  (when (= (length bands) 1) (return-from ast-band-collapse graph))
  (flet ((maybe-fixnum (x)
           (if (numberp x)
               (%load (%salloc :dtype dtype) x)
               (let ((node (id->value graph x)))
                 (if (and node (eql (node-type node) :EXPR))
                     (car (node-reads node))
                     x)))))
    (let* ((merged-size-out (gensym "MS"))
           (ranges (map 'list #'(lambda (x) (id->value graph (car (node-reads x)))) bands))
           (new-idx (intern (with-output-to-string (out) (dolist (r ranges) (format out "~a" (getattr r :idx))))))
           (_ (assert (every #'(lambda (x) (eql (node-type x) :RANGE)) ranges)))
           (merged-size-graph
             (with-context (_ (%expr (node->id (reduce #'%mul (map 'list #'(lambda (x) (apply #'%idiv (map 'list #'maybe-fixnum (node-reads x)))) ranges))) :out merged-size-out))))
           (merged-bands-idx (gensym))
           (merged-bands (with-context (_ (emit (make-node :Render :RANGE (list merged-bands-idx) (list merged-size-out (node->id (%expr (node->id1 (maybe-fixnum 1))))) :idx new-idx :dtype dtype)))))
           (new-body-id (gensym))
           (new-body
             (with-context
                 (_
                  (%bind
                   new-body-id
                   ;; Rewrite i, j (idx)
                   ;; stride_k = ∏_{j=k+1..n-1} D_j
                   (let* ((sizes (map 'list #'(lambda (x) (apply #'%idiv (map 'list #'maybe-fixnum (node-reads x)))) ranges))
                          (strides
                            (loop for i from 0 below (length ranges)
                                  collect
                                  (reduce #'%mul (subseq sizes (1+ i)) :initial-value (maybe-fixnum 1)))))
                     (append
                      (loop for r in ranges
                            for b in bands
                            for stride in strides
                            do (%bind (car (node-writes r))
                                      (%mul
                                       (%mod (%idiv merged-bands-idx stride)
                                             (%idiv (maybe-fixnum (car (node-reads r))) (maybe-fixnum (second (node-reads r)))))
                                       (maybe-fixnum (second (node-reads r)))))
                               (loop for node in (graph-nodes graph)
                                     if (or
                                         (and (eql (node-type node) :LOAD) (eql (getattr node :value) (getattr r :idx)))
                                         (and (eql (node-type node) :RANGE) (eql (getattr node :idx) (getattr r :idx))))
                                       do (%bind (car (node-writes node))
                                                 (%mul
                                                  (%mod (%idiv merged-bands-idx stride)
                                                        (%idiv (maybe-fixnum (car (node-reads r))) (maybe-fixnum (second (node-reads r)))))
                                                  (maybe-fixnum (second (node-reads r)))))))
                      (apply
                       #'%progn
                       (list (id->value graph (second (node-reads (car (last bands)))))))))))))
           (outerband (make-node :Render :FOR (node-writes (car bands))
                                 (list merged-bands-idx new-body-id))))
      (declare (ignore _))
      (insert-nodes graph (append (graph-nodes merged-size-graph) (graph-nodes new-body) (graph-nodes merged-bands)))
      (insert-nodes graph (list outerband))
      (simplify-ast graph)
      graph)))

(defun ast-concrete-sequence (blueprint &aux (visited (make-hash-table)))
  "Concretes the execution order such as:
```
val_1[idx] = ...;   // EXPR(STORE) OUT=A
val_2 = val_1[idx]; // ==> BIND(A, val_1)
```
so that cse won't break the blueprint."
  (declare (type FastGraph blueprint))
  (dolist (node (graph-nodes blueprint))
    (loop for r in (node-reads node) for nth upfrom 0
          for n = (id->value blueprint r)
          if (eql (node-type n) :BIND) do
            (setf (nth nth (node-reads node)) (getattr n :value))))
  (verify-graph blueprint)
  (let ((id->bind (make-hash-table)))
    (labels ((f (item)
               (loop for n in (node-reads item) for nth upfrom 0
                     for k = (gethash n id->bind)
                     if k do
                       (setf (nth nth (node-reads item)) (car (node-writes k))))
               ;; // EXPR(STORE)
               (let* ((parent (id->value blueprint (car (node-reads item))))
                      (setf/out
                        (when (and parent (eql (node-type parent) :SETF))
                          (id->value blueprint (car (node-reads parent))))))
                 (when (and
                        (eql (node-type item) :EXPR)
                        parent setf/out
                        (eql :SETF (node-type parent)))
                   (let* ((val (case (node-type setf/out)
                                 (:AREF (car (node-reads setf/out)))
                                 (:EXPR (car (node-writes setf/out)))
                                 (otherwise (error "ast-concrete-sequence: detected illegal order. SETF(X, Y), X should be AREF or EXPR."))))
                          (tmpid (gensym "BIND"))
                          (bind (make-node :JIT :BIND (list tmpid) (node-writes item) :value val)))
                     (setf (gethash val id->bind) bind)
                     (insert-nodes blueprint (list bind))))))
             (explore (id bfs &aux (node (id->value blueprint id)))
               (when (or (null node) (gethash (node-id node) visited))
                 (return-from explore))
               (setf (gethash (node-id node) visited) t)
               (if (or bfs (eql (node-type node) :EXPR))
                   (progn
                     (mapc #'(lambda (x) (explore x t)) (node-reads node))
                     (f node))
                   (progn
                     (f node)
                     (mapc #'(lambda (x) (explore x bfs)) (node-reads node))))))
      (mapc #'(lambda (x) (explore x nil)) (graph-outputs blueprint))
      (verify-graph blueprint)
      blueprint)))

(defun ast-remove-extra-memloads (blueprint singletons &aux (deleted))
  "Rewrites the following pattern but val_1 is a member of singletons.
```
idx = ai+b;
val_1[idx] = 0.0;
float acc = val_1[idx];
```
===>
```
float acc = 0.0;
```
"
  (declare (type FastGraph blueprint) (type list singletons))
  (labels ((getchild (id type)
             (let ((children (id->users blueprint id)))
               (when (and (= 1 (length children)) (eql type (node-type (car children))))
                 (car children))))
           (replace-for-id (id)
             (let* ((aref (getchild id :AREF))
                    (setf (when aref (getchild (car (node-writes aref)) :SETF)))
                    (expr (when setf (getchild (car (node-writes setf)) :EXPR)))
                    (binds (when expr (id->users blueprint (car (node-writes expr)))))
                    (bind (find :BIND binds :key #'node-type))
                    (aref-child (when (and expr bind (not (eql (node-id expr) (node-id bind))))
                                  (id->users blueprint (car (node-writes bind)))))
                    (aref-child (when (and aref-child (= 1 (length aref-child)))
                                  (car aref-child))))
               ;; [TODO] Assert aref-child.reads[1] == aref.reads[1]
               (when (and aref-child)
                 ;; remove expr
                 (push (car (node-writes expr)) deleted)
                 (dolist (usr (id->users blueprint (car (node-writes aref-child))))
                   (setf (node-reads usr)
                         (loop for r in (node-reads usr)
                               if (eql r (car (node-writes aref-child)))
                                 collect (second (node-reads setf))
                               else
                                 collect r)))))))
    (mapc #'replace-for-id singletons)
    (dolist (node (graph-nodes blueprint))
      (when (eql (node-type node) :PROGN)
        (setf (node-reads node)
              (loop for r in (node-reads node)
                    if (null (find r deleted))
                      collect r))))
    (verify-graph blueprint)
    blueprint))

(defun ast-merge-expr-from-aref-subgraph (blueprint &aux (seen (make-hash-table)))
  (declare (type FastGraph blueprint))
  (labels ((explore (id is-aref-subgraph &aux (node (id->value blueprint id)))
             (when (or (null node) (gethash (node-id node) seen))
               (when (null is-aref-subgraph)
                 (return-from explore)))
             (setf (gethash (node-id node) seen) t)
             (when (or is-aref-subgraph (eql (node-type node) :AREF))
               (let ((new-reads
                       (loop for r in (node-reads node)
                             for v = (id->value blueprint r)
                             if (and v (eql (node-type v) :EXPR))
                               collect (car (node-reads v))
                             else
                               collect r)))
                 (setf (node-reads node) new-reads)))
             (if (eql (node-type node) :AREF)
                 (progn
                   (assert (null is-aref-subgraph))
                   (explore (nth 0 (node-reads node)) nil)
                   (explore (nth 1 (node-reads node)) t))
                 (mapc #'(lambda (x) (explore x is-aref-subgraph)) (node-reads node)))))
    (mapc #'(lambda (x) (explore x nil)) (graph-outputs blueprint))
    (verify-graph blueprint)
    (%simplify-ast blueprint)))
