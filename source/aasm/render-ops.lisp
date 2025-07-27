;;;; render-ops.lisp
;;;; This file includes the following features which is required to generate the optimized code.
;;;; - ASTGraph Creation
;;;; - ASTGraph Simplification
;;;; - ASTGraph Optimization (e.g.: Tile, Unroll, Microkernel, etc)
(in-package :caten/aasm)

(defmacro with-blueprint ((&key (noopt nil)) &body body)
  `(let* ((*ctx* (make-graph))
          (out (progn ,@body)))
     (assert (node-p out) () "The last form must be a node.")
     (setf (graph-outputs *ctx*) (node-writes out))
     (let ((graph (->fast-graph *ctx*)))
       (unless ,noopt (setf graph (%simplify-ast graph)))
       graph)))
;; ~~ Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %expr (name &key (out (gensym "EXPR"))) (emit (make-node :Render :EXPR (list out) (list (node->id1 name)))))

(defun %range (bind size body &key (step 1) (dtype *default-int*) (out (gensym "RANGE")) (mark :noopt) (range) (rid bind))
  "
```
(%range bind size body &key (step 1) (dtype *default-int*) (out (gensym \"RANGE\")) (mark :noopt))
```
Constraints:
- SIZE/STEP is always an EXPR, that is, must not include an control flow.
"
  (declare (type symbol bind) (type (or node symbol) body) (type (or symbol node fixnum) size step) (type keyword dtype) (type symbol out) (type (member :coincident :noopt :reduction) mark))
  (when (node-p size) (setf size (%expr (node->id1 size))))
  (when (node-p step) (setf step (%expr (node->id1 step))))
  (let ((range (or range (emit (make-node :Render :RANGE (list rid) (map 'list #'node->id1 (list size step)) :idx bind :dtype dtype)))))
    (emit (make-node :Render :FOR (list out) (map 'list #'node->id1 (list range body)) :mark mark))))

(defmacro %dotimes ((bind size &key (mark :noopt) (id (gensym "RANGE")) (range)) &body body)
  `(let ((,bind ',bind)) (%range ',bind ,size (%progn ,@body) :mark ,mark :out ',id :range ,range)))

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

(defun %global (name dtype pointer-p &key (mode :io))
  (declare (type dtype-t dtype) (type boolean pointer-p) (type symbol name))
  (emit (make-node :Render :DEFINE-GLOBAL (list name) nil :dtype dtype :pointer-p pointer-p :mode mode)))

(defun %local (name size dtype)
  (declare (type dtype-t dtype) (type list size) (type symbol name))
  (emit (make-node :Render :DEFINE-LOCAL (list name) size :dtype dtype)))

(defun %swizzle (name indices &key (out (gensym "SWIZZLE")))
  "name[indices[0]][indices[1]][...]"
  (declare (type symbol name) (type list indices))
  (let ((indices (map 'list #'node->id1 indices)))
    (emit (make-node :Render :SWIZZLE (list out) (append (list name) indices)))))

(defun %vector (from shape indices &key (out (gensym "VECTOR")))
  (declare (type symbol from) (type list indices))
  (emit (make-node :JIT :VECTOR (list out) (append (list from) (flatten indices)) :shape shape)))

(defun %barrier (&key (out (gensym "BARRIER"))) (emit (make-node :Render :BARRIER (list out) nil)))

(defun %defsmem (&key (size `(4)) (dtype *default-float*) (out (gensym "SMEM")))
  (emit (make-node :Render :DEFINE-SHARED-MEMORY (list out) nil :size size :dtype dtype)))

(defun %bind (name node)
  (declare (type symbol name) (type node node))
  (assert (= 1 (length (node-writes node))) () "%bind: The node must have exactly one read.")
  (setf (node-writes node) (list name))
  node)

(defun %aref (name idx &key (out (gensym "AREF")))
  (declare (type (or symbol node) name idx))
  (emit (make-node :JIT :Aref (list out) (map 'list #'node->id1 (list name idx)))))

(defun %setf (tgt value &key (out (gensym "SETF")))
  (declare (type (or symbol node) tgt value))
  (emit (make-node :JIT :SETF (list out) (map 'list #'node->id1 (list tgt value)))))

(defun %pack (x indices &key (out (gensym)) (contiguous nil))
  (declare (type list indices))
  (emit (make-node :JIT :PACK (list out) (append (list (node->id1 x)) (map 'list #'node->id1 indices)) :contiguous contiguous)))

(defun %unpack (x idx &aux (out (gensym)))
  (declare (type fixnum idx))
  (emit (make-node :JIT :Unpack (list out) (list (node->id1 x) idx))))

(defun %empty (dtype) (make-node :Buffer :Allocate (list (gensym)) nil :dtype dtype :nrank 0))

(defun %gid (rank graph range local-size &key (dtype :int64) (id (gensym "G")))
  (let* ((loop-size
           (if (numberp (car (node-reads range)))
               (caten/aasm/expr:expr-const (car (node-reads range)) :float32)
               (let ((expr (id->value graph (car (node-reads range)))))
                 (assert (and expr (eql (node-type expr) :EXPR)) () "%gid: The parent for Range must be fixnum or EXPR.")
                 (caten/aasm/expr:make-expr
                  :graph (ast-descendants-graph graph (map 'list #'(lambda (x) (id->value graph x)) (node-reads expr)))
                  :out (id->value graph (car (node-reads expr)))))))
         (loop-size (caten/aasm/expr:expr-cast loop-size :float32))
         (local-size (caten/aasm/expr:expr-const local-size :float32))
         (size (caten/aasm/expr:expr-ceiling (caten/aasm/expr:expr-div loop-size local-size) dtype)))
    (emit (make-node :JIT :SPACE (list id) nil :level :block :rank rank :dtype dtype :size size))))

(defun %lid (rank size &key (dtype :int64) (id (gensym "L")))
  (emit (make-node :JIT :SPACE (list id) nil :level :thread :rank rank :dtype dtype :size
                   (if (caten/aasm/expr::expr-p size) size (caten/aasm/expr:expr-const size dtype)))))

(defun %function (name body &key (id (gensym "OUT")))
  (emit (make-node :Runtime :FUNCTION (list id) (list (node->id1 body)) :name name)))
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
  (declare (type FastGraph graph))
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
  (declare (type FastGraph graph) (optimize (speed 3)))
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
  (declare (type FastGraph graph) (type list outputs))
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
  (declare (type FastGraph dg))
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
  (declare (type FastGraph dg) (type hash-table sink-map))
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
  (declare (type FastGraph graph) (optimize (speed 3)) (type list seen))
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
;; [TODO] expr-prognify, tpsort-progn, reexprify ===> 共通項削除！
;; [TODO] %progn-sortだけ切り出しておく
;; [TODO] For LoopのIndexを考慮して，早めに切り出しておく。。。
(defun ast-ensure-progn (graph &aux (visited (make-hash-table)))
  "Inserts extra PROGN undernearth :FOR and :IF"
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
                                        (whitelist `(:DEFINE-GLOBAL :DEFINE-LOCAL :ALLOCATE :RANGE :LOAD :BIND :SETF)))
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
  (declare (type FastGraph graph))
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
               ((:IF :FOR) (mapc #'(lambda (x) (explore x :expr-subgraph-p expr-subgraph-p :scope scope)) (cdr (node-reads node))))
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
  (declare (type FastGraph graph) (optimize (speed 3)))
  (labels ((expr-search-key (expr)
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
                 (setf (gethash (node-id expr) ecache) (e (car (node-writes c)))))))
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
        (if changed-p (ast-ensure-expr-is-singleton graph) graph)))))

(defun %make-parse-ctx (graph)
  (let* ((ctx (uiop:symbol-call :caten/codegen/polyhedral :make-scop-ctx-from-blueprint graph :allow-if t))
         (node-to-loops (uiop:symbol-call :caten/codegen/polyhedral :ctx-node-to-loops ctx))
         (allloops (uiop:symbol-call :caten/codegen/polyhedral :ctx-all-loops ctx))
         (exprs (uiop:symbol-call :caten/codegen/polyhedral :ctx-exprs ctx)))
    (values node-to-loops allloops exprs)))

(defun ast-rewrite-ssa-style-as-tree (graph)
  (declare (type FastGraph graph))
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
                            (:AREF
                             ;; val_9[xx] = ...
                             ;; val_2 = val_9[xx] ...
                             ;; [TODO] FlashAttention, Softmax Patch is needed
                             ;; [TODO] Extra deps i am missing?
                             (mapc #'e (node-reads node)))
                            (:EXPR  (push (car (node-writes node)) ids))
                            (:RANGE (push (getattr node :idx) ids))
                            (:LOAD  (when (find (getattr node :value) range-ids) (push (getattr node :value) ids)))
                            (:BIND (mapc #'e (node-reads node)))
                            (otherwise (mapc #'e (node-reads node))))))
                 (e (car (node-reads expr))))
               (remove-duplicates ids))
             (get-loops (ls) (loop for l in ls if (eql :loop (getf l :type)) collect l))
             (expr-schedule-write (expr)
               ;; Reductionを定義するとき
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
               ;; ReductionされたVariableを使う時
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
             (id-is-reduction-p (id) (gethash id id-cache))      
             (expr-depends-on (expr)
               (let* ((depends-on (expr-depend-vars expr))
                      (w  (expr-schedule-write expr))
                      (rs (loop for r in depends-on for s = (expr-schedule-read expr r)
                                if s collect s)))
                 (list :schedule (append rs (if w (list w)))
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
    ((:FOR (range (:PROGN (body))) :mark mark :directive directive :band band :parallel parallel)
     ->
     (:FOR (range body) :mark mark :directive directive :band band :parallel parallel))
    ((:IF  (condition (:PROGN (body)))) -> (:IF (condition body))))

(defun ast-apply-cse (graph)
  "Applies CSE (Common Subexpression Elimination) to the given graph.
Note that 99% of our transformation does not expect cse applied graph,
so this rule should be applied JUST BEFORE RENDERING THE FINAL CODE."
  (ast-ensure-progn graph) ;; Ensure :PROGN is inserted undernearth :FOR/:IF (required by ast-collapse-expr-tree)
  (ast-rewrite-expr-as-ssa-style graph) ;; Rewrite graph as ssa style first
  (ast-ensure-expr-is-singleton graph) ;; ensure all expr is singleton
  (ast-fixup-scope graph) ;; rewrite and fixup scopes, sort topologically
  (ast-rewrite-ssa-style-as-tree graph) ;; and then construct tree-style expr again
  (simplify-ast graph)
  (ast-simplify-progn graph)) ;; simplify and that's it!
;; ~~~~ Rewriters(Verification) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-expr-graph (graph expr &key (include-expr nil) &aux (seen nil) (nodes))
  (declare (type FastGraph graph) (type node expr))
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
  (declare (type FastGraph graph))
  (labels ((Purge (node)
             (unless (find (car (node-writes node)) seen2)
               (push (car (node-writes node)) seen2)
               (let ((typed (car (relay-reads (read-type-relay node)))))
                 (assert typed () "ast-simplify-expr: Cannot deduce the first src of ~a" node)
                 (list (%bind (car (node-reads node)) (%empty (tensor-relay-dtype typed))) node))))
           (simplify-expr (expr &aux (expr-graph (ast-expr-graph graph expr)))
             ;; Rewriting MUL(MOVE(A, AREF(B)), C) -> MUL(AREF(B), C)
             (funcall (Simplifier () ((:MOVE (_ b)) -> b))  expr-graph)
             (funcall (Simplifier () ((:STORE (_ b)) -> b)) expr-graph)
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
                          #'(lambda (x) (graph-infer-type-relay x) x))))
  "Simplifies the AST"
  (declare (type FastGraph graph))
  (let ((g (funcall (apply #'compose (reverse opts)) graph)))
    (verify-graph g)
    g))

(defun simplify-ast (graph)
  (%simplify-ast graph :opts (list #'fold-constant #'fuse-duplicated-store #'simplify-control-flow #'ast-simplify-expr #'ast-simplify-constant #'ast-purge-unused-expr #'(lambda (x) (graph-infer-type-relay x) x))))

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
;; ~~ Scheduling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Tiling
(defun ngid (gid suffix) (intern (format nil "~a~a" gid suffix)))
(defun %ast-band-tile (graph band tile-sizes &key (sp "_p") (sc "_c") (cid (gensym "C")) &aux (bands) (globals) (locals))
  "Tiles the band:
```
for (int i=0; i<M; i++) Instance(i)
```
===>
```
for (int i=0; i<M; i+=32)
  for (int ii=0; ii<min(M - i, 32); ii++)
    Instance(i + ii)
```"
  (declare (type FastGraph graph) (type node band) (type list tile-sizes))
  (assert (eql (node-type band) :FOR) () "%ast-tile-band: band must be :FOR, getting ~a" band)
  (assert (>= (length tile-sizes) 1) () "TileSizes must be larger than 1.")
  (when (null (getattr band :band)) (assert (= 1 (length tile-sizes)) () "tile-size must be one for non-coincidence band."))
  (push band bands)
  (labels ((explore (id band-depth)
             (when (>= band-depth (length tile-sizes))
               (return-from explore))
             (let ((node (id->value graph id)))
               (assert (and node (eql (node-type node) :FOR)) () "%ast-tile-band: The given tile-sizes ~a is too large (<= ~a)" tile-sizes band-depth)
               (assert (eql (getattr node :band) (getattr (car bands) :band)) () "%ast-tile-band: ~a and ~a are not coincident." node (car bands))
               (push node bands)
               (explore (second (node-reads node)) (1+ band-depth)))))
    (explore (second (node-reads band)) 1))
  (assert (= (length bands) (length tile-sizes)) () "tile-sizes and band-depth should correspond.")
  (setf bands (nreverse bands))
  (flet ((g (obj)
           (if (numberp obj)
               (%iconst obj :dtype :int64)
               (let ((val (id->value graph obj)))
                 (if (and val (eql (node-type val) :EXPR))
                     (car (node-reads val))
                     obj)))))
    ;; 1. Create (and supercede w/) new tiled range.
    (loop for band in bands
          for tile-size in tile-sizes
          for idx = (car (node-reads band))
          for range = (id->value graph idx)
          for gid = (getattr range :idx)
          for dtype = (getattr range :dtype)
          for new-step-id = (gensym "NEWSTEP")
          for new-step-graph = (with-context (_ (%mul (g tile-size) (g (second (node-reads range))) :id new-step-id)))
          for new-step-expr = (%expr new-step-id)
          for range-size-id = (gensym "TILEBOUND")
          for range-size-graph = (with-context (out (%expr (node->id1 (%min (%sub (g (car (node-reads range))) (ngid idx sp)) (g tile-size))) :out range-size-id)))
          for range-parent = (make-node :Render :Range (list (ngid idx sp)) (list (car (node-reads range)) (node->id1 new-step-expr)) :idx (ngid gid sp) :dtype dtype)
          for range-child = (make-node :Render :Range (list (ngid idx sc)) (list range-size-id 1) :idx (ngid gid sc) :dtype dtype)
          do (insert-nodes graph (list new-step-expr range-parent range-child))
             (insert-nodes graph (graph-nodes new-step-graph))
             (insert-nodes graph (graph-nodes range-size-graph)))
    ;; 2. Create new FOR
    (let ((next-write-to (car (node-writes (car bands)))))
      ;; Insert Parents, and then children
      (dolist (prefix (list sp sc))
        (loop for band in bands
              for idx = (car (node-reads band))
              for range = (id->value graph idx)
              for prev-body = (gensym "T")
              for new-band = (if (equal prefix sp) (getattr band :band) (ngid (getattr band :band) cid))
              for new-for = (make-node :Render :FOR (list next-write-to) (list (ngid idx prefix) prev-body)
                                       :mark (getattr band :mark) :band new-band)
              do (insert-nodes graph (list new-for))
                 (if (eql prefix sp) (push new-for globals) (push new-for locals))
                 (setf next-write-to prev-body)))
      ;; Finally insert the body to next-write-to
      (let* ((innermost (car (last bands)))
             (body (copy-node (id->value graph (second (node-reads innermost))))))
        (assert (= (length (node-writes body)) 1))
        (setf (node-writes body) (copy-list (node-writes body))
              (node-writes body) (list next-write-to))
        (insert-nodes graph (list body))))
    ;; 3. Replace the access to i -> i + ii
    (loop for band in bands
          for rng-old = (id->value graph (car (node-reads band)))
          for idx = (car (node-reads band))
          for idx-new = (%add (ngid idx sp) (ngid idx sc) :id idx)
          do (insert-nodes graph (list idx-new))
             (loop for node in (graph-nodes graph)
                   if (or
                       (and (eql (node-type node) :RANGE) (eql (getattr node :idx) idx))
                       (when (eql (node-type rng-old) :RANGE)
                         (and (eql (node-type node) :RANGE) (eql (getattr node :idx) (getattr rng-old :idx))))
                       (when (eql (node-type rng-old) :RANGE)
                         (and (eql (node-type node) :LOAD) (eql (getattr node :value) (getattr rng-old :idx)))))
                     do (let ((idx-new (%add (ngid idx sp) (ngid idx sc) :id (car (node-writes node)))))
                          (insert-nodes graph (list idx-new)))))
    (values graph (nreverse globals) (nreverse locals))))

(defun ast-band-tile-gpu (graph band local-sizes)
  "Tiles the given band and map them into gpu with local-sizes."
  (flet ((reveal-expr (x)
           (if (numberp x)
               x
               (let ((expr (id->value graph x)))
                 (assert (and expr (eql (node-type expr) :EXPR)) () "Expecting EXPR, getting ~a" expr)
                 (car (node-reads expr))))))
    (let ((loop-sizes))
      (multiple-value-bind (graph block-bands thread-bands) (%ast-band-tile graph band local-sizes)
        (loop for block-band in block-bands
              for thread-band in thread-bands
              for size in local-sizes
              for level upfrom 0
              for range = (id->value graph (car (node-reads block-band)))
              for trange = (id->value graph (car (node-reads thread-band)))
              for body = (id->value graph (second (node-reads block-band)))
              do (push range loop-sizes)
                 (insert-nodes
                  graph
                  (with-context-nodes
                      (out
                       (%bind
                        (car (node-writes block-band))
                        (%progn
                         (%bind
                          (car (node-writes range))
                          (%expr (node->id1 (%mul (reveal-expr (second (node-reads range))) (%gid level graph range size)))))
                         (%bind (car (node-writes trange)) (%expr (node->id1 (%lid level size))))
                         body))))))
        (loop for grid-band in thread-bands
              for band-size in (reverse loop-sizes)
              for block-band in block-bands
              for range = (id->value graph (car (node-reads grid-band)))
              for body = (id->value graph (second (node-reads grid-band)))
              for level upfrom 0
              for size in local-sizes
              do (insert-nodes
                  graph
                  (with-context-nodes
                      (out
                       (%bind
                        (car (node-writes grid-band))
                        (%if (%< nil :row (%add (car (node-writes range)) (car (node-writes (id->value graph (car (node-reads block-band)))))) (reveal-expr (car (node-reads band-size)))) body))))))
        (verify-graph graph)
        graph))))
;; Unrolling, Upcast, Vectorize, TensorCore
(defgeneric compute-unroll-reminder (reminder size step n-unroll)
  (:documentation "Finds the maximum integer which satisfies MOD(SIZE//STEP, n_unroll) == 0"))

(defmethod compute-unroll-reminder ((reminder (eql :idiv)) size step n-unroll)
  (let* ((id (gensym))
         (g (with-context (out (%mul step (%mul n-unroll (%idiv (%idiv size step) n-unroll)) :id id)))))
    (setf (graph-outputs g) (list id))
    g))

(defun node-force-number-bypass (node)
  "Inserts %LOAD if the node is trying to load number directly"
  (when (eql (node-type node) :RANGE) (return-from node-force-number-bypass (list node)))
  (with-context-nodes
      (_
       (loop for r in (node-reads node)
             for nth upfrom 0
             if (integerp r) do (setf (nth nth (node-reads node)) (node->id1 (%iconst r)))
             else if (floatp r) do (setf (nth nth (node-reads node)) (node->id1 (%fconst r)))))
      (__ (emit node))))

(defun ast-band-children (graph band &key (nodes nil) (seen nil))
  (labels ((explore (id seen-expr-p &aux (node (id->value graph id)))
             (when (or (null node) (find id seen)) (return-from explore))
             (push id seen)
             (when (eql (node-type node) :EXPR)
               (if seen-expr-p (return-from explore) (setf seen-expr-p t)))
             (push node nodes)
             (mapc #'(lambda (x) (explore x seen-expr-p)) (node-reads node))))
    (explore (second (node-reads band)) nil))
  nodes)

(defun ast-unroll-reminder (graph reminder idx offset)
  "Rewrites the IDX -> IDX+UNROLL_OFFSET"
  (let ((nodes (apply #'make-graph (ast-band-children graph reminder)))
        (out (second (node-reads reminder))))
    (setf (graph-outputs nodes) (list out)
          nodes (graph-nodes (->graph-with-tpsort (->fast-graph nodes)))
          nodes (loop for node in nodes unless (eql (node-type node) :RANGE) collect node))
    (labels ((%cpy-node (node)
               (let ((node (copy-node node)))
                 (setf (node-id node) (gensym "NID"))
                 node))
             (newid (id &aux (node (id->value graph id)))
               (if (and id (eql (node-type node) :RANGE) (eql idx (getattr node :idx)))
                   offset
                   (if (eql id idx) offset id)))
             (clone-graph ()
               (with-context
                   (_ (loop for node_ in nodes for node = (%cpy-node node_)
                            do (setf (node-reads node) (map 'list #'newid (node-reads node))
                                     (node-writes node) (map 'list #'newid (node-writes node)))
                               (emit node))))))
      (insert-nodes graph (graph-nodes (clone-graph)))
      (%progn out))))

(defun filter-extract-load/acc/alu/store (graph filter)
  (declare (type FastGraph graph) (type node filter))
  (let ((stores) (loads) (alus) (accs) (seen (make-hash-table)))
    (labels ((e (id expr-depth &aux (node (id->value graph id)))
               (when (null node) (return-from e))
               (when (gethash (node-id node) seen) (return-from e))
               (setf (gethash (node-id node) seen) t)
               (case (node-type node)
                 (:EXPR
                  (incf expr-depth)
                  (when (> expr-depth 1) (return-from e))
                  (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                      (push node alus)
                      (push node accs))
                  (e (car (node-reads node)) expr-depth))
                 (:AREF (push node loads))
                 (:SETF
                  (let ((place (id->value graph (car (node-reads node)))))
                    (when (and place (eql :AREF (node-type place)))
                      (push node stores)))
                  (e (second (node-reads node)) expr-depth))
                 (otherwise
                  (mapc #'(lambda (x) (e x expr-depth)) (node-reads node))))))
      (e (car (node-writes filter)) 0))
    (values loads accs alus stores)))

(defun %clone-node (node)
  (let ((copy (copy-node node)))
    (setf (node-id copy) (gensym "NID")
          (node-reads copy) (copy-list (node-reads copy))
          (node-writes copy) (copy-list (node-writes copy)))
    copy))

(defun ast-rewrite-and-clone (graph id filter &aux (seen (make-hash-table)))
  "Filter: A lambda function which returns a node, filter(cloned_node, new_reads)"
  (labels ((e (id expr-depth &aux (node (id->value graph id)))
             (when (null node) (return-from e id))
             (when (gethash id seen) (return-from e (gethash id seen)))
             (when (eql (node-type node) :EXPR) (incf expr-depth))
             (when (> expr-depth 1) (return-from e (gethash id seen node)))
             (when (eql (node-type node) :DEFINE-GLOBAL) (return-from e node))
             (when (eql (node-type node) :DEFINE-LOCAL) (return-from e node))
             (let* ((new-reads (map 'list #'(lambda (x) (e x expr-depth)) (node-reads node)))
                    (c (%clone-node node))
                    (_ (setf (node-reads c) (map 'list #'(lambda (x) (if (node-p x) (car (node-writes x)) x)) new-reads)))
                    (new-node (funcall filter c new-reads))
                    (newid (gensym)))
               (declare (ignore _))
               (assert (node-p new-node))
               (assert (= 1 (length (node-writes new-node))))
               (setf (gethash (car (node-writes new-node)) seen) new-node
                     (gethash newid seen) new-node
                     (node-writes new-node) (list newid))
               (emit new-node)
               (return-from e new-node))))
    (car (node-writes (e id 0)))))

(defun aref-depends-on (graph aref &aux (seen (make-hash-table)) (deps))
  (labels ((e (id &aux (node (id->value graph id)))
             (when (or (null node) (gethash (node-id node) seen)) (return-from e))
             (setf (gethash (node-id node) seen) t)
             (case (node-type node)
               (:LOAD (when (symbolp (getattr node :value)) (push (getattr node :value) deps)))
               (:RANGE (push (getattr node :idx) deps))
               (:EXPR)
               (otherwise (mapc #'e (node-reads node))))))
    (assert (eql (node-type aref) :AREF))
    (e (second (node-reads aref))))
  (remove-duplicates deps))

(defstruct (Vectorized)
  (name nil :type symbol)
  (node nil :type (or null Node))
  (space nil :type list)
  (gids nil :type list)
  (block-size nil :type list)
  (bind-to nil :type (or Null node)))

(defun make-vectorized-form (graph band vectorize-context filter-rewriter &key (dtype :int64) (is-reminder-p t) (suffix "_1") &aux (seen (make-hash-table)))
  "is-reminder-p: If set to T, uses max(...) as a loop bound, otherwise, replaced w/ vectorize amount."
  (declare (type (or null vectorized) vectorize-context))
  (labels ((e (id gids &key (size) &aux (node (id->value graph id)))
             (when (null node) (return-from e node))
             (when (gethash (node-id node) seen) (return-from e (gethash (node-id node) seen)))
             (ecase (node-type node)
               ((:PROGN :EXPR) ;; Filter
                (let ((newnode (funcall filter-rewriter gids (map 'list #'(lambda (x) (id->value *ctx* x)) gids) seen)))
                  (emit newnode)
                  (setf (gethash (node-id node) seen) (car (node-writes newnode)))))
               (:RANGE
                   (assert size)
                   (let* ((cpy (%clone-node node)) (newid (ngid (car (node-writes cpy)) suffix)))
                     (when (null is-reminder-p) (setf (car (node-reads cpy)) (node->id1 (%expr (%load (%salloc :dtype dtype) size)))))
                     (setf (getattr cpy :idx) (ngid (getattr cpy :idx) suffix)
                           (node-writes cpy) (list newid))
                     (emit cpy)
                     (setf (gethash (node-id node) seen) newid)))
               (:FOR
                (let* ((cpy (%clone-node node)) (newid (ngid (car (node-writes node)) suffix))
                       (n1 (e (car (node-reads cpy)) nil :size (uiop:symbol-call :caten/codegen/polyhedral :directive-amount (getattr cpy :directive))))
                       (n1-range (id->value graph (car (node-reads cpy))))
                       (band-is-used-p (or (null vectorize-context) (find (getattr n1-range :idx) (vectorized-gids vectorize-context))))
                       (new-gids (if band-is-used-p
                                     (append gids (list n1))
                                     gids)))
                  (setf (node-writes cpy) (list newid)
                        (node-reads cpy) (list n1 (e (second (node-reads cpy)) new-gids)))
                  (emit cpy)
                  (if band-is-used-p
                      (setf (gethash (node-id node) seen) newid)
                      (setf (gethash (node-id node) seen) (second (node-reads cpy)))))))))
    (e (car (node-writes band)) nil)))

(defun make-space-from-bands (vectorized gids spaces)
  (declare (type vectorized vectorized) (type list gids) (type list spaces))
  (map
   'list
   #'(lambda (gid width)
       (if (find gid (vectorized-gids vectorized))
           (loop for i from 0 below width collect i)
           (make-list width :initial-element 0)))
   gids spaces))

(defun %vector-from-vectorized (vectorized)
  ;; [TODO] GIDS
  (%vector
   (node->id1 (emit (make-node :JIT :BIND (list (gensym)) (node-writes (vectorized-bind-to vectorized))
                               :value (vectorized-name vectorized))))
   (vectorized-block-size vectorized)
   (make-space-from-bands vectorized (vectorized-gids vectorized) (vectorized-block-size vectorized))))

(defun ast-vectorize-alu (graph alu ctx &aux (seen (make-hash-table)))
  "
VectorizeContext
acc | acc_acc[_gid_p3][_gid_p4]
X   | X[_gid_p4][_gid_p5]
Y   | Y[_gid_p3][_gid_p5]
===>
@VECTORIZE(4)  for (int _gid_p3_1_vload=0; _gid_p3_1_vload<4; _gid_p3_1_vload+=1)  [B1]
  @VECTORIZE(4)  for (int _gid_p4_1_vload=0; _gid_p4_1_vload<4; _gid_p4_1_vload+=1)  [B1]
    @VECTORIZE(4)  for (int _gid_p5_vload=0; _gid_p5_vload<4; _gid_p5_vload+=1)  [B2]
      acc_acc[_gid_p3][_gid_p4] = acc_acc[_gid_p3][_gid_p4] + X[_gid_p4][_gid_p5] * Y[_gid_3][_gid_p5]
Later rewritten as unrolling using VECTOR, Finally, the form will be rewritten as:
VECTOR(acc_acc, {0, 1, 2, 3}, {0, 1, 2, 3}, {0, 0, 0, 0}) +=
  VECTOR(X    , {0, 0, 0, 0}, {0, 1, 2, 3}, {0, 1, 2, 3}) *
  VECTOR(Y    , {0, 1, 2, 3}, {0, 0, 0, 0}, {0, 1, 2, 3})
If PatternMatcher detects this access pattern, this can be further rewritten as TensorCore or SIMD otherwise unrolled"
  (declare (type hash-table ctx) (type node alu) (type graph graph))
  (flet ((rewrite (node)
           (case (node-type node)
             (:EXPR
              (let ((vec (gethash (car (node-writes node)) ctx)))
                (if vec
                    (%vector-from-vectorized vec)
                    node)))
             (:AREF
              (let ((vec (gethash (car (node-reads node)) ctx)))
                (if vec
                    (%vector-from-vectorized vec)
                    node)))
             (:BIND
                 (let ((vec (gethash (getattr node :value) ctx)))
                   (if vec
                       (%vector-from-vectorized vec)
                       node)))
             (otherwise
              node))))
    (ast-rewrite-and-clone
     graph
     (car (node-writes alu))
     #'(lambda (node new-reads)
         (let ((new-reads (map 'list #'rewrite new-reads)))
           (setf (node-reads node) (map 'list #'node->id1 new-reads))
           (let ((prev (id->value *ctx* (car (node-reads node)))))
             ;; Updates SETF and BIND dependencies trigger w/ EXPR+SETF
             ;; Assumes VECTOR is expanded w/ following structure:
             ;; - VECTOR(BIND(SOME_ID, CTX_KEY), ...)
             (when (and (eql (node-type node) :EXPR) prev (eql (node-type prev) :SETF))
               (let* ((prev-reads (map 'list #'(lambda (x) (id->value *ctx* x)) (node-reads prev)))
                      (vector (car prev-reads))
                      (bind   (when (and vector (eql :VECTOR (node-type vector))) (id->value *ctx* (car (node-reads vector)))))
                      (key    (when (and bind (eql :BIND (node-type bind))) (getattr bind :value)))
                      (vectorized (find key (hash-table-values ctx) :key #'vectorized-name)))
                 (when (and key vectorized)
                   ;; Just updating bind-to is ok as long as you are using %vector-from-vectorized
                   (setf (vectorized-bind-to vectorized) node)))))
           node)))))
;; (defun ast-unroll-vector (graph id)) [TODO]
(defun ensure-setf (ctx setf)
  (setf (vectorized-bind-to ctx) setf)
  (node->id1 setf))

(defun ast-band-vectorize (graph band &key (dtype :int64) (vectorize-context (make-hash-table)))
  "
```
@VECTORIZE for (...) <--- band
  @VECTORIZE for (...)
    @VECTORIZE for (...)
      PROGN(EXPR(...))
```
===>
a = EXPR(DEFINE_FLOAT_8x8)
b = EXPR(DEFINE_SHARED_MEMORY_8x8)
dom = min(10, _gid_p0)
if (dom==10) // Full Tile or not?
  {
  // LOAD
  for (... < 4);
    for (... < 4);
      for (... < 4);
        a[...] = ;
  // COMPUTE (ALU or STORE)
  VECTORIZED_COMPUTATION;
} else {
  // LOAD
  for (... < min(4, _gid_0_p))
    for (... < min(4, _gid_1_p))
      for (... < min(4, _gid_2_p))
        b[...] = ;
  // COMPUTE (ALU or STORE)
  for (... < min(4, _gid_0_p))
    for (... < min(4, _gid_1_p))
      for (... < min(4, _gid_2_p))
        ...;
}
```
^ TpSortでIf融合できると嬉しい。
^ ReminderはあとでUnrollする。
(values graph fail_reason)"
  (let ((seen (make-hash-table)) (bands) (filter))
    (labels ((explore (id &aux (node (id->value graph id)))
               (when (or (null node) (gethash (node-id node) seen)) (return-from explore))
               (when (and (eql (node-type node) :FOR)
                          (or (null (getattr node :directive))
                              (not (equalp "VECTORIZE" (uiop:symbol-call :caten/codegen/polyhedral :directive-type (getattr node :directive))))))
                 (return-from ast-band-vectorize (values graph "Failed to vectorize")))
               (case (node-type node)
                 (:PROGN
                   (let ((reads (map 'list #'(lambda (x) (id->value graph x)) (node-reads node))))
                     (if (every #'(lambda (x) (eql (node-type x) :EXPR)) reads)
                         (progn
                           (assert (null filter)) ;;(when filter (return-from ast-band-vectorize (values graph "Multiple filter detected")))
                           (setf filter node))
                         (return-from ast-band-vectorize (values graph "PROGN must be a list of EXPR")))))
                 (:EXPR
                  (assert (null filter)) ;; (when filter (return-from ast-band-vectorize (values graph "Multiple filter detected")))
                  (setf filter node))
                 (:FOR
                  (push node bands)
                  (explore (second (node-reads node))))
                 (otherwise (return-from ast-band-vectorize (values graph (format nil "The node ~a is not supported" (node-type node))))))))
      (explore (car (node-writes band))))
    (setf bands (reverse bands))
    (multiple-value-bind (loads accs alus stores) (filter-extract-load/acc/alu/store graph filter)
      (print bands)
      (print filter)
      
      (PRINT "++++++++++++++")
      (print "LOADS")
      (print loads)
      (PRINT "ACCS")
      (print accs)
      (PRINT "ALUS")
      (print alus)
      (PRINT "STORES")
      (print stores)
      (let ((vectorize-space (map 'list #'(lambda (x) (uiop:symbol-call :caten/codegen/polyhedral :directive-amount (getattr x :directive))) bands)))
        ;; Update Contexts
        (flet ((node->gid (node)
                 (let ((range (id->value graph (car (node-reads node)))))
                   (assert range) (assert (eql :RANGE (node-type range)))
                   (getattr range :idx))))
          (loop for load in loads do
            (let ((depend-bands
                    (loop with dep-ids = (aref-depends-on graph load)
                          for band in bands for size in vectorize-space
                          if (find (getattr (id->value graph (car (node-reads band))) :idx) dep-ids)
                            collect (cons band size))))
              (setf (gethash (car (node-reads load)) vectorize-context)
                    (make-vectorized :name (ngid (car (node-reads load)) "_vectorized") :node load :gids (map 'list (compose #'node->gid #'car) depend-bands)
                                     :space (map 'list #'car depend-bands) :block-size (map 'list #'cdr depend-bands)))))
          (loop for acc in accs do
            (setf (gethash (car (node-writes acc)) vectorize-context)
                  (make-vectorized :name (ngid (car (node-writes acc)) "_acc") :node acc
                                   :space bands :block-size vectorize-space
                                   :gids (map 'list #'node->gid bands)))))
        ;; [TODO]
        ;; - [ ] VECTOR(, gid) Problem Resolve
        ;; - [ ] Add Some Simplifiers
        ;;  - [ ] Rewrite Directive After This Rewrite
        ;;  - [ ] Loop Collapse, contiguous=true option, etc
        ;;  - [ ] VECTORIZE->STORE->VECTORIZE Fusion
        ;; - [ ] Softmax Vectorize
        ;; - [ ] Add Renderer Support
        ;; - [ ] Reminder Computation
        ;; - [ ] TensorCore
        ;; WIP Things:
        ;; - [ ] BIND, SetfでSortできるように注意
        ;; - [ ] _1のgidの処理をどうするか。まだSpaceは正しくない。
        ;; - [x] SETF
        ;; - [ ] @VECTORIZE Directive，たまに付与に失敗してる・・・
        (insert-nodes
         graph
         (graph-nodes
          (with-blueprint (:noopt t)
            (%bind
             (car (node-writes band))
             (%progn
              ;; Declarations
              (loop for load in loads
                    for ctx = (or (gethash (car (node-reads load)) vectorize-context) (error "The loader ~a is not in vectorized context." load))
                    collect
                    (%local (ngid (car (node-writes load)) "_vectorized")
                            (vectorized-block-size ctx) (tensor-relay-dtype (car (relay-writes (read-type-relay load))))))
              ;;(loop for load in loads                                                      
              ;;      collect
              ;;      (%local (ngid (car (node-writes load)) "_shared")
              ;;              vectorize-space (tensor-relay-dtype (car (relay-writes (read-type-relay load))))))
              (loop for acc in accs
                    for ctx = (or (gethash (car (node-writes acc)) vectorize-context) (error "The loader ~a is not in vectorized context." acc))
                    collect
                    (%local (ngid (car (node-writes acc)) "_acc")
                            (vectorized-block-size ctx) (tensor-relay-dtype (car (relay-writes (read-type-relay acc))))))
              ;; Accs (VECTORIZED)
              (make-vectorized-form
               graph band nil
               #'(lambda (gids gids1 seen)
                   (declare (ignore gids1 seen))
                   (%progn
                    (loop for acc in accs for ctx = (gethash (car (node-writes acc)) vectorize-context)
                          collect
                          (%expr (ensure-setf ctx (%setf (%swizzle (ngid (car (node-writes acc)) "_acc") gids) (car (node-reads acc))))))))
               :is-reminder-p nil)
              ;; Loaders (Vectorized)
              (loop for suffix1 in (list "_vectorized"); "_shared")
                    for suffix2_prefix in (list "_vload" "_rload")
                    for vectorized-p in (list nil t)
                    collect
                    (loop for load in loads for nth upfrom 0
                          for suffix2 = (format nil "~a_~a" suffix2_prefix nth)
                          for ctx = (gethash (car (node-reads load)) vectorize-context)
                          collect
                          (make-vectorized-form
                           graph band ctx
                           #'(lambda (gids gids1 seen)
                               (declare (ignore seen))
                               (%expr
                                (ensure-setf ctx
                                 (%setf (%swizzle (ngid (car (node-writes load)) suffix1) gids)
                                        (ast-rewrite-and-clone
                                         graph (car (node-writes load))
                                         #'(lambda (node reads)
                                             (declare (ignore reads))
                                             (case (node-type node)
                                               (:LOAD
                                                (let ((new-gid (find (ngid (getattr node :value) suffix2) gids1 :key #'(lambda (x) (getattr x :idx)))))
                                                  (when new-gid (setf (getattr node :value) (getattr new-gid :idx)))
                                                  node))
                                               (:RANGE
                                                   (let ((new-gid (find (ngid (getattr node :idx) suffix2) gids1 :key #'(lambda (x) (getattr x :idx)))))
                                                     (if new-gid
                                                         (let ((n (%clone-node new-gid)))
                                                           (setf (node-writes n) (node-writes node))
                                                           n)
                                                         node)))
                                               (otherwise node))))))))
                           :is-reminder-p vectorized-p
                           :suffix suffix2)))
              ;; VectorizedALUs Rewriter
              (loop for alu in alus
                    collect
                    (ast-vectorize-alu graph alu vectorize-context))
              ;; Vectorized/Reminder Stores
              (loop for suffix1 in (list "_vectorized") ;; _shared
                    for suffix2_prefix in (list "_vstore" "_rstore")
                    for vectorized-p in (list nil t)
                    collect
                    (loop for store in stores for nth upfrom 0
                          for suffix2 = (format nil "~a_~a" suffix2_prefix nth)
                          for aref = (id->value graph (car (node-reads store)))
                          for ctx = (or
                                     (gethash (car (node-reads aref)) vectorize-context)
                                     (let ((bind (id->value graph (car (node-reads aref)))))
                                       (when (and bind (eql (node-type bind) :BIND))
                                         (gethash (getattr bind :value) vectorize-context))))
                          collect
                          (make-vectorized-form
                           graph band ctx
                           #'(lambda (gids gids1 seen)
                               (declare (ignore seen))
                               (assert (and aref (eql (node-type aref) :AREF)))
                               (print store)
                               (assert ctx)
                               (%expr
                                (ensure-setf
                                 ctx
                                 (%setf
                                  (ast-rewrite-and-clone
                                   graph (car (node-reads store))
                                   #'(lambda (node reads)
                                       (declare (ignore reads))
                                       (case (node-type node)
                                         (:LOAD
                                          (let ((new-gid (find (ngid (getattr node :value) suffix2) gids1 :key #'(lambda (x) (getattr x :idx)))))
                                            (when new-gid (setf (getattr node :value) (getattr new-gid :idx)))
                                            node))
                                         (:RANGE
                                             (let ((new-gid (find (ngid (getattr node :idx) suffix2) gids1 :key #'(lambda (x) (getattr x :idx)))))
                                               (if new-gid
                                                   (let ((n (%clone-node new-gid)))
                                                     (setf (node-writes n) (node-writes node))
                                                     n)
                                                   node)))
                                         (otherwise node))))
                                  (%swizzle
                                   (node->id1
                                    (emit (make-node :JIT :BIND (list (gensym)) (node-writes (vectorized-bind-to ctx))
                                                     :value (vectorized-name ctx))))
                                   gids)))))
                           :is-reminder-p vectorized-p
                           :suffix suffix2)))))))))
      (caten/codegen/blueprint:print-blueprint graph t)
      graph)))
;; DEFINE-FLOAT-8x8
;; VECTOR_LOAD_SIMPLIFY_PATTERN (CONTIGUOUS=True/False)
;; (defun ast-band-split-reduce ()) <- ReductionがなかったらError
;; SplitReduce, SyncThreads, SharedMemory
(defun ast-band-collapse (graph bands &key (dtype :int64) (parallel nil))
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
  ;; [TODO] Loop Interchange?
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
           (sizes (map 'list #'(lambda (r) (car (node-reads r))) ranges))
           (new-body-id (gensym))
           (new-body
             ;; [TODO] Compute Step
             (with-context
                 (_
                  (%bind
                   new-body-id
                   ;; Rewrite i, j (idx)
                   (let ((acc (maybe-fixnum 1)) out)
                     (dolist (sz (reverse sizes))
                       (push acc out)
                       (setf acc (%mul (maybe-fixnum acc) (maybe-fixnum sz))))
                     (append
                       (loop for r in ranges
                             for b in bands
                             for size in sizes
                             for stride in (reverse out)
                             do (%bind (car (node-writes r)) (%mul (%mod (%idiv merged-bands-idx stride) size) (second (node-reads r))))
                                (loop for node in (graph-nodes graph)
                                      if (or
                                          (and (eql (node-type node) :LOAD) (eql (getattr node :value) (getattr r :idx)))
                                          (and (eql (node-type node) :RANGE) (eql (getattr node :idx) (getattr r :idx))))
                                        do (%bind (car (node-writes node)) (%mod (%idiv merged-bands-idx stride) size))))
                     (apply
                      #'%progn
                      (list (id->value graph (second (node-reads (car (last bands)))))))))))))
           (outerband (make-node :Render :FOR (node-writes (car bands))
                                 (list merged-bands-idx new-body-id) :mark :noopt :parallel parallel)))
      (declare (ignore _))
      (insert-nodes graph (append (graph-nodes merged-size-graph) (graph-nodes new-body) (graph-nodes merged-bands)))
      (insert-nodes graph (list outerband))
      (simplify-ast graph)
      graph)))
