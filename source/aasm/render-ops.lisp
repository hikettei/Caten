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
(defun %expr (name &key (out (gensym "EXPR"))) (emit (make-node :Render :EXPR (list out) (list name))))

(defun %range (bind size body &key (step 1) (dtype *default-int*) (out (gensym "RANGE")) (mark :noopt) (range))
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
  (let ((range (or range (emit (make-node :Render :RANGE (list bind) (map 'list #'node->id1 (list size step)) :idx bind :dtype dtype)))))
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
  (assert (every #'(lambda (x) (or (symbolp x) (node-p x))) body) () "%progn: The body must be a list of symbols or nodes.")
  (emit (make-node :Render :PROGN (list out) (map 'list #'node->id1 (loop for b in body if b collect b)))))

(defun %global (name dtype pointer-p &key (mode :io))
  (declare (type dtype-t dtype) (type boolean pointer-p) (type symbol name))
  (emit (make-node :Render :DEFINE-GLOBAL (list name) nil :dtype dtype :pointer-p pointer-p :mode mode)))

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
  (emit (make-node :JIT :SPACE (list id) nil :level :thread :rank rank :dtype dtype :size (caten/aasm/expr:expr-const size dtype))))

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
;;    ((:FOR (range (:PROGN (body))) :mark mark) -> (:FOR (range body) :mark mark))
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
                        (insert-nodes graph load)))
              (insert-nodes graph (append =0 (list expr)))
              body))))))
    ;; TODO: Fuse :FOR+:PROGN to maximize the band depth
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

(defun ast-exprify-tensor-graph (base-graph dg sink-map &aux (exprs))
  (declare (type FastGraph dg) (type hash-table sink-map))
  (labels ((exprify (id &aux (name (gensym "E")))
             (push (%expr name :out id) exprs)
             (let ((out-node (id->value dg id)))
               (setf (node-writes out-node) (list name))
               (insert-nodes base-graph (list out-node)))))
    (mapc #'exprify (hash-table-keys sink-map)))
  exprs)

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
                   if (render-p p) do (when tmp (push (reverse tmp) results)) (push p results) (setf tmp nil)
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
;; ~~~~ Rewriters(Verification) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-expr-graph (graph expr &aux (seen nil) (nodes))
  (declare (type FastGraph graph) (type node expr))
  (assert (eql (node-type expr) :EXPR))
  (labels ((explore (id &aux (node (id->value graph id)))
             (when (or (null node) (find id seen)) (return-from explore))
             (when (eql (node-type node) :EXPR) (return-from explore))
             (push node nodes)
             (mapc #'explore (node-reads node))))
    (explore (car (node-reads expr))))
  (let ((g (apply #'make-graph nodes)))
    (setf (graph-outputs g) (node-reads expr))
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
