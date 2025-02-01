;;;; render-ops.lisp
;;;; This file includes the following features which is required to generate the optimized code.
;;;; - ASTGraph Creation
;;;; - ASTGraph Simplification
;;;; - ASTGraph Optimization (e.g.: Tile range)
(in-package :caten/ir)

(defmacro with-blueprint ((&key (noopt nil)) &body body)
  `(let* ((*ctx* (make-graph))
          (out (progn ,@body)))
     (assert (node-p out) () "The last form must be a node.")
     (setf (graph-outputs *ctx*) (node-writes out))
     (let ((graph (->fast-graph *ctx*)))
       (unless ,noopt (setf graph (simplify-ast graph)))
       graph)))
;; ~~ Control Flows ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %range (bind size body &key (step 1) (dtype *default-int*) (out (gensym "RANGE")) (mark :noopt))
  (declare (type symbol bind) (type (or node symbol) body) (type (or symbol node fixnum) size step) (type keyword dtype) (type symbol out) (type (member :coincident :noopt :reduction) mark))
  (let ((range (emit (make-node :Render :RANGE (list bind) (map 'list #'node->id1 (list size step))))))
    (emit (make-node :Render :FOR (list out) (map 'list #'node->id1 (list range body)) :mark mark))))

(defmacro %dotimes ((bind size &optional (mark :noopt)) &body body)
  `(%range ',bind ,size (%progn ,@body) :mark ,mark))

(defun %if (condition body &key (out (gensym "IF")))
  (declare (type (or symbol node) condition body) (type symbol out))
  (emit (make-node :Render :IF (list out) (map 'list #'node->id1 (list condition body)))))

(defun %when (condition body &key (out (gensym "IF"))) (%if condition body :out out))

(defun %progn (&rest body &aux (out (gensym "PROGN")))
  (assert (every #'(lambda (x) (or (symbolp x) (node-p x))) body) () "%progn: The body must be a list of symbols or nodes.")
  (emit (make-node :Render :PROGN (list out) (map 'list #'node->id1 (loop for b in body if b collect b)))))

(defun %global (name) (emit (make-node :Render :DEFINE-GLOBAL (list name) nil)))

(defun %barrier (&key (out (gensym "BARRIER"))) (emit (make-node :Render :BARRIER (list out) nil)))

(defun %bind (name node)
  (declare (type symbol name) (type node node))
  (assert (= 1 (length (node-writes node))) () "%bind: The node must have exactly one read.")
  (setf (node-writes node) (list name))
  node)

(defun %aref (name idx &key (out (gensym "AREF")))
  (declare (type (or symbol node) name idx))
  (emit (make-node :Render :Aref (list out) (map 'list #'node->id1 (list name idx)))))

(defun %function (name args body &aux (out (gensym "FUNCTION")))
  (emit (make-node :Render :Function (list out) (map 'list #'node->id1 (append (list body) args)) :name name)))

(defun %expr (name &key (out (gensym "EXPR"))) (emit (make-node :Render :EXPR (list out) (list name))))

(defmacro %defun (name (&rest args) &body body)
  `(%function ',name (list ,@args) (%progn ,@body)))
;; ~~ AST Simplification ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun Empty! (node)
  (assert (typep (node-attr node) 'RenderOps))
  (let ((node (copy-node node)))
    (setf (getattr node :is-empty) t)
    node))

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
    ((:IF (cond1 (:IF (cond2 body))))
     ->
     ((node graph)
      (with-context-nodes
        (new-cond (%and cond1 cond2))
        (out (%if new-cond body :out (car (node-writes node)))))))
    ;; Removing Empty IF/Range
    ((:FOR ((:Range (_ _)) (:PROGN ())) :is-empty (guard x (null x))) -> ((node graph) (Empty! node)))
    ((:IF (_ (:PROGN ())) :is-empty (guard x (null x))) -> ((node graph) (Empty! node)))
    ((:FOR ((:Range (_ _)) (:FOR ((:RANGE (_ _)) _) :is-empty (guard x (identity x)))) :is-empty (guard y (null y))) -> ((node graph) (Empty! node)))
    ((:IF (_ (:IF (_ _) :is-empty (guard x (identity x)))) :is-empty (guard y (null y))) -> ((node graph) (Empty! node)))
    ;; If the size==1 -> remove the range
    ;; ((:RANGE (bind size step body)) -> ((node graph))
    ;; :FOR + :PROGN (X)
    )

(defun ast-descendants-graph (graph outputs &key (seen) (result) (stop-at (make-hash-table)))
  (declare (type FastGraph graph) (type list outputs))
  (let ((out-ids (remove-duplicates (apply #'append (map 'list #'node-writes outputs)))))
    (labels ((explore (x &aux (node (id->value graph x)))
               (when (or (null node) (find x seen)) (return-from explore))
               (when (gethash x stop-at) (return-from explore))
               (push x seen)
               (push node result)
               (mapc #'explore (node-reads node))))
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
  (reverse exprs))

(defun exprify-ast (graph &aux (seen nil))
  (declare (type FastGraph graph) (optimize (speed 3)) (type list seen))
  "Groups multiple strongly connected ops into a single Expr. Expr and Expr are also mergeable."
  ;; Find sink points
  (labels ((render-p (node) (and (eql (node-class node) :Render) (null (find (node-type node) `(:EXPR :Aref :DEFINE-GLOBAL)))))
           (sort-progn-body (parents &aux (dg (ast-descendants-graph graph parents)) (m (ast-make-sink-map dg)))
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
           (explore (id &aux (node (id->value graph id)))
             (when (or (null node) (find (the symbol id) seen)) (return-from explore))
             (push (node-id node) seen)
             ;; A Expr is only mergeable with descendants w/ current PROGN.
             (when (eql (node-type node) :PROGN)
               (let* ((new-progn
                        (loop with parents = (split-parent (map 'list #'(lambda (x) (id->value graph x)) (node-reads node)))
                              for p in parents
                              if (listp p) append (let ((p (sort-progn-body p))) (insert-nodes graph p) p)
                                else collect p))
                      (new-progn (apply #'%progn new-progn)))
                 (setf (node-writes new-progn) (node-writes node))
                 (insert-nodes graph (list new-progn))))
             (mapc #'explore (node-reads (id->value graph id)))))
    (mapc #'explore (graph-outputs graph)))
  graph)

(defun ast-purge-realize (graph)
  "The first argument of MOVE in the EXPRBlock does not use the first argument and thus removed."
  ;; 1. Search for EXPR
  ;; 2. Search for MOVE
  ;; 3. Rewrite MUL(MOVE(A, AREF(B)), C) -> ...
  
  )

(defun ast-resolve-reduction (graph)
  "Resolves the storage-id to satisfy the reduction/assign relations"
  
  )

(defun simplify-ast (graph
                     &key
                       (opts
                        (list
                         ;#'exprify-ast
                         #'simplify-control-flow
                         #'(lambda (x) (optimize-aasm x :heavy-opt-threshold 0)))))
  (declare (type graph graph))
  ;; [TODO] Simplify the ast graph based on indexing dependencies!
  ;; e.g.: relocate allocate on the top
  (verify-graph (->graph graph)) ;; [TMP]
  (let ((g (funcall (apply #'compose opts) graph)))
    (verify-graph g)
    g))

(defun print-ast (graph)
  (pprint-graph graph)
  ;(caten/air:->dot graph :pathname "/tmp/graph.dot")
  (viz-ast graph))
;; [TODO] Decompose 3 -> 1 + 1 + 1 and optimize the indexing?
(defun viz-ast (graph) (uiop:symbol-call :caten/codegen/blueprint :print-blueprint graph t))
;; ~~ Optimizations ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun apply-tile (graph b1 b2)
  ;; Rewrite IDX -> ...
  ;; TODO: Prognと同じ理由でFailしない？
;  (funcall
;   (Simplifier
;       ()
;       ((:RANGE ((guard bind1 (eql bind1 b1)) size1 step1 (:RANGE ((guard bind2 (eql bind2 b2)) size2 step2 body) :mark (eql :coincident))) :mark (eql :coincident))
;        ->
;        ((node graph) nil)))
   graph)

(defun ast-shift ())
(defun ast-vectorize ())
(defun ast-grouptop ())
(defun ast-group ())

(defstruct AstGraph
  (graph (error "Graph must occur") :type Graph)
  (node (error "Node must occur") :type Node))
;; [TODO] OpFusion
;; PROGN+PROGN -> PROGN
;; IndexingをもっとSimplifyしたい。RANGEの外に出す方法？
;; イメージ:
;; ./caten/codegen -> caten/ir/render-ops.lispの機能を使って色々AST変形を実施する
;; - Remove :GLOBAL :LOCAL If Guard (which is rebundant only)
;; - Remove :LOAD is an args of buffer, instead, use :DEFINE-GLOBAL
;; - EXPRの実装が先？
;; - 10000 Total LoC
;; - how to manipulate gids?
;; - can we vectorize/tile Range?
(print-ast
 (with-blueprint ()
   (%defun eladd ((%global 'a) (%global 'b) (%global 'm) (%global 'n))
     (%dotimes (gid0 (%add (%iconst 'm) (%iconst 'n)) :coincident)
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

(print-ast
 (with-blueprint ()
   (%defun smth ((%global 'x))
     (%range
      '_gid0 (%iconst 100)
      (%progn
       (%range
        '_gid1 (%iconst 1000)
        (%progn
         (%add
          (%aref 'x '_gid0)
          (%aref 'x '_gid1))))
       
       )))))
;; ^ これ使ってOP定義できるようにする(AOT)
;; [TODO]
;; - OpFusion
;; - TileBands
;;  - Vectorize
;;    - PrefetchがISL無しで実装できるか？
;;  - Parallelize
;;  - It is IR's obligation to provide the information to opfusion
;;  - Switch to use aasm instead of blueprint in codegen
;;  - Remove EXPR, EXPR-Cache, and so on ...
;;  - EXPR内部の
;;  - defsimplifier -> make-simplifierにする
;; - Workload:
;;  - Exprify
;;  - RANGE Specification
;;  - Lowering INDEX-COMPONENTS, Aref Indexing
;;  - any valid way to run type inference? ==> Supercede the old type infer results
;;  - remove the declare-type option instead use allocate
;;  - Redefine the ast?
;;    IDX, BODY <- RANGE(SIZE, STEP)
;;    SimplifierがLeafの方に持ってくと依存がおかしくなる気がする
;;    is not top-down graph but bottom-up graph ...
;; Remove Fused Move using Pattern Mathcer, Exprify is located here!
;; graph-seen is always an argument of the function?
;; [TODO] Update Aref Renderer
;; TODO
;; - FORのAllocate,Symbolic ARefのEXPR?
;; - RenderOpsしかないことを保証
;; - 常にLOADをPropagateしたい (%defglobalの挿入が必須)
;; - %prognの時系列
;; - val_8はなんで0? --> simplfy-ast
;; - まずRangeのIndexingの実装方法をちゃんと考える。。。
;; - Undefined Variableを直すのが先
;; - Need to reimplement :RANGE First ...
