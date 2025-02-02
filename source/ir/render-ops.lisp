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
       (unless ,noopt (setf graph (%simplify-ast graph)))
       graph)))
;; ~~ Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %range (bind size body &key (step 1) (dtype *default-int*) (out (gensym "RANGE")) (mark :noopt))
  "
Constraints:
- SIZE/STEP is always an EXPR, that is, must not include an control flow.
"
  (declare (type symbol bind) (type (or node symbol) body) (type (or symbol node fixnum) size step) (type keyword dtype) (type symbol out) (type (member :coincident :noopt :reduction) mark))
  (when (node-p size) (setf size (%expr (node->id1 size))))
  (when (node-p step) (setf body (%expr (node->id1 body))))
  (let ((range (emit (make-node :Render :RANGE (list bind) (map 'list #'node->id1 (list size step)) :idx bind :dtype dtype))))
    (emit (make-node :Render :FOR (list out) (map 'list #'node->id1 (list range body)) :mark mark))))

(defmacro %dotimes ((bind size &key (mark :noopt) (id (gensym "RANGE"))) &body body)
  `(let ((,bind ',bind)) (%range ',bind ,size (%progn ,@body) :mark ,mark :out ',id)))

(defun %if (condition body &key (out (gensym "IF")))
  "
Constraints:
- condition is always an EXPR, that is, must not include an control flow."
  (declare (type (or symbol node) condition body) (type symbol out))
  (when (node-p condition) (setf condition (%expr (node->id1 condition))))
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
;; ~~ ControlFlow Simplifiers ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    ;; If the size==1 -> remove the range
    ;; [TODO] Make it working ...
    ;; TODO: (RANGE (4 4)) is also removable
    ;((:RANGE (1 1) :idx idx :dtype dtype) -> ((node graph) (with-context-nodes (out (%bind idx (%iconst 0 :dtype dtype))))))
    ;((:FOR ((Var (= 0) _) body)) -> body)
    ;; TODO: Fuse :FOR+:PROGN to maximize the band depth
    )
;; ~~ Exprify (OpFusion) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-descendants-graph (graph outputs &key (seen) (result) (stop-at (make-hash-table)))
  (declare (type FastGraph graph) (type list outputs))
  (let ((out-ids (remove-duplicates (apply #'append (map 'list #'node-writes outputs)))))
    (labels ((explore (x &aux (node (id->value graph x)))
               (when (or (null node) (find x seen)) (return-from explore))
               (when (gethash x stop-at) (return-from explore))
               (when (member (node-type node) `(:RANGE :DEFINE-GLOBAL)) (return-from explore))
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
  exprs)

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
;; ~~~~ Rewriters(Verification) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun ast-verify-sequence (graph &aux (sorted (graph-nodes (->graph-with-tpsort graph))))
  "The function `ast-verify` sorts the order of PROGN(S1, S2, ..., Sn) based on the order of topological sorted graph."
  (declare (type FastGraph graph))
  (flet ((helper (node)
           ;; The younger the latter
           (let* ((field (map 'list #'(lambda (x) (id->value graph x)) (node-reads node)))
                  (field-positions (map 'list #'(lambda (x) (position (node-id x) sorted :key #'node-id)) field))
                  (pairs (map 'list #'cons field field-positions)))
             (assert (every #'identity field) () "ast-verify-sequence: There is an undefined progn field: ~a" field)
             (setf (node-reads node) (map 'list (compose #'car #'node-writes #'car) (sort pairs #'< :key #'cdr))))))
    (loop for node in (graph-nodes graph)
          when (eql (node-type node) :PROGN)
            do (helper node))
    graph))

(defun ast-maximize-band-depth (graph &aux (bands) (gid2band (make-hash-table)) (count 0))
  (funcall
   (Simplifier
       ()
       ((:FOR (range (:PROGN (body))) :mark mark) -> (:FOR (range body) :mark mark))
       ;; Found a sequence of band which is independent each other ==> fuse the band!
       ((:FOR ((:RANGE (_ _) :idx r1) (:FOR ((:RANGE (_ _) :idx r2) _) :mark (eql :coincident))) :mark (eql :coincident))
        ->
        ((node graph)
         ;; [TODO] This implementation can recognise an innermost loop band fusion?
         (when (null (find (cons r1 r2) bands :test #'equal))
           (push (cons r1 r2) bands)
           (list node)))))
   graph)
  (flet ((maybe-new (x y) (or (gethash x gid2band) (gethash y gid2band) (intern (format nil "B~a" (incf count)) "KEYWORD"))))
    (loop for (r1 . r2) in bands
          do (setf (gethash r1 gid2band) (maybe-new r1 r2) (gethash r2 gid2band) (maybe-new r1 r2)))
    (loop for node in (graph-nodes graph)
          for range = (id->value graph (car (node-reads node)))
          if (and node range (eql (node-type node) :FOR) (eql (node-type range) :RANGE) (eql :coincident (getattr node :mark))) do
            (setf (getattr node :band) (maybe-new (getattr range :idx) (getattr range :idx))))) 
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
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %simplify-ast (graph
                      &key
                        (opts
                         (list
                          #'fold-constant
                          ;;#'minimize-duplicated-symbolic-path;; <- TODO: Implement AST-supported version of this to simplify the indexing
                          ;; ^ 共通項を見つけたらPROGNをInsertして配置するように？・・・
                          #'fuse-duplicated-store
                          #'simplify-control-flow
                          #'exprify-ast
                          #'(lambda (x) (verify-graph x) x)
                          #'ast-verify-sequence
                          #'ast-maximize-band-depth
                          #'(lambda (x) (print (->graph-with-tpsort x)) x)
                          )))
  "Simplifies the AST"
  (declare (type graph graph))
  ;; [TODO] Simplify the ast graph based on indexing dependencies!
  (funcall (apply #'compose (reverse opts)) graph))

(defun simplify-ast (graph)
  (%simplify-ast graph :opts (list #'fold-constant #'fuse-duplicated-store #'simplify-control-flow #'ast-verify-sequence)))

;; ~~ Scheduling  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %ast-band-tile (graph band tile-sizes &key (sp "_p") (sc "_c") (cid (gensym "C")) &aux (bands))
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
               (assert (and node (eql (node-type node) :FOR)) () "%ast-tile-band: The given tile-sizes ~a is too large (<= ~a)" band-depth)
               (assert (eql (getattr node :band) (getattr (car bands) :band)) () "%ast-tile-band: ~a and ~a are not coincident." node (car bands))
               (push node bands)
               (explore (second (node-reads band)) (1+ band-depth)))))
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
          for idx = (car (node-reads band))
          for idx-new = (%add (ngid idx sp) (ngid idx sc) :id idx)
          do (insert-nodes graph (list idx-new)))
    ;; [TODO] Ensure the old grpah was purged from graph
    (verify-graph graph)
    graph))
;; tile series
(defun ast-band-tile ())
(defun ast-band-parallelize ()) ;; for collapse (band_depth)
(defun ast-band-global ())

(defun ast-vectorize ())
(defun ast-upcast ())
;; smem transfers
(defun ast-grouptop ())
(defun ast-group ())
;; Microkernel Optimization
(defun ast-microkernel ())
;;; OLD
(defstruct AstGraph
  (graph (error "Graph must occur") :type Graph)
  (node (error "Node must occur") :type Node))
;; OLD
(defun print-ast (graph)
  (pprint-graph graph)
  ;(caten/air:->dot graph :pathname "/tmp/graph.dot")
  (viz-ast graph))
;; [TODO] Decompose 3 -> 1 + 1 + 1 and optimize the indexing?
(defun viz-ast (graph) (uiop:symbol-call :caten/codegen/blueprint :print-blueprint graph t))
;; todo
(defstruct CatenFunction (blueprint))
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
(let ((g
        (with-blueprint ()
          (%defun eladd ((%global 'a) (%global 'b) (%global 'm) (%global 'n))
            (%dotimes (gid0 (%add (%iconst 'm) (%iconst 'n)) :mark :coincident :id target-loop)
              (%progn
               (%progn
                (let ((idx1 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst gid0)))
                      (idx2 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst gid0))))
                  (%when (%< nil :row (%iconst 'gid0) (%add (%iconst 'm) (%iconst 'n)))
                         (%when (%< nil :row (%iconst 'gid1) (%add (%iconst 'm) (%iconst 'n)))
                                (%progn
                                 (%add (%aref 'a (%iconst 0)) (%aref 'b idx2))
                                 (%add (%aref 'a (%add idx1 (%iconst 1))) (%aref 'b (%add idx2 (%iconst 1))))
                                 (%add (%aref 'a (%add idx1 (%iconst 2))) (%aref 'b (%add idx2 (%iconst 2))))
                                 (%add (%aref 'a (%add idx1 (%iconst 3))) (%aref 'b (%add idx2 (%iconst 3)))))))))))))))
  (%ast-tile-band g (id->value g 'target-loop) `(4))
  (print-ast g))

(let ((g
        (with-blueprint ()
          (%defun 2d-elwise-add ((%global 'a) (%global 'b))
            (%dotimes (gid0 512 :mark :coincident :id tgt-loop)
              (%dotimes (gid1 512 :mark :coincident)
                (let ((idx (%add (%mul (%iconst 512) gid0) gid1)))
                  (%add (%aref 'a idx) (%aref 'b idx)))))))))
  (%ast-band-tile g (id->value g 'tgt-loop) `(64 32))
  (%ast-band-tile g (id->value g 'tgt-loop) `(4 4))
  
  (simplify-ast g)
  ;; (%ast-band-global
  (print-ast g))

(print-ast
 (with-blueprint ()
   (%defun smth ((%global 'x))
     (%range
      '_gid0 (%iconst 100)
      (%progn
       (%range
        '_gid1 (%iconst 100)
        (%progn
         (%add
          (%aref 'x '_gid0)
          (%aref 'x '_gid1))))
       
       )))))
;; TODO: %defun -> macro
;; (get-caten-function 'smth) ==> CatenFunction
;; ^ (opt f scheduling) --> Scheduling Transformation
;; Finally (render-ast ast renderer)
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
;; Workload
;; - 1. GIDに依存しないOpはなるべく外に出す
;; - 2. MaximizeBandを実装
;; - 3. SIZE=1 --> Remove
;; - 4. 

;; TODO
;; - FORのAllocate,Symbolic ARefのEXPR?
;; - RenderOpsしかないことを保証
;; - 常にLOADをPropagateしたい (%defglobalの挿入が必須)
;; - %prognの時系列
;; - まずRangeのIndexingの実装方法をちゃんと考える。。。
;; - Undefined Variableを直すのが先
;; - Need to reimplement :RANGE First ...
;; - StorageIDの実装(LOAD, Reductionの時系列...)
;;   - Assign/Reduction
;;   - Allocate+Load
;;   - Store after reduction
;;   - Insert Store Rewriting Rule!
;;   - in DAG
;;   - FIRST PRIORITY: 
;;   - Render -> AST, Add RenderOps (Renderer will use this for simplicity, ast->render to translate this)
;;   - Type Inferenceを実行した後，Scalar LoadをPropagateできる (TODO: 既存の実装で1とか2を直接読んでる箇所は修正すること)
;;   - まず時系列問題を修正する，その次に謎のeを直す
;;   - First, play w/ manual scheduler and reimplement gemm scheduling example.
;;   - EXPR Purge
;;   - ASTGraph -> RenderOps Listは確定, print-blueprint的な実装で再現できるはず
;;   - UNROLL/Vectorizeをどうやって実装するか？
;;     -> GID0が頂点，再起的にSubgraphを探索してGID0に関連するIDを+1する
;;   - TODO: tensor-compute-schedule
;;  - TODO: (NEG 1), (MUL 1, 4) simplification!
;;  - Remove away MAX
