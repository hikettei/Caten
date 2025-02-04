;;;; render-ops.lisp
;;;; This file includes the following features which is required to generate the optimized code.
;;;; - ASTGraph Creation
;;;; - ASTGraph Simplification
;;;; - ASTGraph Optimization (e.g.: Tile, Unroll, Microkernel, etc)
(in-package :caten/ir)

(defparameter *function* (make-hash-table))

(defun get-caten-function (name)
  (copy-graph (or (gethash name *function*) (error "The function ~a is not defined." name))))

(defmacro with-blueprint ((&key (noopt nil)) &body body)
  `(let* ((*ctx* (make-graph))
          (out (progn ,@body)))
     (assert (node-p out) () "The last form must be a node.")
     (setf (graph-outputs *ctx*) (node-writes out))
     (let ((graph (->fast-graph *ctx*)))
       (unless ,noopt (setf graph (%simplify-ast graph)))
       graph)))
;; ~~ Interface ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %range (bind size body &key (step 1) (dtype *default-int*) (out (gensym "RANGE")) (mark :noopt))
  "
```
(%range bind size body &key (step 1) (dtype *default-int*) (out (gensym \"RANGE\")) (mark :noopt))
```
Constraints:
- SIZE/STEP is always an EXPR, that is, must not include an control flow.
"
  (declare (type symbol bind) (type (or node symbol) body) (type (or symbol node fixnum) size step) (type keyword dtype) (type symbol out) (type (member :coincident :noopt :reduction) mark))
  (when (node-p size) (setf size (%expr (node->id1 size))))
  (when (node-p step) (setf body (%expr (node->id1 body))))
  (let ((range (emit (make-node :Render :RANGE (list bind) (map 'list #'node->id1 (list size step)) :idx bind :dtype dtype))))
    (emit (make-node :Render :FOR (list out) (map 'list #'node->id1 (list range body)) :mark mark))))

(defmacro %dotimes ((bind size &key (mark :noopt) (id (gensym "RANGE"))) &body body)
  ""
  `(let ((,bind ',bind)) (%range ',bind ,size (%progn ,@body) :mark ,mark :out ',id)))

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

(defun %global (name dtype pointer-p)
  (declare (type dtype-t dtype) (type boolean pointer-p) (type symbol name))
  (emit (make-node :Render :DEFINE-GLOBAL (list name) nil :dtype dtype :pointer-p pointer-p)))

(defun %barrier (&key (out (gensym "BARRIER"))) (emit (make-node :Render :BARRIER (list out) nil)))

(defun %bind (name node)
  (declare (type symbol name) (type node node))
  (assert (= 1 (length (node-writes node))) () "%bind: The node must have exactly one read.")
  (setf (node-writes node) (list name))
  node)

(defun %aref (name idx &key (out (gensym "AREF")))
  (declare (type (or symbol node) name idx))
  (emit (make-node :JIT :Aref (list out) (map 'list #'node->id1 (list name idx)))))

(defun %expr (name &key (out (gensym "EXPR"))) (emit (make-node :Render :EXPR (list out) (list name))))

(defun %setf (tgt value &key (out (gensym "SETF")))
  (declare (type (or symbol node) tgt value))
  (emit (make-node :JIT :SETF (list out) (map 'list #'node->id1 (list tgt value)))))

(defmacro %defun (name (&rest args) &body body)
  (flet ((verify-args (arg)
           (assert (listp arg))
           (multiple-value-bind (value dtype pointer-p) (apply #'values arg)
             (assert (symbolp value))
             (assert (typep dtype 'dtype-t))
             (assert (typep pointer-p 'boolean))
             (list value dtype pointer-p))))
    `(setf (gethash ',name *function*)
           (with-blueprint ()
             (%progn
              (let (,@(loop for arg in args
                            for arg-list = (verify-args arg)
                            collect `(,(car arg-list) (%global ',(car arg-list) ,@(cdr arg-list)))))
                (%progn ,@body)))))))

(defun %empty () (make-node :JIT :Empty (list (gensym)) nil))
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
    ;;((:RANGE ((EConst 1) (EConst 1)) :idx idx :dtype dtype) -> ((node graph) (with-context-nodes (out (%bind (car (node-writes node)) (%iconst 0 :dtype dtype))))))
    ;;((:FOR ((EConst 0) body)) -> body)
    ;; TODO: Fuse :FOR+:PROGN to maximize the band depth
    )
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
  (declare (type FastGraph graph) (optimize (speed 3)) (type list seen))
  "Groups multiple strongly connected ops into a single Expr. Expr and Expr are also mergeable."
  ;; Find sink points
  (labels ((render-p (node) (eql (node-class node) :Render))
           (sort-progn-body (parents &aux (dg (ast-descendants-graph graph parents :only-surface t)) (m (ast-make-sink-map dg)))
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
             (push id seen)
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
               (list (%bind (car (node-reads node)) (%empty)) node)))
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
                  ;;((:LOAD   (_)) -> ((node graph) (Purge node)))
                  ((:<  (_ _ _)) -> ((node graph) (Purge node)))
                  ((:!= (_ _ _)) -> ((node graph) (Purge node)))
                  ((:Cast (_ _)) -> ((node graph) (Purge node))))
              expr-graph)
             (insert-nodes graph (graph-nodes expr-graph))
             (list expr)))
    (funcall
     (Simplifier () ((:EXPR (id)) -> ((node graph) (unless (find id seen1) (push id seen1) (simplify-expr node)))))
     graph)))
;; [TODO] TypeMap definition is in air
(defun ast-infer-type-map (graph))g
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
                          #'(lambda (x) (verify-graph x) x)
                          #'ast-verify-sequence
                          #'ast-maximize-band-depth)))
  "Simplifies the AST"
  (declare (type FastGraph graph))
  (let ((g (funcall (apply #'compose (reverse opts)) graph)))
    (verify-graph g)
    g))

(defun simplify-ast (graph)
  (%simplify-ast graph :opts (list #'fold-constant #'fuse-duplicated-store #'simplify-control-flow #'ast-verify-sequence)))
;; ~~ Scheduling ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
          for idx = (car (node-reads band))
          for idx-new = (%add (ngid idx sp) (ngid idx sc) :id idx)
          do (insert-nodes graph (list idx-new)))
    ;; [TODO] Ensure the old grpah was purged from graph
    (verify-graph graph)
    (values graph (nreverse globals) (nreverse locals))))
;;; OptOps (Tile Based)
(defun ast-band-tile (graph band tile-sizes)
  "Tiles the given band with tile-sizes."
  (%ast-band-tile graph band tile-sizes))

(defun %gid (rank graph range local-size &key (dtype :int64) (id (gensym "G")))
  (let* ((loop-size
           (if (numberp (car (node-reads range)))
               (caten/ir/expr:expr-const (car (node-reads range)) :float32)
               (let ((expr (id->value graph (car (node-reads range)))))
                 (assert (and expr (eql (node-type expr) :EXPR)) () "%gid: The parent for Range must be fixnum or EXPR.")
                 (caten/ir/expr:make-expr
                  :graph (ast-descendants-graph graph (node-reads expr))
                  :out (id->value graph (car (node-reads expr)))))))
         (loop-size (caten/ir/expr:expr-cast loop-size :float32))
         (local-size (caten/ir/expr:expr-const local-size :float32))
         (size (caten/ir/expr:expr-ceiling (caten/ir/expr:expr-div loop-size local-size) dtype)))
    (emit (make-node :JIT :SPACE (list id) nil :level :block :rank rank :dtype dtype :size size))))

(defun %lid (rank size &key (dtype :int64) (id (gensym "L")))
  (emit (make-node :JIT :SPACE (list id) nil :level :thread :rank rank :dtype dtype :size (caten/ir/expr:expr-const size dtype))))

(defun ast-band-tile-gpu (graph band local-sizes)
  "Tiles the given band and map them into gpu with local-sizes."
  (flet ((reveal-expr (x)
           (if (numberp x)
               x
               (let ((expr (id->value graph x)))
                 (assert (and expr (eql (node-type expr) :EXPR)))
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

(defun ast-band-unroll (graph band local-sizes)
  (multiple-value-bind (graph global-bands local-bands) (%ast-band-tile graph band local-sizes)
    ;; The work here is to remove away local-bands
    ;; also inserting reminder bands in the gloal-bands
    ;; [TODO] Insert Reminder Statements
    ;; [TODO] How to determine the unrolled variable index? it depends on time-series dependencies
    ))

(defun ast-vectorize (graph band local-sizes)
  ;; ensure stride == 1
  )

(defun ast-upcast ()) ;; reuse unroll

(defun ast-band-parallelize ()) ;; Not an tile but uses the depth of band to mark for collapse(N), reuse tile
;;; OptOps (Shared Memory Transfer)
(defun ast-group (graph band)
  ;; band == reduction
  )

(defun ast-grouptop (graph band))
;;; OptOps (MicroKernel)
(defun ast-microkernel ())
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; debug
(defun print-ast (graph)
  (pprint-graph graph)
  ;(caten/air:->dot graph :pathname "/tmp/graph.dot")
  (viz-ast graph))
;; [TODO] Decompose 3 -> 1 + 1 + 1 and optimize the indexing?
(defun viz-ast (graph) (uiop:symbol-call :caten/codegen/blueprint :print-blueprint graph t))
;; ./caten/codegen -> caten/ir/render-ops.lispの機能を使って色々AST変形を実施する
;; - Remove :GLOBAL :LOCAL If Guard (which is rebundant only)
;; - Remove :LOAD is an args of buffer, instead, use :DEFINE-GLOBAL
;; - EXPRの実装が先？
;; - 10000 Total LoC
;; - how to manipulate gids?
;; - can we vectorize/tile Range?

(%defun eladd ((a :float32 t) (b :float32 t) (m :int64 nil) (n :int64 nil))
  (%dotimes (gid0 (%add (%iconst 'm) (%iconst 'n)) :mark :coincident :id target-loop)
    (let ((idx1 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst gid0)))
          (idx2 (%mul (%add (%iconst 'm) (%iconst 'n)) (%iconst gid0))))
      (%when (%< nil :row (%iconst 'gid0) (%add (%iconst 'm) (%iconst 'n)))
             (%when (%< nil :row (%iconst 'gid1) (%add (%iconst 'm) (%iconst 'n)))
                    (%progn
                     (%add (%aref a idx1) (%aref b idx2))
                     (%add (%aref a (%add idx1 (%iconst 1))) (%aref b (%add idx2 (%iconst 1))))
                     (%add (%aref a (%add idx1 (%iconst 2))) (%aref b (%add idx2 (%iconst 2))))
                     (%add (%aref a (%add idx1 (%iconst 3))) (%aref b (%add idx2 (%iconst 3))))))))))

(print-ast (get-caten-function 'eladd))

(%defun 2d-elwise-add ((a :float32 t) (b :float32 t))
  (%dotimes (gid0 512 :mark :coincident :id tgt-loop)
    (%dotimes (gid1 512 :mark :coincident)
      (let ((idx (%add (%mul (%iconst 512) gid0) gid1)))
        (%setf (%aref 'a idx) (%add (%aref a idx) (%aref b idx)))))))

(print-ast (get-caten-function '2d-elwise-add))

(let ((g (get-caten-function '2d-elwise-add)))
  (ast-band-tile-gpu g (id->value g 'tgt-loop) `(128 128)) ;; [TODO] Add Simplifier for removing IF Guard
  ;;(ast-band-tile g (id->value g 'tgt-loop) `(16 16))
  
  (simplify-ast g)
  (print-ast g))

(%defun matmul ((a :float32 t) (b :float32 t) (c :float32 t) (m :int64) (n :int64) (k :int64))
  (%dotimes (_gid0 (%iconst 'M) :mark :coincident)
    (%dotimes (_gid1 (%iconst 'K) :mark :coincident)
      (%bind 'acc (%iconst 0.0 :dtype :float32))
      (%dotimes (_gid2 (%iconst 'N) :mark :reduction)
        (%setf 'acc (%add 'acc (%mul (%aref a (%add (%mul (%iconst 'n) _gid0) _gid2)) (%aref b (%add _gid1 (%mul (%iconst 'k) _gid2)))))))
      (%setf (%aref c (%add _gid1 (%mul (%iconst 'k) _gid0))) 'acc))))

(print-ast (get-caten-function 'matmul))

(%defun smth ((x :float32 t))
  (%range
   '_gid0 (%iconst 100)
   (%progn
    (%range
     '_gid1 (%iconst 100)
     (%progn
      (%setf
       (%aref x '_gid0)
       (%add
        (%aref x '_gid0)
        (%aref x '_gid1))))))))

(print-ast (get-caten-function 'smth))
;; TODO: %defun -> macro
;; ^ これ使ってOP定義できるようにする(AOT)
;; [TODO]
;; - TileBands
;;  - Vectorize
;;    - PrefetchがISL無しで実装できるか？
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
;; - 2. MaximizeBandを実装 (OK)
;; - 3. SIZE=1 --> Remove
;; - 4. OptOpsを一通り実装する (GLOBAL/LOCAL), Vectorize, Unroll, and the most important thing TensorCore

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
;;     - TODO: tensor-compute-schedule
;;  - Remove away MAX (TODO) from tiled schedule

;; Finish Valid Codegen workload
;; - (with-inference-mode () (caten (forward (caten/nn:Embedding 128 128) (make-tensor `(128 128)))))
;;  - val_7を取り除く (PROGNでReadされてるから消えない)
;; - %GLOBAL, %AREFなどBlueprint作成APIの使用をconcreteする
;;  - 手動Schedulingの例としてExample提供して仕様を固める
;; - Memory Planner
;;   - ArefのNAMEを書き換えるだけで完了
;; - DTYPE MAPの実装を完了させる
;;  - これはTypeMapというAttributeに格納して，TypeRelayとは別のものにする(TypeRelayはCodegen内部で隠蔽する)
;; - EXPRIFY: EXPR+EXPRで何回も適用できるように，Simplifyした後のグラフでもやりたい
;; - Schedule Cacheを完了
;; - CodegenをUpdate, 全てのテストをPassする
;; - OptOpsに取り掛かる
