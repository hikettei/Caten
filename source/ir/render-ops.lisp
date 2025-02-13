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
  (when (node-p step) (setf body (%expr (node->id1 body))))
  (let ((range (or range (emit (make-node :Render :RANGE (list bind) (map 'list #'node->id1 (list size step)) :idx bind :dtype dtype)))))
    (emit (make-node :Render :FOR (list out) (map 'list #'node->id1 (list range body)) :mark mark))))

(defmacro %dotimes ((bind size &key (mark :noopt) (id (gensym "RANGE")) (range)) &body body)
  ""
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

(defun %global (name dtype pointer-p)
  (declare (type dtype-t dtype) (type boolean pointer-p) (type symbol name))
  (emit (make-node :Render :DEFINE-GLOBAL (list name) nil :dtype dtype :pointer-p pointer-p)))

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
               (let ((typed (car (getattr node :src-types))))
                 (assert typed () "ast-simplify-expr: Cannot deduce the first src of ~a" node)
                 (list (%bind (car (node-reads node)) (%empty (typed-dtype typed))) node))))
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
      #'ast-infer-typed-node)
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
                          #'(lambda (x) (verify-graph x) x)
                          #'ast-maximize-band-depth
                          #'ast-infer-typed-node)))
  "Simplifies the AST"
  (declare (type FastGraph graph))
  (let ((g (funcall (apply #'compose (reverse opts)) graph)))
    (verify-graph g)
    g))

(defun simplify-ast (graph)
  (%simplify-ast graph :opts (list #'fold-constant #'fuse-duplicated-store #'simplify-control-flow
                                   #'ast-simplify-expr #'ast-infer-typed-node)))
;; ~~ Type Map ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Typed (:constructor make-typed (dtype pointer-p)) (:conc-name typed-)) ;; -> AType
  (dtype dtype :type (or (member :void) dtype-t))
  (pointer-p pointer-p :type boolean))

(defmethod print-object ((typed Typed) stream)
  (print-unreadable-object (typed stream :type nil)
    (format stream "TYPED ~(~a~)~a" (typed-dtype typed) (if (typed-pointer-p typed) "*" ""))))

(defun ast-infer-typed-node (graph &aux (typemap (make-hash-table)) (waitlist (map 'list #'node-id (graph-nodes graph))))
  (declare (type FastGraph graph) (optimize (speed 3)) (type list waitlist))
  (flet ((send (node typed)
           (declare (type Typed typed))
           (assert (find (node-id node) waitlist))
           (setf waitlist (remove (node-id node) waitlist)
                 (gethash (car (node-writes node)) typemap) typed))
         (getyped (typed)
           (if (numberp typed)
               (if (floatp typed)
                   (make-typed *default-float* nil)
                   (progn
                     (assert (integerp typed))
                     (make-typed *default-int* nil)))
               (gethash typed typemap))))
    (loop for node in (graph-nodes graph) do
      (assert (subtypep (class-of (node-attr node)) 'TypedNode) () "The given node ~a is not defined as TypedNode." node)
      ;; Entry points of the graph.
      (case (node-type node)
        (:ALLOCATE
         (send node (make-typed (getattr node :dtype) (> (length (node-reads node)) 0))))
        (:DEFINE-GLOBAL
         (send node (make-typed (getattr node :dtype) (getattr node :pointer-p))))
        ;; Set VOID for control flow
        (:FOR (send node (make-typed :void nil)))
        (:PROGN (send node (make-typed :void nil)))
        (:IF (send node (make-typed :void nil)))
        (:SPACE (send node (make-typed (getattr node :dtype) nil)))
        (:BARRIER (send node (make-typed :void nil)))
        (:DEFINE-SHARED-MEMORY (send node (make-typed (getattr node :dtype) t)))))
    ;; Repeat until all leave inference is completed.
    (loop until (null waitlist)
          for changed-p = nil do
            (loop for node in (graph-nodes graph)
                  for src-ids = (map 'list #'getyped (node-reads node))
                  if (and (find (node-id node) waitlist) (every #'identity src-ids)) do
                    (setf changed-p t)
                    (case (node-type node)
                      (:AREF
                       (assert (typed-pointer-p (car src-ids)))
                       (send node (make-typed (typed-dtype (car src-ids)) nil)))
                      (:CAST
                       (send node (make-typed (getattr node :dtype) (typed-pointer-p (car src-ids)))))
                      (otherwise
                       (send node (or (apply #'caten/air::%get-output-to (node-attr node) src-ids) (error "Please add special typemap handler for the node ~a" node)))))
                  else if (and (find (node-id node) waitlist) (eql (node-type node) :BIND) (getyped (getattr node :value))) do
                    (send node (make-typed (typed-dtype (getyped (getattr node :value))) (typed-pointer-p (getyped (getattr node :value))))))
            (when (and waitlist (null changed-p))
              (error "Failed to deduce the graph type due to cycle dependencies of ~a"
                     (map 'list #'(lambda (x) (find x (the list (graph-nodes graph)) :key #'node-id)) waitlist))))
    ;; Deploy the inferred map
    (loop for node in (graph-nodes graph)
          for src-types = (map 'list #'getyped (node-reads node))
          for dst-types = (map 'list #'getyped (node-writes node)) do
            (when (and (eql (node-type node) :BIND) (some #'null src-types))
              (setf src-types (list (getyped (getattr node :value)))))
            (assert (every #'identity src-types)) (assert (every #'identity dst-types))
            (setf (getattr node :src-types) src-types (getattr node :dst-types) dst-types))
    graph))

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
          do (insert-nodes graph (list idx-new))
             (loop for node in (graph-nodes graph)
                   if (and (eql (node-type node) :RANGE) (eql (getattr node :idx) idx)) do
                     (let ((idx-new (%add (ngid idx sp) (ngid idx sc) :id (car (node-writes node)))))
                       (insert-nodes graph (list idx-new)))))
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
                  :graph (ast-descendants-graph graph (map 'list #'(lambda (x) (id->value graph x)) (node-reads expr)))
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

(defgeneric compute-unroll-reminder (reminder size step n-unroll)
  (:documentation "Finds the maximum integer which satisfies MOD(SIZE//STEP, n_unroll) == 0"))

(defmethod compute-unroll-reminder ((reminder (eql :idiv)) size step n-unroll)
  (let* ((id (gensym))
         (g (with-context (out (%mul step (%mul n-unroll (%idiv (%idiv size step) n-unroll)) :id id)))))
    (setf (graph-outputs g) (list id))
    g))

(defun ast-upcast-body (graph body idx n)
  "Inserts SWIZZLE[N] to the body"
  (declare (type Graph graph) (type symbol idx) (type fixnum n))
  
  )

(defun ast-unroll-body (graph body idx n)
  "Removes IDX from body by unrolling with N"
  (declare (type graph graph) (type symbol idx) (type fixnum n))
  (let ((nodes (apply #'make-graph (ast-band-children graph body)))
        (out (second (node-reads body))))
    (setf (graph-outputs nodes) (list out)
          nodes (graph-nodes (->graph-with-tpsort (->fast-graph nodes)))
          nodes (loop for node in nodes unless (eql (node-type node) :RANGE) collect node))
    (labels ((%cpy-node (node)
               (let ((node (copy-node node)))
                 (setf (node-id node) (gensym "NID"))
                 node))
             (is-range-p (node &aux (node (id->value graph node)))
               (and node (eql (node-type node) :RANGE) (eql idx (getattr node :idx))))
             (unroll-id (cnt id &aux (val (id->value graph id)))
               (declare (type fixnum cnt) (type symbol id))
               (if val
                   (if (find id nodes :key #'node-writes :test #'find)
                       (intern (format nil "~a_~a" id cnt))
                       id)
                   id))
             (unroll-with-count (count &aux (seen (make-hash-table)))
               (flet ((getid (id cnt)
                        (or
                         (gethash id seen)
                         (setf (gethash id seen)
                               (if (is-range-p id)
                                   (node->id1 cnt)
                                   (unroll-id count id))))))
                 (with-context
                   (cnt (%iconst count))
                   (_ (loop for node_ in nodes for node = (%cpy-node node_)
                            do (setf (node-reads node) (map 'list #'(lambda (x) (getid x cnt)) (node-reads node))
                                     (node-writes node) (map 'list #'(lambda (x) (getid x cnt)) (node-writes node)))
                               (emit node)))))))
      (apply
       #'%progn
       (loop for i upfrom 0 below n
             for unrolled = (unroll-with-count i)
             for end = (intern (format nil "~a_~a" out i))
             do (insert-nodes graph (graph-nodes unrolled))
             collect end)))))

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

(defun ast-band-unroll (graph band local-sizes &key (reminder :idiv) (dtype :int64) (rewriter #'ast-unroll-body) &aux (n-unroll (car local-sizes)))
  (assert (= 1 (length local-sizes)) () "ast-band-unroll: the length of local-sizes must be one.")
  (let ((range (id->value graph (car (node-reads band)))))
    (assert (and range (eql (node-type range) :RANGE)))
    (multiple-value-bind (graph global-bands local-bands) (%ast-band-tile graph band local-sizes)
      ;; The work here is to remove away local-bands
      ;; also inserting reminder bands in the gloal-bands
      ;; [TODO] Insert Reminder Statements
      ;; [TODO] How to determine the unrolled variable index? it depends on time-series dependencies?
      ;; note: do not run verify-graph during %ast-band-tile
      (flet ((expr-out (id &aux (node (id->value graph id)))
               (if (numberp id)
                   id
                   (car (node-reads node)))))
        (let* ((reminder-graph (compute-unroll-reminder reminder (expr-out (car (node-reads range))) (expr-out (second (node-reads range))) n-unroll))
               (reminder-expr (%expr (car (graph-outputs reminder-graph))))
               (new-step (%mul n-unroll (second (node-reads range))))
               (new-step-expr (%expr (node->id1 new-step)))
               (idx1 (getattr (id->value graph (car (node-reads (car global-bands)))) :idx))
               (idx2 (getattr (id->value graph (car (node-reads (car local-bands)))) :idx))
               (range1 (make-node :Render :RANGE (list (gensym)) (list (node->id1 reminder-expr) (node->id1 new-step-expr))
                                  :idx idx1 :dtype dtype))
               (reminder-size-tmp (%neg (car (graph-outputs reminder-graph))))
               (reminder-size (%add (expr-out (car (node-reads range))) reminder-size-tmp))
               (reminder-size-expr (%expr (node->id1 reminder-size)))
               (range2 (make-node :Render :RANGE (list (gensym)) (list (node->id1 reminder-size-expr) (second (node-reads range)))
                                  :idx idx2 :dtype dtype))
               (body1 (funcall rewriter graph (car local-bands) idx2 n-unroll)) ;; Unrolled body
               (body2 (ast-unroll-reminder graph (car local-bands) idx1 (car (graph-outputs reminder-graph)))) ;; Reminder body (idx2 is rewritten as idx2 + reminder_graph.out)
               (main-band (make-node :Render :FOR (list (gensym)) (list (node->id1 range1) (node->id1 body1)) :mark :noopt))
               (reminder-band (make-node :Render :FOR (list (gensym)) (list (node->id1 range2) (node->id1 body2)) :mark :noopt))
               (prgn (%bind (car (node-writes (car global-bands))) (%progn main-band reminder-band))))
          (let ((nodes
                  (append
                   (graph-nodes reminder-graph)
                   (list new-step new-step-expr reminder-size-tmp reminder-expr reminder-size-expr
                         body1 body2 range
                         range1 reminder-size range2 main-band reminder-band prgn))))
            (insert-nodes graph (apply #'append (map 'list #'node-force-number-bypass nodes)))
            graph))))))

(defun ast-band-upcast (graph band local-sizes &key (reminder :idiv) (dtype :int64))
  "Similar to ast-band-unroll but this function upcasts the band (corresponding to swizzling/vectorizing in CPU/GPU, maximizing the innermost loop parallelism)"
  (declare (type Graph graph) (type node band) (type list local-sizes))
  ;; Implementing this
  (ast-band-unroll graph band local-sizes :reminder reminder :dtype dtype :rewriter #'ast-upcast-body))

(defun ast-band-parallelize ()) ;; Not an tile but uses the depth of band to mark for collapse(N), reuse tile

(defun expr-get-reduced-to (graph expr)
  (declare (type graph graph) (type symbol expr))
  ;; Assuming a tree like EXPR(SETF(X, Y))
  ;; If X is ReduceOps, returns (id->value graph x)
  (let* ((entry (id->value graph expr))
         (x (and entry (id->value graph (car (node-reads entry)))))
         (y (and entry (id->value graph (second (node-reads entry))))))
    (when (and entry (eql (node-type entry) :SETF) x y (getattr y :reduction :allow-undefined t))
      ;; (list load alu)
      (list x (copy-node y) entry))))

(defun progn->expr-list (graph progn)
  (declare (type Graph graph) (type node progn))
  (assert (eql (node-type progn) :PROGN))
  (loop for r in (node-reads progn)
        do (assert (and (id->value graph r) (eql (node-type (id->value graph r)) :EXPR)) () "progn should be a list of EXPR.")
        collect (car (node-reads (id->value graph r)))))
;;; OptOps (Shared Memory Transfer)
(defun ast-band-group (graph band size)
  "Optimization for GPU"
  (declare (type FastGraph graph) (type node band) (type list size))
  (assert (eql (node-type band) :FOR) () "ast-band-group: The given band is not :FOR.")
  (assert (eql (getattr band :mark) :reduction) () "ast-band-group is only applicable for :reduction.")
  (assert (= (length size) 1))
  (ast-infer-typed-node graph)
  (multiple-value-bind (graph global-bands local-bands) (%ast-band-tile graph band size)
    (let* ((body (id->value graph (second (node-reads (car (last local-bands))))))
           (range (car (node-reads (car (last local-bands)))))
           (rid (getattr (id->value graph range) :idx))
           (reductions
             (ecase (node-type body)
               (:EXPR (list (expr-get-reduced-to graph (car (node-reads body)))))
               (:PROGN (map 'list #'(lambda (x) (expr-get-reduced-to graph x)) (progn->expr-list graph body))))))
      (flet ((lid (node nth) (intern (format nil "s~(~a~)_~a" (car (node-writes node)) nth))))
        (insert-nodes
         graph
         (with-context-nodes
             (uload
              (%bind
               (second (node-reads (car global-bands)))
               (apply
                #'%progn
                ;; Shared Memory Declaration for local buffers and initial value.
                (append
                 (loop for reduce in reductions
                       for l = (car reduce)
                       for id = (when l (lid l 0))
                       for id1 = (when l (lid l 1))
                       when l
                         append
                         (list
                          (%bind id (%defsmem :size size :dtype (typed-dtype (car (getattr l :dst-types)))))
                          (%dotimes (tmpgid (reduce #'* size) :mark :noopt) ;; expecting the loop to be unrolled away ...
                            (%expr (node->id1 (%setf (%aref id tmpgid) (car (node-reads l)))) :out id1))))
                 (list
                  (%dotimes (tmpgid (reduce #'* size) :mark :noopt :range range)
                    (apply
                     #'%progn
                     (loop for reduce in reductions
                           for l = (car reduce) for alu = (second reduce)
                           for id1 = (when l (emit (make-node :JIT :BIND (list (lid l 3)) (list (lid l 1)) :value (lid l 0))))
                           for id2 = (when l (lid l 2))
                           for body = (id->value graph (second (node-reads (car (last local-bands)))))
                           when l do (setf alu (copy-node alu)
                                           (node-id alu) (gensym "N")
                                           (node-writes alu) (list (gensym "W"))
                                           (car (node-reads alu)) (node->id1 (%aref id1 rid)))
                                     (assert (= 2 (length (node-reads alu))) () "alu ~a is not binaryops" alu)
                                     (emit alu)
                           and append (when (and body (eql (node-type body) :PROGN) (> (length (node-reads body)) 1)) (butlast (node-reads body)))
                           and collect (%expr (node->id1 (%setf (%aref id1 rid) (node->id1 alu))) :out id2)))))
                 (list (%barrier))
                 (list
                  (%when
                   (%= nil :row (car (node-reads (car global-bands))) (%iconst 0))
                   (%dotimes (tmpgid (reduce #'* size) :mark :noopt)
                     (apply
                      #'%progn
                      (loop for reduce in reductions
                            for l = (car reduce) for alu = (second reduce) for e = (third reduce)
                            for id2 = (when l (emit (make-node :JIT :BIND (list (gensym)) (list (lid l 3)) :value (lid l 0))))
                            when l do (setf alu (copy-node alu)
                                            (node-id alu) (gensym "N")
                                            (node-writes alu) (list (gensym "W"))
                                            (node-reads alu) (list (node->id1 l) (node->id1 (%aref id2 tmpgid))))
                                      (emit alu)
                            and collect (%expr (node->id1 (%setf l (node->id1 alu))) :out (lid l 4)))))))))))))
        (loop for node in (graph-nodes graph)
              when (eql (node-type node) :BIND) do
                (loop for reduce in reductions
                      for l = (first reduce) for final-id = (when l (lid l 4))
                      if (and l (eql (getattr node :value) (car (node-writes l)))) do
                        (let ((n (copy-node node)))
                          (setf (node-reads n) (list final-id))
                          (insert-nodes graph (list n))))))
      (verify-graph graph)
      graph)))
;; TODO: Matmul is this:
;; - Old Auto Schedulerと同じ方針でOK
;; - oneDNN Graph Compiler
;; https://github.com/siboehm/SGEMM_CUDA/blob/master/src/kernels/10_kernel_warptiling.cuh#L50
(defun ast-band-prefetch (graph band size)
  "Prefetch will first transfers all buffers used in the reduction band to the shared memory, and performs
the reduction in only the cached region."
  (declare (type FastGraph graph) (type node band) (type list size))
  (assert (eql (node-type band) :FOR) () "ast-band-prefetch: The given band is not :FOR.")
  (assert (eql (getattr band :mark) :reduction) () "ast-band-prefetch is only applicable for :reduction.")
  ;; groupの_gid2_pのBandにval_19のPrefetchを追加すればOK?
  (print graph)
  (print band)
  graph)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; Simplifier Things:
;; - [ ] Remove :GLOBAL :LOCAL IF Guard which is rebundant
;; - [ ] :LOAD is always an const define-global
;; - [ ] RANGE(1) ==> Remove
;; - [ ] TileBands, Remove MAX if unnecessary.
;; - [ ] GID Count is GLOBAL
;; - [x] Softmax, Reduce is lowered as _GID2_1, _GID2_2, ...
;; Optimizer Things:
;; - [ ] BEAM Search
;; - [ ] Matmul ---> Block Warp Reduction is effective for both GPU and CPU.
;;   - [ ] https://github.com/siboehm/SGEMM_CUDA/blob/master/src/kernels/10_kernel_warptiling.cuh (Prefetch, effective for CPU and GPU)
;; - [x] Softmax --> Implement Block Reduction
;; - [ ] Get optimal scheduling for Softmax on GPU manually
;; - [ ] Get optimal scheduling for Matmul on GPU manually
;; - [x] Finish Implementing Unroll
;; - [ ] (Unroll) --> Unroll blockIdx.x (First Priority before doing upcast)
;; - [ ] Implenment Swizzle/Upcast/Vectorize, Support tmp.x. (Add Ops for UNROLL(BIND, X)
;;  - [ ] ReExprify
;; More Things:
;; - [x] Add: tensor-schedule-graph
;; - [ ] Add: ast-finalize-graph -> Propagate all :LOAD
;; Finish valid codegen workload
;; - (with-inference-mode () (caten (forward (caten/nn:Embedding 128 128) (make-tensor `(128 128)))))
;;  - val_7を取り除く (PROGNでReadされてるから消えない)
;; - Matmul -> ADDMUL Fusion (ReExprify)
;; - Manual Scheduler Example
;; - Bring Back Memory Planner
;;   - CoincidentなArefのNAMEを書き換えるだけで完了
;; - Schedule Cache
;; - CodegenをUpdate, 全てのテストをPassする
;; - OptOpsに取り掛かる
;; - All node ends with SETF?
;; - Move CStyleRenderer -> byoc/renderers/cstyle.lisp
;; - optimize pprint-graph!!
;; - codegenが動くようにする
;; - [ ] There is a bug in type inference
;; - GLOBAL+UNROLl/UPCAST?
;; - [ ] BEAM Search --> CUDA Graph Capturingみたいな感じで，TQDMで進捗表示
