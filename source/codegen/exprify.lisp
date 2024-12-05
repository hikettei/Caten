(defpackage :caten/codegen/exprify
  (:documentation "
The package `caten/codegen/exprify` is responsible for providing a rewriting-rule targeting the EXPR node.
")
  (:use :cl :caten/air :caten/codegen/expr :caten/codegen/expr-cache)
  (:import-from
   :caten/codegen/shape-inference
   #:read-type-relay
   #:relay-reads
   #:relay-writes
   #:relay-read-iters
   #:relay-write-iters
   #:iteration-space-shape
   #:ensure-iteration-space-length
   #:node-writes-broadcasted-p)
  (:import-from
   :caten/avm
   #:Buffer
   #:Buffer-depend-idx-list
   #:buffer-nrank
   #:buffer-dtype)
  (:import-from
   :caten/codegen/renderer
   #:make-aref)
  (:import-from
   :caten/codegen/helpers
   :nodes-write-to
   :nodes-depends-on)
  (:export
   #:graph-scalarify
   #:graph-exprify
   #:graph-propagate-pointer-id-type
   #:expr-set-iterations
   #:expr-rewrite-edge-with-pointer-id))

(in-package :caten/codegen/exprify)

(defun force-realize-p (node schedule-graph)
  (when (null (getattr node :reduction :allow-undefined t))
    (return-from force-realize-p nil))
  (let ((parent (id->value schedule-graph (car (node-reads node)))))
    (when (null parent) (return-from force-realize-p nil))
    (when (not (getattr parent :allocate-p))
      (return-from force-realize-p nil))
    (let ((allocate (car (getattr parent :Items))))
      (assert (eql (node-type allocate) :Allocate))
      (if (getattr allocate :from) t nil))))

(defun memory-access-local-p (blueprint id)
  (declare (optimize (speed 3))
           (type list blueprint)
           (type symbol id))
  (let ((search-key
          (loop for bp in blueprint
                for nth fixnum upfrom 0
                if (and (not (eql (node-class bp) :Render)) (or (find id (node-reads bp)) (find id (node-writes bp))))
                  collect nth)))
    (assert search-key () "The id ~a is not used in the bp." id)
    (loop with start fixnum = (apply #'min search-key)
          with end fixnum = (apply #'max search-key)
          with depth fixnum = 0
          for nth upfrom start to end
          for ir = (nth nth blueprint)
          if (find (node-type ir) `(:FOR :IF))
            do (incf depth)
          else if (find (node-type ir) `(:ENDFOR :ENDIF))
                 do (decf depth)
          end
          if (< depth 0) do (return-from memory-access-local-p nil))
    t))

(defun buffer-scalarify (buffer)
  (let ((buffer (caten/avm:copy-buffer buffer)))
    (setf (buffer-nrank buffer) -1)
    buffer))

(defmethod propagate-load-const ((node Node) bp)
  (loop for nth upfrom 0
        for r in (node-reads node)
        for n = (find r bp :key #'node-writes :test #'find)
        if (and (not (eql (node-class node) :Render))
                (and n (eql (node-type n) :LOAD)) ;; Propagate only scalars
                (numberp (getattr n :value))
                (= (buffer-nrank (car (relay-writes (read-type-relay n)))) 0))
          do (setf (nth nth (node-reads node)) (getattr n :value))))

(defun remove-unused-blueprint (blueprint sched-item)
  (declare (type list blueprint) (type node sched-item) (optimize (speed 3)))
  (loop with reads = (apply #'append (map 'list #'node-reads blueprint))
        for bp in blueprint
        for used-p = (or (find (the symbol (car (node-writes bp))) (the list (node-writes sched-item)))
                         (find (the symbol (car (node-writes bp))) (the list reads)))
        if (or (eql (node-class bp) :Render) used-p)
          collect bp))

(defmethod exprify ((node Node))
  "Mutates the node into EXPR."
  (let ((nth->aref (make-hash-table)))
    (make-node :JIT :EXPR (copy-list (node-writes node)) (copy-list (node-reads node))
               :reduction (getattr node :reduction :allow-undefined t)
               :_type_relay (read-type-relay node)
               :declare-type (getattr node :declare-type)
               :expr (make-expr
                      :graph
                      (apply
                       #'make-graph
                       (append
                        (loop for r in (node-reads node)
                              for rt in (relay-reads (read-type-relay node))
                              for ri in (relay-read-iters (read-type-relay node))
                              for nth upfrom 0
                              if (symbolp r)
                                append
                                (if (= 0 (buffer-nrank rt))
                                    (let ((nodes (graph-nodes (expr-graph (expr-const r (buffer-dtype rt))))))
                                      (assert (eql (node-type (car (last nodes))) :LOAD))
                                      (setf (node-writes (car (last nodes))) (list r))
                                      nodes)
                                    (let ((newid (gensym "JIT_AREF_TMP")))
                                      (setf (gethash nth nth->aref) newid)
                                      (list (make-aref newid r rt ri)))))
                        (let ((node (copy-node node)))
                          (setf (node-reads node)
                                (loop for i upfrom 0
                                      for r in (node-reads node)
                                      collect (or (gethash i nth->aref) r)))
                          (list node))))
                      :out node))))

(defun merge-and-graft-expr (node pairs)
  "NODE <- pairs"
  (assert (eql (node-type node) :EXPR))
  (let ((ids) (types) (iters) (writes))
    (loop for (id . expr-parent) in pairs
          do (expr-graft-after (getattr node :expr) id (getattr expr-parent :expr))
             (setf writes (append writes (node-writes expr-parent)))
             (loop for read in (node-reads expr-parent)
                   for type in (relay-reads (read-type-relay expr-parent))
                   for iter in (relay-read-iters (read-type-relay expr-parent))
                   do (push read ids) (push type types) (push iter iters)))
    (setf (node-reads node) (append (node-reads node) (reverse ids))
          (relay-reads (read-type-relay node)) (append (relay-reads (read-type-relay node)) (reverse types))
          (relay-read-iters (read-type-relay node)) (append (relay-read-iters (read-type-relay node)) (reverse iters)))
    (let ((gather-map
            (loop for r in (node-reads node)
                  for nth upfrom 0
                  if (find r writes)
                    collect nil
                  else
                    collect t)))
      (setf (node-reads node)
            (loop for r in (node-reads node)
                  for g in gather-map
                  if g collect r)
            (relay-reads (read-type-relay node))
            (loop for r in (relay-reads (read-type-relay node))
                  for g in gather-map
                  if g collect r)
            (relay-read-iters (read-type-relay node))
            (loop for r in (relay-read-iters (read-type-relay node))
                  for g in gather-map
                  if g collect r)))
    node))

(defmethod graph-scalarify ((blueprint list) (node Node) &aux (seen (make-hash-table)) (io (append (node-reads node) (node-writes node))))
  "Only the following buffers are realized:
- appeared in either of (node-reads node) or (node-writes node)
- the access is not completed in the innermost loop"
  (flet ((local-p (id)
           (when (find id io) (return-from local-p nil))
           (when (not (symbolp id)) (return-from local-p nil))
           (if (gethash id seen)
               (eql (gethash id seen) :yes)
               (let ((result (memory-access-local-p blueprint id)))
                 (setf (gethash id seen) (if result :yes :no))
                 result))))
    (loop with idxs = nil
          for bp in blueprint
          if (eql (node-type bp) :FOR) do
            (push (getattr bp :idx) idxs)
          if (eql (node-type bp) :ENDFOR) do
            (setf idxs (remove (getattr bp :idx) idxs))
          if (not (eql (node-class bp) :Render)) do
            (loop for r in (node-reads bp)
                  for rt in (relay-reads (read-type-relay bp))
                  for ri in (relay-read-iters (read-type-relay bp))
                  for nth upfrom 0
                  if (and rt ri (local-p r)) do
                    (setf (nth nth (relay-reads (read-type-relay bp))) (buffer-scalarify rt)))
            (when (null (getattr bp :reduction :allow-undefined t))
              (loop for w in (node-writes bp)
                    for wt in (relay-writes (read-type-relay bp))
                    for wi in (relay-write-iters (read-type-relay bp))
                    for nth upfrom 0
                    if (and wt wi (local-p w)) do
                      (setf (nth nth (relay-writes (read-type-relay bp))) (buffer-scalarify wt)
                            (getattr bp :declare-type) (list t)))))
    (dolist (node blueprint)
      (propagate-load-const node blueprint))
    (remove-unused-blueprint blueprint node)))

(defmethod graph-exprify ((blueprint list) (node Node) &aux (seen (make-hash-table)) (io (append (node-reads node) (node-writes node))))
  (flet ((local-p (id)
           (when (find id io) (return-from local-p nil))
           (when (not (symbolp id)) (return-from local-p nil))
           (if (gethash id seen)
               (eql (gethash id seen) :yes)
               (let ((result (memory-access-local-p blueprint id)))
                 (setf (gethash id seen) (if result :yes :no))
                 result))))
    (let ((new-bp (map 'list #'(lambda (x) (if (eql (node-class x) :Render) x (exprify x))) blueprint)))
      (labels ((replace-p (id other-pairs current-pair)
                 (and
                  (local-p id) ;; [TODO] is it removable?
                  (= 1 (apply #'+ (map 'list #'(lambda (node) (count id (node-reads node))) blueprint)))
                  (or (null current-pair) ;; no parent                          
                      (null (intersection (expr-writes current-pair) (apply #'append (map 'list #'expr-writes other-pairs)))))))
               (group->expr-group (group &aux (tops (nodes-write-to group)) (graph (apply #'make-graph group)) (seen nil))
                 (setf (graph-outputs graph) tops)
                 (assert (every #'(lambda (x) (eql (node-type x) :EXPR)) group))
                 (labels ((explore (id &aux (node (id->value graph id)))
                            (when (and node (null (find (node-id node) seen)))
                              (push (node-id node) seen)
                              (let* ((parents
                                       (loop for r in (node-reads node)
                                             collect (explore r)))
                                     (stashes)
                                     (rewrite-pairs
                                       (loop with merging-pairs = nil
                                             for r in (node-reads node)
                                             for p in parents
                                             if (and (replace-p r merging-pairs (car (last p))) p)
                                               collect (cons r (car (last p))) and do (push (car (last p)) merging-pairs) ;; The symbol r -> graft p from r.
                                             else
                                               do (push (car (last p)) stashes))))
                                `(,@(apply #'append (map 'list #'butlast parents))
                                  ,@(nreverse stashes)
                                  ,(merge-and-graft-expr node rewrite-pairs))))))
                   (loop for e in (apply #'append (map 'list #'explore tops))
                         if e collect e)))
               (rewriter (from to &aux (group) (new-region :nothing))
                 (loop with count = from while (< count to)
                       for node = (nth count new-bp) do
                         (ecase (node-type node)
                           (:FOR
                            (let* ((endfor
                                     (find (getattr node :Idx) (nthcdr count new-bp)
                                           :key #'(lambda (x) (and (eql (node-type x) :ENDFOR) (getattr x :idx)))))
                                   (_ (when (null endfor) (error "malformed bp ~a" blueprint)))
                                   (endfor-abs-position (position (node-id endfor) new-bp :key #'node-id)))
                              (declare (ignore _))
                              (assert (>= endfor-abs-position count) () "malformed bp ~a" blueprint)
                              (assert (eql (node-type (nth endfor-abs-position new-bp)) :ENDFOR))
                              (setf new-region
                                    `(,@(when (not (eql new-region :nothing))
                                          new-region)
                                      ,@(group->expr-group (reverse group))
                                      ,node
                                      ,@(rewriter (1+ count) endfor-abs-position)
                                      ,(nth endfor-abs-position new-bp))
                                    group nil)
                              (setf count endfor-abs-position)
                              (incf count)))
                           (:ENDFOR
                            (error "malformed bp ~a" blueprint))
                           (:EXPR
                            (push node group)
                            (incf count))))
                 (when group
                   (setf new-region
                         `(,@(when (not (eql new-region :nothing))
                               new-region)
                           ,@(group->expr-group (reverse group)))))
                 (if (eql new-region :nothing)
                     nil
                     new-region)))
        (rewriter 0 (length new-bp))))))

(defun expr-set-iterations (blueprint)
  (loop with gids = nil
        for bp in blueprint
        if (eql (node-type bp) :FOR)
          do (push (getattr bp :idx) gids)
        else if (eql (node-type bp) :ENDFOR)
          do (setf gids (remove (getattr bp :idx) gids))
        else do
          (assert (eql (node-type bp) :EXPR))
          (let* ((is (car (relay-write-iters (read-type-relay bp))))
                 (required-gids (and is (length (iteration-space-shape is)))))
            (if (or gids (null is) (= (length gids) required-gids))
                (setf (getattr bp :iterations) (map 'list #'(lambda (x) (expr-const x :int64)) (reverse gids)))
                (progn
                  ;; Set gid=0 for ops located in outside of a loop.
                  (setf (getattr bp :iterations) (loop repeat required-gids collect (expr-const 0 :int64)))))
            (when is
              (setf (getattr bp :iterations) (ensure-iteration-space-length is (getattr bp :iterations))))))
  blueprint)
;; [TODO] What we are doing here is very simple.
;; [TODO] Reduction ... MOVEはItem内部で完結する
;; [TODO] Assignをどうやって実装するか考える. Memory Planne周り・・・
;; Expr-Realized-Buffersを作成したら，Blueprintにあるnode-reads/node-writesの推論は削除する
(defun graph-propagate-reduction (blueprint node)
  "Rewrite the following EXPR e.g.:
```
A <- B, C // :reduction=t
```
into:
```
B <- C // :reduction=t
```

If each element is broadcasted, otherwise (= assign), register an alias to global variable.
"
  (declare (type list blueprint) (type node node))
  (assert *expr-cache* () "Create an expr-cache first otherwise graph-propagate-reduction cannot communicate the alias of !assign.")
  ;; Assignの場合は，メモリが余計なコピーを作らないことだけを宣言すればいい
  ;; 1. Reduction/AssignのBuffer TypeをSchedule Type内部のみでUpdateする (since the output cannot be a reduction rule)
  ;; ^ LPARALLELで実行順が合わなくなる -> ALIASの遷移先がおかしくなる
  ;; 2. expr-realized-buffersでIOを確定させる
  )

(defun graph-finalize-realize (blueprint node)

  )

(defun expr-realized-buffers (expr)
  (declare (type Expr expr))
  ;; Return: A list of buffer needs to be realized, since it is expr, it can include symbols for computing stride/view
  
  )

(defun node-realized-buffers (node)
  (if (eql (node-type node) :EXPR)
      (expr-realized-buffers node)
      (render-realized-buffers node)))
;; ~~~ OLD ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun expr-writes (expr)
  (loop for item in (graph-nodes (expr-graph (getattr expr :expr)))
        if (eql (node-type item) :Aref)
          collect (getattr item :storage-id)
        else
          append (node-writes item)))

(defun expr-only-leaf-are-arguments (nodes schedule-graph)
  "Rewrites the expr in nodes, to have only the leaf nodes as an argument."
  (let* ((tmp->id (make-hash-table)))
    (dolist (node nodes)
      (when (eql (node-type node) :EXPR)
        (dolist (item (graph-nodes (expr-graph (getattr node :expr))))
          (when (eql (node-type item) :Aref)
            (setf (gethash (car (node-writes item)) tmp->id) (getattr item :storage-id))))))
    (flet ((newid (x) (or (gethash x tmp->id) x)))
      (loop for node in nodes
            if (eql (node-type node) :EXPR) do
              (let* ((allocated-but-not-used
                       (loop with expr = (expr-graph (getattr node :EXPR))
                             for node in (graph-nodes expr)
                             ;; See renderer.lisp, MOVE first argument is not rendered for example.
                             ;; [Note] Add more nodes if you found an argument which is actually rendered but not used in the rendered kernel.
                             if (and
                                 (find (node-type node) `(:MOVE :CAST :!= :< :INDEX-COMPONENTS :LOAD :STORE))
                                 (id->value schedule-graph (newid (car (node-reads node))))
                                 (getattr (id->value schedule-graph (newid (car (node-reads node)))) :allocate-p)
                                 (if (eql (node-type node) :LOAD)
                                     ;; x = load(x) is reading an output from another schedule item.
                                     (not (eql (car (node-writes node)) (getattr node :value)))
                                     t))
                               collect (newid (car (node-reads node)))
                             if (and
                                 (not (eql (node-type node) :Aref))
                                 (if (eql (node-type node) :LOAD)
                                     (not (eql (car (node-writes node)) (getattr node :value)))
                                     t))
                               collect (car (node-writes node))))
                     (reads
                       (loop for read in (node-reads node)
                             for rt in (relay-reads (read-type-relay node))
                             for ri in (relay-read-iters (read-type-relay node))
                             if (and (symbolp read) (not (find read allocated-but-not-used)))
                               collect (list read rt ri))))
                (setf (node-reads node) (map 'list #'first reads)
                      (relay-reads (read-type-relay node)) (map 'list #'second reads)
                      (relay-read-iters (read-type-relay node)) (map 'list #'third reads))))))
  nodes)
;; [TODO] Clean up this function!
(defun graph-propagate-pointer-id-type (blueprint schedule-graph)
  (assert *expr-cache*)
  (let ((rewrite-map (make-hash-table))
        ;; [TODO] Stop globalize them
        (id->tgt (expr-cache-reduce-alias *expr-cache*))) ;; id -> (list new_id new_type new_is)
    (loop for bp in blueprint
          if (and (eql (node-type bp) :EXPR) (getattr bp :reduction))
            do (setf (gethash (car (node-writes bp)) id->tgt)
                     (if (node-writes-broadcasted-p bp)
                         ;; val[1, 10] += val[10, 10] is a reduction.
                         (list (car (node-reads bp)) (car (relay-reads (read-type-relay bp))) (car (relay-read-iters (read-type-relay bp))))
                         ;; val[10, 10] += val[10, 10] is just an assign.
                         (list (car (node-reads bp)) nil nil))))
    (labels ((final-new-id (id)
               (if (gethash id id->tgt)
                   (or (final-new-id (car (gethash id id->tgt))) (gethash id id->tgt))
                   (gethash id id->tgt)))
             (new (id type is)
               (let ((new-id (final-new-id id)))
                 (if (null new-id)
                     (values id type is)
                     (progn
                       (setf (gethash id rewrite-map) (car new-id))
                       (values id (or (second new-id) type) (or (third new-id) is)))))))
      (macrolet ((updt (expr &key (reader) (ireader) (treader))
                   `(loop for nth upfrom 0
                          for read in (,reader ,expr)
                          for is in (,ireader (read-type-relay ,expr))
                          for type in (,treader (read-type-relay ,expr))
                          if (and read is type (symbolp read)) do
                            (multiple-value-bind (newid new-type new-is) (new read type is)
                              (assert (and newid new-type new-is))
                              (setf (nth nth (,reader ,expr)) newid
                                    (nth nth (,ireader (read-type-relay ,expr))) new-is
                                    (nth nth (,treader (read-type-relay ,expr))) new-type)))))
        (loop for bp in blueprint
              if (eql (node-type bp) :EXPR)
                do (updt bp :reader node-reads :ireader relay-read-iters :treader relay-reads)
                   (updt bp :reader node-writes :ireader relay-write-iters :treader relay-writes)
                   (rewrite-expr-aref (expr-graph (getattr bp :expr)) #'new))
        (values (expr-only-leaf-are-arguments blueprint schedule-graph) rewrite-map)))))

(defun rewrite-expr-aref (expr replace)
  (declare (type graph expr))
  (dolist (n (graph-nodes expr))
    (if (eql (node-type n) :AREF)
        (multiple-value-bind (id type is) (funcall replace (getattr n :storage-id) (getattr n :buffer) (getattr n :space))
          (setf (getattr n :storage-id) id
                (getattr n :buffer) (or type (getattr n :buffer))
                (getattr n :space) (or is (getattr n :space))))
        (setf (node-reads n)
              (map
               'list
               #'(lambda (x)
                   (let ((id (funcall replace x nil nil)))
                     (or id x)))
               (node-reads n))))))

(defun expr-rewrite-edge-with-pointer-id (blueprint map)
  (labels ((newid (id &optional _ __) (declare (ignore _ __))
             (if (gethash id map)
                 (newid (gethash id map))
                 id)))
      (macrolet ((updt (expr &key (reader))
                   `(loop for nth upfrom 0
                          for read in (,reader ,expr)
                          if (and read (symbolp read)) do
                            (setf (nth nth (,reader ,expr)) (newid read)))))
        (loop for bp in blueprint
              if (eql (node-type bp) :EXPR) do
                (updt bp :reader node-reads)
                (updt bp :reader node-writes)
                (rewrite-expr-aref (expr-graph (getattr bp :expr)) #'newid)))))
