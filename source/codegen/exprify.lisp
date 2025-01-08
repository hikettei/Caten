(defpackage :caten/codegen/exprify
  (:documentation "
The package `caten/codegen/exprify` is responsible for providing a rewriting-rule targeting the EXPR node.
")
  (:use :cl :caten/air :caten/codegen/expr :caten/codegen/expr-cache :caten/codegen/shape-inference :caten/runtime/buffer)
  (:import-from
   :caten/codegen/renderer
   #:make-aref)
  (:import-from
   :caten/codegen/helpers
   :nodes-write-to
   :nodes-depends-on)
  (:export
   #:blueprint-scalarify
   #:blueprint-exprify
   #:blueprint-set-iterations
   #:blueprint-propagate-reduction
   #:blueprint-realized-buffers))

(in-package :caten/codegen/exprify)

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
  (let ((buffer (copy-buffer buffer)))
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
               :_type_relay (caten/codegen/shape-inference::copy-inferred-type (read-type-relay node))
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

(defmethod blueprint-scalarify ((blueprint list) (node Node) (base-graph Graph) &aux (seen (make-hash-table)) (io (append (node-reads node) (node-writes node))))
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
                  for parent = (find r blueprint :key #'node-writes :test #'find)
                  for parent-reduce-p = (and parent (getattr parent :reduction :allow-undefined t))
                  for definition = (id->value base-graph r)
                  for nth upfrom 0
                  if (and rt ri (local-p r) (or (null parent-reduce-p) (getattr parent :declare-type))) do
                    (setf (nth nth (relay-reads (read-type-relay bp))) (buffer-scalarify rt))
                  else if (and definition (= 0 (buffer-nrank (car (relay-writes (read-type-relay definition)))))) do
                    (let ((s (buffer-scalarify rt)))
                      (setf (buffer-nrank s) 0
                            (nth nth (relay-reads (read-type-relay bp))) s)))
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

(defun expr-writes (expr)
  (loop for item in (graph-nodes (expr-graph (getattr expr :expr)))
        if (eql (node-type item) :Aref)
          collect (getattr item :storage-id)
        else
          append (node-writes item)))

(defun blueprint-exprify (blueprint node &key (seen (make-hash-table)) (io (append (node-reads node) (node-writes node))))
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
                  (or (null current-pair) (null (getattr current-pair :reduction))) ;; do not the chain reduction
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

(defun blueprint-set-iterations (blueprint)
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

(defun blueprint-make-alias-map (blueprint)
  (let ((alias-map (make-hash-table)))
    (loop for bp in blueprint
          ;; 1. val_1[1, 10] += val[10, 10] is a reduction, pointer propagation is effected in the only current blueprint (apply here)
          ;; 2. val_1[10, 10] += val[10, 10] is an assign. (val_1 and val must indicate the same buffer)
          if (and (eql (node-type bp) :EXPR) (getattr bp :reduction))
            do (if (node-writes-broadcasted-p bp)
                   (setf (gethash (car (node-writes bp)) alias-map)
                         (list (car (node-reads bp)) (car (relay-reads (read-type-relay bp))) (car (relay-read-iters (read-type-relay bp)))))
                   (setf (gethash (car (node-writes bp)) alias-map) (list (car (node-reads bp)) nil nil))))
    alias-map))

(defun rewrite-expr-aref (expr replace)
  (declare (type graph expr))
  (dolist (n (graph-nodes expr))
    (if (eql (node-type n) :AREF)
        (multiple-value-bind (id type is) (funcall replace (getattr n :storage-id) (getattr n :buffer) (getattr n :space))
          (assert (and id type is))
          (setf (getattr n :storage-id) id (getattr n :buffer) type  (getattr n :space) is))
        (setf (node-reads n) (map 'list #'(lambda (x) (or (funcall replace x nil nil) x)) (node-reads n))))))

(defun blueprint-propagate-reduction (blueprint)
  "Rewrite the following EXPR e.g.:
```
A <- B, C // :reduction=t
```
into:
```
B <- C // :reduction=t
```
"
  (declare (type list blueprint))
  (let ((id->tgt (blueprint-make-alias-map blueprint))
        (id-as-dag (make-hash-table)))
    (labels ((final-new-id (id)
               (if (gethash id id->tgt)
                   (or (final-new-id (car (gethash id id->tgt))) (gethash id id->tgt))
                   (gethash id id->tgt)))
             (new (id type is)
               (let ((new-id (final-new-id id)))
                 (if (null new-id)
                     (values id type is)
                     (progn
                       (setf (gethash (car new-id) id-as-dag) id)
                       (values (car new-id) (or (second new-id) type) (or (third new-id) is)))))))
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
        (values blueprint id-as-dag)))))

(defun blueprint-realized-buffers (blueprint sched-item)
  (declare (type list blueprint) (type node sched-item))
  (assert (eql (node-type sched-item) :Schedule-Item))
  (let ((seen (apply #'append (map 'list #'node-writes blueprint)))
        (candidates (append (node-writes sched-item) (node-reads sched-item))) (constants) (writes) (reads)) ;; constants = buffers used in the stride/view/shape computation
    (loop for bp in blueprint
          for f = (if (eql (node-type bp) :EXPR) #'expr-node-realized-buffers #'render-realized-buffers) do
            (multiple-value-bind (new-writes new-reads new-consts) (funcall f bp :candidates candidates :written seen)
              (setf writes (append writes new-writes) reads (append reads new-reads) constants (append constants new-consts))))
    (setf
     writes (remove-duplicates writes :key #'car)
     reads (remove-duplicates reads :key #'car)
     constants (remove-duplicates constants :key #'car)) ;; TODO(hikettei) What is one is loaded as a float, while another is loaded as a uint?
    (dolist (io (intersection writes reads :key #'car)) ;; if theres input & output, they are output.
      (setf reads (remove (car io) reads :key #'car)))
    (assert (every #'buffer-p (map 'list #'cdr writes)))
    (assert (every #'buffer-p (map 'list #'cdr reads)))
    (assert (every #'keywordp (map 'list #'cdr constants)))
    (values writes reads constants)))

(defun iterspace-depend-consts (is &key (written))
  (let ((consts))
    ;; depends on = strides, views
    (assert (= (length (iteration-space-shape is)) (length (iteration-space-strides is))))
    (loop for s in (iteration-space-strides is)
          for v in (iteration-space-views is)
          for offset = (first v)
          for by = (third v) do
            (when (and offset (symbolp offset) (not (find offset written)))
              (push (cons offset caten/aasm:*default-int*) consts))
            (when (and by (symbolp by) (not (find by written)))
              (push (cons by caten/aasm:*default-int*) consts))
            (multiple-value-bind (rs cs) (expr-realized-buffers s :written written)
              (assert (null rs))
              (loop for c in cs
                    if (null (find (car c) written)) do
                      (push c consts))))
    (remove-duplicates consts :key #'car)))

(defun expr-realized-buffers (expr &key (candidates) (written) &aux (args) (constants) (seen) (graph (expr-graph expr)))
  (declare (type expr expr))
  ;; Return: A list of buffer needs to be realized, since it is expr, it can include symbols for computing stride/view
  (labels ((node-jump-to (node)
             (case (node-type node)
               ((:MOVE :CAST :!= :< :INDEX-COMPONENTS :STORE :LOAD) ;; they won't use the first argument
                (append `(nil) (cdr (node-reads node))))
               (otherwise
                (node-reads node))))
           (explore (id)
             (when (find id seen) (return-from explore nil))
             (let ((val (id->value graph id)))
               (when val
                 (push id seen)
                 (case (node-type val)
                   (:LOAD
                    (when (and (symbolp (getattr val :value)) (null (find (getattr val :value) written)))
                      (if (getattr val :_type_relay :allow-undefined t)
                          (push (cons (getattr val :value) (buffer-dtype (car (relay-writes (read-type-relay val))))) constants)
                          (let ((alloc (id->value graph (car (node-reads val)))))
                            (assert (and alloc (eql (node-type alloc) :Allocate)))
                            (push (cons (getattr val :value) (getattr alloc :dtype)) constants)))))
                   (:AREF
                    (when (find (getattr val :storage-id) candidates)
                      (setf constants (append constants (iterspace-depend-consts (getattr val :space) :written written)))
                      (push (cons (getattr val :storage-id) (getattr val :buffer)) args)))
                   (otherwise
                    (when (null (getattr val :_type_relay :allow-undefined t))
                      (mapc #'explore (node-jump-to val))
                      (return-from explore))
                    (loop for next-val in (node-jump-to val)
                          for next-type in (relay-reads (read-type-relay val))
                          for next-iter in (relay-read-iters (read-type-relay val))
                          for next-node = (when next-val (id->value graph next-val))
                          ;; candidate assumes next-val is not written by any other local blueprint.
                          if (and (null next-node) (find next-val candidates)) do
                            (setf constants (append constants (iterspace-depend-consts next-iter :written written)))
                            (push (cons next-val next-type) args))))
                 (mapc #'explore (node-jump-to val))))))
    ;; Read dependencies
    (mapc #'explore (node-writes (expr-out expr))))
  (values args constants))

(defun expr-node-realized-buffers (expr-node &key (candidates) (written))
  (multiple-value-bind (reads consts) (expr-realized-buffers (getattr expr-node :EXPR) :candidates candidates :written written)
    (let ((writes
            (loop for w in (node-writes expr-node)
                  for wt in (relay-writes (read-type-relay expr-node))
                  for wi in (relay-write-iters (read-type-relay expr-node))
                  if (and wt wi (not (= -1 (buffer-nrank wt))))
                    do (setf consts (append consts (iterspace-depend-consts wi :written written))) and
                  collect (cons w wt))))
      (values writes reads consts))))

(defun render-realized-buffers (render &key (candidates) (written))
  (declare (type node render))
  (assert (eql (node-class render) :Render))
  (ecase (node-type render)
    (:IF (expr-realized-buffers (getattr render :condition) :candidates candidates :written written))
    (:ENDIF)
    (:FOR
     (multiple-value-bind (rs1 cs1) (expr-realized-buffers (getattr render :upfrom) :candidates candidates :written written)
       (multiple-value-bind (rs2 cs2) (expr-realized-buffers (getattr render :below) :candidates candidates :written written)
         (values nil (append rs1 rs2) (append cs1 cs2)))))
    (:ENDFOR)))
