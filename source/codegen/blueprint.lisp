(defpackage :caten/codegen/blueprint
  (:documentation "
```
(lower-schedule-item [Schedule-Item] ...) => [Blueprint]
```
The package `caten/codegen/blueprint` is responsible for lowering the schedule-item into a blueprint. A blueprint is an IR that represents a computation graph with explicit loop bounds.
The `lower-schedule-item` method infers loop boundaries based on `Schedule-item` and performs lowering into a format that includes :FOR/:ENDFOR nodes.")
  (:use :cl :caten/air :caten/aasm :caten/aasm/expr :alexandria :caten/codegen/iteration :caten/codegen/helpers :caten/runtime/runtime :caten/runtime/buffer)
  (:import-from :caten/codegen/renderer #:render-expr #:Default-Renderer #:render-node)
  (:import-from :caten/codegen/rewriting-rules #:nodes-apply-static-gensym)
  (:import-from :caten/codegen/realize #:bp-finalize-realize)
  (:export
   #:lower-schedule-item
   #:lower-cached-schedule-item
   #:print-blueprint
   #:blueprint-gather-grids)
  ;; GFlops Mesaurer
  (:export
   #:GFlops-Measurer
   #:GFlops-Measurer-ops
   #:GFlops-Measurer-succeed-p
   #:compute-gflops
   #:schedule-item-gflops)
  ;; Verifier
  (:export #:verify-blueprint #:expr-gather-buffer-loads))

(in-package :caten/codegen/blueprint)
;; ~~ Temporary Ops ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defnode (:Tmp :TmpRange) () "" :slots ((size :type Expr) (type :type keyword) (idx)))
(defnode (:Tmp :TmpEndRange) () "" :slots ((idx)))
(defun %make-coincident-range (idx size) (make-node :Tmp :TmpRange (list idx) nil :size size :type :coincident :idx idx))
(defun %make-reduction-range (idx size) (make-node :Tmp :TmpRange (list idx) nil :size size :type :reduction :idx idx))
(defun %make-endrange (idx) (make-node :Tmp :TmpEndRange nil nil :idx idx))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod get-grouped-dims ((graph Graph) (base-graph Graph))
  "Infers the loop boundaries of the graph by finding the common iteration space."
  (let* ((kernel-rank
           (loop for node in (graph-nodes graph)
                 for type = (read-type-relay node)
                 maximize
                 (loop for r in (append (relay-reads type) (relay-writes type))
                       when r maximize (length (tensor-relay-shape r)))))
         (pid2space (make-hash-table :test #'equal))
         (candidates nil))
    ;; Assuming all buffers in the graph have reshaped to `kernel-rank` by the scheduler.
    (labels ((broadcastable-p (a b)
               (or (expr-equal-to a 1)
                   (expr-equal-to b 1)
                   (expr-scalar-equivalent-p a b)))
             (is-one (expr) (expr-equal-to expr 1))
             (check (buffer &key (noopt t))
               (when buffer
                 (let ((space (if noopt
                                  (tensor-relay-iteration-space base-graph buffer)
                                  (tensor-relay-merge-dims base-graph buffer))))
                   (when space
                     (loop for s in (iteration-space-shape space)
                           for p in (iteration-space-procedure space)
                           do (setf (gethash p pid2space)
                                    (if (null (gethash p pid2space))
                                        s
                                        (if (is-one (gethash p pid2space))
                                            s
                                            (gethash p pid2space))))
                              (when (and (= 0 (ctx:getenv :OPTIMIZE)) (not (broadcastable-p (gethash p pid2space) s)))
                                (warn "Detected invaild scheduling: ~a vs ~a are not broadcastable." (gethash p pid2space) s)))))))
             (explore (node &key (noopt t))
               (mapc #'(lambda (x) (check x :noopt noopt)) (relay-reads (read-type-relay node)))
               (mapc #'(lambda (x) (check x :noopt noopt)) (relay-writes (read-type-relay node)))))
      (if (= 0 (ctx:getenv :OPTIMIZE))
          (progn
            (mapc #'explore (graph-nodes graph))
            (setf candidates (hash-table-keys pid2space)))
          (progn
            ;; OPTIMIZE is not set to 0: also register the collapsed axis
            (mapc #'(lambda (x) (explore x :noopt nil)) (graph-nodes graph))
            (setf candidates (hash-table-keys pid2space))
            (mapc #'explore (graph-nodes graph))))
      (let ((new-procedure))
        (dolist (c (sort (copy-list candidates) #'< :key #'length))
          (when (every #'(lambda (x) (null (find x (flatten new-procedure)))) c)
            (push c new-procedure)))
        (loop for i upfrom 0 below kernel-rank
              if (null (find i (flatten new-procedure)))
                do (push (list i) new-procedure))
        (setf new-procedure (sort new-procedure #'< :key #'car))
        (assert (equal (alexandria:flatten new-procedure) (range 0 kernel-rank)))
        (cons
         (map
          'list
          #'(lambda (x)
              (assert (gethash x pid2space) () "the axis ~a is not found from ~a" x (alexandria:hash-table-keys pid2space))
              (gethash x pid2space))
          new-procedure)
         new-procedure)))))

(defmethod fixup-graph-iteration-space ((graph Graph) found-pair g &aux (kernel-rank (apply #'max (alexandria:flatten (cdr found-pair)))))
  "Rewrite the all node buffers to have the common iteration space found by the `get-grouped-dims`. All nodes must have the same ranked buffer in advance. (rewritten by scheduler.lisp)"
  (multiple-value-bind (found-space procedure) (values (car found-pair) (cdr found-pair))
    (labels ((merge-list (proc list)
               (loop for p in proc
                     collect
                     (apply #'expr-mul (map 'list #'(lambda (x) (%expr-const g (nth x list) :int64)) p))))
             (merge-stride (proc list)
               (loop for p in proc
                     collect
                     (let ((strides (map 'list #'(lambda (x) (nth x list)) p)))
                       (%expr-const g (if (find 0 strides :test #'eql) 0 (car (last strides))) :int64))))
             (new-stride (stride view)
               (loop for s in stride
                     for nth upfrom 0
                     for v = (nth nth view)
                     if (and (listp v) (fourth v))
                       collect 0
                     else
                       collect s))
             (merge-view (proc view)
               (loop for p in proc
                     collect
                     (if (= (length p) 1)
                         (nth (car p) view)
                         nil)))
             (fixup-dims (id original-buffer)
               (when (and original-buffer (> (length (tensor-relay-shape original-buffer)) 0))
                 ;; Caten cannot inference where to insert one here.
                 (assert (= (length (tensor-relay-shape original-buffer)) (1+ kernel-rank))
                         ()
                         "(id=~a) Cannot uprank ~a into the space ~a. A original buffer should be upranked by the scheduler in advance.~%~a" id original-buffer found-space graph)
                 (multiple-value-bind (new-shape new-stride new-view)
                     (values (merge-list procedure (tensor-relay-shape original-buffer))
                             (merge-stride procedure (new-stride (tensor-relay-stride original-buffer) (tensor-relay-views original-buffer)))
                             (merge-view procedure (tensor-relay-views original-buffer)))
                   (make-iteration-space
                    :shape new-shape
                    :strides new-stride
                    :views new-view
                    :procedure procedure)))))
      (dolist (n (graph-nodes graph))
        (setf (relay-read-iters (read-type-relay n)) (map 'list #'fixup-dims (node-reads n) (relay-reads (read-type-relay n)))
              (relay-write-iters (read-type-relay n)) (map 'list #'fixup-dims (node-writes n) (relay-writes (read-type-relay n))))))))

(defmethod node-depend-idx-list ((node Node) gid
                                 &aux
                                   (type (read-type-relay node))
                                   (shapes (make-list (length gid)))
                                   (strides (make-list (length gid))))
  "Enumerates a list of gid that the node depends on.
- If loop size is 1 => the loop is removed with _gidn=0.
- If stride is 0    => the index space won't use corresponding _gid."
  (flet ((no-dep-p (size stride) (or (expr-equal-to size 1) (expr-equal-to stride 0))))
    (dolist (space (append (relay-read-iters type) (relay-write-iters type)))
      (when space
        (loop for axis upfrom 0
              for shape in (iteration-space-shape space)
              for stride in (iteration-space-strides space)
              do (push shape (nth axis shapes)) (push stride (nth axis strides)))))
    (loop for g in gid
          for size in shapes
          for stride in strides
          if (not (every #'no-dep-p size stride))
            collect g)))

(defun node-reduced-axes (node)
  (let ((is (car (relay-write-iters (read-type-relay node)))))
    (when is
      (loop for s in (iteration-space-strides is)
            if (expr-equal-to s 0)
              collect t
            else
              collect nil))))

(defun node-reduced-gids (node gids &aux (axes (node-reduced-axes node)))
  (when (null axes) (setf axes (make-list (length gids))))
  (assert (= (length gids) (length axes)) () "the reduction node ~a is not the highest rank tensor." node)
  (when (getattr node :reduction :allow-undefined t)
    (loop for nth upfrom 0
          for r in axes
          if r collect (nth nth gids))))

(defun graph-reduced-axes (graph rank-size)
  (let ((reduced-axes (make-list rank-size)))
    (dolist (node (graph-nodes graph))
      ;; Broadcasting information are always stored by the highest rank tensor.
      (when (and
             (getattr node :reduction :allow-undefined t)
             (car (relay-write-iters (read-type-relay node))))
        (when (= rank-size (length (iteration-space-shape (car (relay-write-iters (read-type-relay node))))))
          (loop for nth upfrom 0
                for r in (node-reduced-axes node)
                if r do (setf (nth nth reduced-axes) t)))))
    reduced-axes))

(defun initial-loop-permutation (graph rank)
  (let ((reduced (graph-reduced-axes graph rank))
        (stashed))
    ;; reduced axes should be the last
    `(,@(loop for p in (range 0 rank)
              for r in reduced
              if r ;; (reduced)
                do (push p stashed)
              else
                collect p)
      ,@(nreverse stashed))))

(defmethod graph-swizzle-space ((graph Graph) (order list))
  "Permutes all buffers in the graph by the order."
  (flet ((swizzle (id space)
           (when space
             (assert (length (iteration-space-procedure space)) () "graph-swizzle-loop-order: Cannot swizzle the space ~a ~a with ~a" id space order)
             (setf (iteration-space-shape space) (permute-list order (iteration-space-shape space))
                   (iteration-space-strides space) (permute-list order (iteration-space-strides space))
                   (iteration-space-views space) (permute-list order (iteration-space-views space))
                   (iteration-space-procedure space) (permute-list order (iteration-space-procedure space))))))
    (dolist (n (graph-nodes graph))
      (mapc #'swizzle (node-reads n) (relay-read-iters (read-type-relay n)))
      (mapc #'swizzle (node-writes n) (relay-write-iters (read-type-relay n))))))

(defstruct ctx
  "an intermidate object used to debug `recursive-lower-into-bp`"
  graph order blueprint seen gids loop-size-list band2node schedule-graph)
;; ctx: print-object displays nothing not to collapse the trace macro.
(defmethod print-object ((ctx ctx) stream) (format stream "<CTX>"))

(defmethod initial-bp ((ctx ctx))
  (with-slots ((gids gids) (group-size loop-size-list)) ctx
    `(,@(map 'list #'%make-coincident-range gids group-size) ,@(reverse (map 'list #'%make-endrange gids)))))

(defmethod reduce-bp ((ctx ctx) (something node) gids key)
  (let* ((sizes (loop with size = (iteration-space-shape (car (relay-write-iters (read-type-relay something))))
                      for g in gids
                      for p = (position g (ctx-gids ctx))
                      collect (nth p size)))
         (loops (map 'list #'%make-reduction-range gids sizes)))
    (dolist (l loops) (setf (gethash (node-id l) (ctx-band2node ctx)) key))
    `(,@loops ,something ,@(reverse (map 'list #'%make-endrange gids)))))

(defun recursive-scalar-p (ctx id)
  (declare (type ctx ctx))
  (or
   (null (id->value (ctx-graph ctx) id))
   (let* ((node (id->value (ctx-graph ctx) id))
          (node-depend-axes
            (node-depend-idx-list node (ctx-gids ctx)))
          (node-reduce-axes
            (node-reduced-gids node (ctx-gids ctx)))
          (node-depend-axes
            (loop for x in node-depend-axes
                  if (null (find x node-reduce-axes))
                    collect x))
          (node-depend-users
            ;; `node` must be located before users are located.
            (id->users (ctx-graph ctx) (car (node-writes node))))
          (user-depend-axes
            (loop for user in node-depend-users
                  if (and
                      (eql (car (node-writes node)) (car (node-reads user)))
                      (getattr user :reduction :allow-undefined t))
                    append (node-reduced-gids user (ctx-gids ctx)))))
     (and (null node-depend-axes) (null node-reduce-axes) (null user-depend-axes)
          (every #'(lambda (x) (recursive-scalar-p ctx x)) (node-reads node))))))

(defun gid-n (gid &aux (gid-str (princ-to-string gid)))
  (assert (equalp (subseq gid-str 0 4) "_gid"))
  (parse-integer (subseq gid-str 4)))

(defun ctx-padding-loop (ctx)
  "Inserts padding loops to keep the rank of loops same. Polyhedral Compiler should responsible for transforming this, so, caten/codegen should keep the ir untouched."
  (let* ((appeared
           (loop for bp in (ctx-blueprint ctx)
                 if (eql (node-type bp) :TMPRange)
                   collect bp))
         (threshold (and appeared (apply #'min (map 'list #'(lambda (x) (gid-n (getattr x :idx))) appeared))))
         (padding-loops
           (loop for gid in (ctx-gids ctx)
                 for size in (ctx-loop-size-list ctx)
                 if (and threshold (expr-equal-to size 1)
                         (null (find gid appeared :key #'(lambda (x) (getattr x :idx))))
                         (< (gid-n gid) threshold)) ;; only the outermost loops are subject.
                   collect (cons (%make-coincident-range gid size) (%make-endrange gid)))))
    (when (null appeared) (return-from ctx-padding-loop (ctx-blueprint ctx)))
    `(,@(map 'list #'car padding-loops)
      ,@(ctx-blueprint ctx)
      ,@(reverse (map 'list #'cdr padding-loops)))))

(defun try-insert-node (ctx node &aux (changed-p nil))
  "First this function tries to search all possible insertable position for the node, and then insert the node in the most suitable position. (values new_bp successed_p) is returned."
  (with-slots ((blueprint blueprint)) ctx
    (let* ((node-depend-axes ;; list of gids `node` should depend on
             (node-depend-idx-list node (ctx-gids ctx)))
           (node-reduce-axes ;; list of gids `node` should NOT depend on
             (node-reduced-gids node (ctx-gids ctx)))
           (node-depend-axes
             (loop for x in node-depend-axes
                   if (null (find x node-reduce-axes))
                     collect x))
           (node-depend-users
             ;; `node` must be located before users are located.
             (id->users (ctx-graph ctx) (car (node-writes node))))
           (user-depend-axes
             (loop for user in node-depend-users
                   if (and
                       (eql (car (node-writes node)) (car (node-reads user)))
                       (getattr user :reduction :allow-undefined t))
                     append (node-reduced-gids user (ctx-gids ctx))))
           (satisfied)
           (idx-satisfied)
           (insertable-positions)
           (high-priority-positions)
           (serialized-reduce-idx))
      (dolist (depend node-depend-users)
        ;; The try-insert-node algorithm below assumes all of `node-depend-users` are located to ctx-blueprint.
        ;; <=> If the node depends on an unplaced node, determine that node first.
        (when (null (find (node-id depend) (ctx-blueprint ctx) :key #'node-id))
          (recursive-lower-into-bp ctx (car (node-writes depend)))))
      ;; If the condition is satisfied when T=0 -> insert -1
      (when (and
             (null node-depend-axes) (null node-reduce-axes) (null user-depend-axes))
        (push -1 insertable-positions))
      (when (intersection node-depend-axes user-depend-axes)
        (warn "(intersection node-depend-axes user-depend-axes) == nil failed, the lowering may fail."))
      (loop for bp in blueprint
            for nth upfrom 0
            for high-priority-p = nil
            if (eql (node-type bp) :TMPRange) do
              (let ((cache (gethash (node-id bp) (ctx-band2node ctx))))
                (when (and node-reduce-axes cache)
                  ;; Relucant to create a new reduce loop
                  (let ((p (or
                            (graph-weakly-connected-p (ctx-graph ctx) (car (node-writes node)) cache)
                            (graph-weakly-connected-p (ctx-graph ctx) cache (car (node-writes node))))))
                    (when (null p)
                      (push (getattr bp :idx) serialized-reduce-idx))))
                (push (getattr bp :idx) idx-satisfied))
            else
              if (eql (node-type bp) :TMPEndRange)
                do (setf idx-satisfied (remove (getattr bp :idx) idx-satisfied)
                         serialized-reduce-idx (remove (getattr bp :idx) serialized-reduce-idx))
            else
              do (push (node-id bp) satisfied)
            if (and
                (every #'(lambda (x) (null (find (node-id x) satisfied))) node-depend-users)
                (every #'(lambda (x) (find x idx-satisfied)) node-depend-axes)
                (or
                 (every #'(lambda (x) (null (find x idx-satisfied))) node-reduce-axes)
                 ;; If node-reduce-axes loops are already introduced by other independant nodes
                 ;; => Set high-priority-positions to the current position, and forcibly insert the node there.
                 (setf high-priority-p (every #'(lambda (x) (find x serialized-reduce-idx)) node-reduce-axes)))
                (every #'(lambda (x) (null (find x idx-satisfied))) user-depend-axes))
              do (if high-priority-p
                     (push nth high-priority-positions)
                     (push nth insertable-positions)))
      (when (null insertable-positions)
        (return-from try-insert-node (values blueprint nil)))
      (when (and
             (find -1 insertable-positions)
             (every #'(lambda (x) (recursive-scalar-p ctx x)) (node-reads node)))
        (let ((nrank (tensor-relay-nrank (car (relay-writes (read-type-relay node))))))
          (when (and (> nrank 0) (getattr node :reduction :allow-undefined t))
            ;; The node is located out of the loop, scalarize it.
            ;; This will mutate constant as a pointer, or pointer as a constant.
            ;; Running `simplify-pointer-and-constant` and fix them.
            (setf (iteration-space-strides (car (relay-read-iters (read-type-relay node))))
                  (loop repeat nrank collect (expr-const 0 :int64))))
          (when (= nrank 0)
            ;; If only used in this blueprint
            (let ((users (id->users (ctx-schedule-graph ctx) (car (node-writes node)))))
              (if (some #'(lambda (x) (eql :kernel (getattr x :type))) users) ;; If the scalar is used in another jitable kernel?
                  (let* ((dtype (tensor-relay-dtype (car (relay-writes (read-type-relay node)))))
                         (buffer (make-tensor-relay `(1) `(0) dtype `((0 1 1 t))))
                         (space (tensor-relay-merge-dims (ctx-schedule-graph ctx) buffer)))
                    (setf (car (relay-write-iters (read-type-relay node))) space
                          (car (relay-writes (read-type-relay node))) buffer))
                  ;; Otherwise, its constant
                  (setf (getattr node :declare-type) (list t))))))
        (return-from try-insert-node
          (if node-reduce-axes
              (values `(,@(reduce-bp ctx node node-reduce-axes (car (node-writes node))) ,@blueprint) t)
              (values `(,node ,@blueprint) t))))
      (values
       (loop with insert-at = (if high-priority-positions
                                  (apply #'max high-priority-positions)
                                  (apply #'max insertable-positions))
             for bp in blueprint
             for nth upfrom 0
             collect bp
             if (and (null changed-p) (= nth insert-at)
                     ;; Merging loops w/o introducing extra inner reduce loops
                     (or high-priority-positions (null node-reduce-axes)))
               do (setf changed-p t) and collect node
             if (and (null changed-p) (= nth insert-at) node-reduce-axes)
               ;; Merging loops w/ introducing extra inner reduce loops
               do (setf changed-p t) and append (reduce-bp ctx node node-reduce-axes (car (node-writes node))))
       changed-p))))

(defun recursive-lower-into-bp (ctx id &aux (node (id->value (ctx-graph ctx) id)))
  (with-slots ((blueprint blueprint) (seen seen) (gids gids) (order order)) ctx
    (when (null node) (return-from recursive-lower-into-bp))
    (when (find id seen) (return-from recursive-lower-into-bp))
    (push id seen)
    (multiple-value-bind (new-bp changed-p) (try-insert-node ctx node)
      (when (null changed-p) ;; if failed -> add the outermost loops and retry. (todo: which is not preferable though...)
        (setf (ctx-blueprint ctx) (append (initial-bp ctx) (ctx-blueprint ctx)))
        (multiple-value-bind (new-bp1 changed-p1)
            (try-insert-node ctx node)
          (setf new-bp new-bp1 changed-p changed-p1)))
      (assert changed-p () "recursive-lower-into-bp: Cannot lower the node ~a within a single kernel.
Depends=~a Reduce=~a Users=~a
```
~a
```"
              node (node-depend-idx-list node (ctx-gids ctx)) (node-reduced-gids node (ctx-gids ctx))
              (map 'list #'node-id (id->users (ctx-graph ctx) (car (node-writes node)))) (ctx-blueprint ctx))
      (setf blueprint new-bp)
      (mapc
       #'(lambda (x)
           (when (and (null (find x seen)) (id->value (ctx-graph ctx) x))
             (recursive-lower-into-bp ctx x)))
       (node-reads node))
      nil)))

(defun make-index-components (node gids)
  (assert (eql (node-type node) :INDEX-COMPONENTS))
  (flet ((maybe-expr-const (x) (if (numberp x) (expr-const x :int64) x)))
    (labels ((from-expr (shapes components)
               (reduce
                #'expr-add
                (map
                 'list
                 #'(lambda (size stride gid)
                     (if (expr-equal-to size 1)
                         (expr-const 0 :int64)
                         (expr-mul (maybe-expr-const stride) (maybe-expr-const gid))))
                 shapes
                 components
                 gids)))
             (merge-stride (proc list)
               (loop for p in proc
                     collect
                     (let ((strides (map 'list #'(lambda (x) (nth x list)) p)))
                       (if (find 0 strides :test #'eql) (expr-const 0 :int64) (maybe-expr-const (car (last strides))))))))
      (let* ((is (car (relay-write-iters (read-type-relay node))))
             (proc (iteration-space-procedure is))
             (components (merge-stride proc (cdr (node-reads node)))))
        (let ((e (from-expr (iteration-space-shape is) components)))
          (setf (node-writes (expr-out e)) (node-writes node)
                (graph-outputs (expr-graph e)) (node-writes node))
        e)))))

(defun astify-blueprint (schedule-item bp rank base-graph schedule-graph &aux (caten/aasm/expr::*expr-no-simplify-mode* t) (gid-seen (make-hash-table)))
  (declare (type list bp))
  (with-blueprint (:noopt t)
    (loop for n in (node-reads schedule-item)
          for nt in (getattr schedule-item :read-types)
          do (%global n (tensor-relay-dtype nt) (> (tensor-relay-nrank nt) 0)))
    (loop for w in (node-writes schedule-item)
          for wt in (getattr schedule-item :write-types)
          do (%global w (tensor-relay-dtype wt) (> (tensor-relay-nrank wt) 0)))
    (labels ((sendexpr (expr)
               (dolist (n (graph-nodes (expr-graph expr))) (emit n))
               (expr-out expr))
             (is-setf-p (id &aux (val (id->value *ctx* id)))
               (when (and val (eql (node-type val) :SETF))
                 (car (node-reads val))))
             (padding-gids (gids)
               (append
                gids
                (loop repeat (- rank (length gids)) collect 0)))
             (rel->alloc (id rel)
               (let* ((*ctx* (make-graph))
                      (alloc (%alloc (tensor-relay-nrank rel) (tensor-relay-shape rel) (tensor-relay-stride rel) :dtype (tensor-relay-dtype rel))))
                 (setf (node-writes alloc) (list id))
                 ;; Gather related region from the allocation
                 (let* ((alloc-graph (apply #'make-graph (loop for n in (graph-nodes base-graph)
                                                               if (find id (node-writes n)) collect alloc
                                                                 else collect n))))
                   (setf (graph-outputs alloc-graph) (list id))
                   ;; note(hikettei): I am not sure if this is correct
                   (let ((runtime-graph (->fast-graph alloc-graph)))
                     (verify-graph runtime-graph)
                     (graph-infer-type-relay runtime-graph)
                     (loop with region = (apply #'append (map 'list #'node-writes (graph-nodes schedule-graph)))
                           for item in (graph-nodes runtime-graph)
                           if (null (intersection (node-writes item) region))
                             do (insert-nodes schedule-graph (list (caten/codegen/scheduler::group->schedule-item (caten/codegen/scheduler:make-group :predecessor (node-reads item) :successor (node-writes item) :items (list item))))))))))
             (ensure-unique-gid (gid)
               (if (null (gethash gid gid-seen))
                   (prog1 gid (setf (gethash gid gid-seen) 1))
                   (prog1 (intern (format nil "~a_~a" gid (gethash gid gid-seen))) (incf (gethash gid gid-seen)))))
             (lower-item (node gids)
               (let ((node (copy-node node))
                     (gids (padding-gids gids)))
                 (assert (= (length (node-writes node)) 1) () "Cannot lower the node ~a with multiple writes." node)
                 ;; Lowering %aref if needed.
                 (loop for buffer in (relay-reads (read-type-relay node))
                       for ri in (relay-read-iters (read-type-relay node))
                       for name in (node-reads node)
                       for nth upfrom 0
                       if (is-setf-p name) do
                         (let* ((load (emit (make-node :JIT :BIND (list (gensym "BIND")) (list name) :value (is-setf-p name)))))
                           (setf (nth nth (node-reads node)) (car (node-writes load))))
                       else if (and buffer (> (tensor-relay-nrank buffer) 0)) do
                         (let ((aref (emit (%aref name (sendexpr (reduce #'expr-add (iteration-space-expr-aref ri buffer gids)))))))
                           (setf (nth nth (node-reads node)) (car (node-writes aref)))))
                 ;; Insert %setf if the node is reduction.
                 (when (getattr node :reduction :allow-undefined t)
                   (assert (not (find (car (node-writes node)) (node-writes schedule-item))) () "The reduction node ~a cannot be an output of the schedule-item." node)
                   (let* ((type (read-type-relay node))
                          (aref (if (> (tensor-relay-nrank (car (relay-reads type))) 0)
                                    (emit
                                     (%aref
                                      (car (node-reads node))
                                      (sendexpr (reduce #'expr-add (iteration-space-expr-aref (car (relay-read-iters type)) (car (relay-reads type)) gids)))))
                                    (car (node-reads node))))
                          (id (car (node-writes node))))
                     (setf (getattr node :_type_relay) nil)
                     (setf (node-writes node) (list (gensym "TMP")))
                     (return-from lower-item (emit (%setf aref (emit node) :out id)))))
                 ;; Also insert %setf if the node is an output of the schedule-item.
                 (when (or
                        (find (car (node-writes node)) (node-writes schedule-item))
                        (> (tensor-relay-nrank (car (relay-writes (read-type-relay node)))) 0))
                   (when (null (find (car (node-writes node)) (node-writes schedule-item)))
                     ;; Insert a %global as a temporary buffer
                     ;; Here, (car (node-writes node)) should not exist in the schedule graph, so we have to insert allocation.
                     (let ((id (car (node-writes node)))
                           (rel (car (relay-writes (read-type-relay node)))))
                       (assert (and id rel))
                       (when (null (find id (node-reads schedule-item)))
                         (push id (node-reads schedule-item))
                         (push rel (getattr schedule-item :read-types))
                         (rel->alloc id rel)))
                     (%global (car (node-writes node)) (tensor-relay-dtype (car (relay-writes (read-type-relay node)))) t))
                   (assert (null (getattr node :reduction :allow-undefined t)) () "The node ~a cannot be a reduction node." node)
                   (let* ((type (read-type-relay node))
                          (aref (if (> (tensor-relay-nrank (car (relay-writes type))) 0)
                                    (emit
                                     (%aref
                                      (car (node-writes node))
                                      (sendexpr (reduce #'expr-add (iteration-space-expr-aref (car (relay-write-iters type)) (car (relay-writes type)) gids)))))
                                    ;; [TODO] (%aref x 0) ?
                                    (car (node-writes node)))))
                     (setf (getattr node :_type_relay) nil)
                     (setf (node-writes node) (list (gensym "T")))
                     ;; Note: %setf aref is the end of node.
                     (return-from lower-item (emit (%setf aref (emit node))))))
                 ;; Otherwise emit the node directly.
                 ;; [TODO] Set declare-type?
                 (setf (getattr node :_type_relay) nil)
                 (emit node)))
             (explore (rest-items gids)
               (declare (type list rest-items))
               (when (null rest-items) (return-from explore))
               (case (node-type (car rest-items))
                 (:TMPRange
                  (let ((endrange (position-if #'(lambda (x) (and (eql (node-type x) :TmpEndRange) (equal (getattr x :idx) (getattr (car rest-items) :idx)))) rest-items))
                        (gid (ensure-unique-gid (getattr (car rest-items) :idx))))
                    (assert endrange () "Inserting TMPRange w/o corresponding TmpEndRange is invaild. Malformed lowering result?")
                    (%progn
                     (%range
                      gid (sendexpr (getattr (car rest-items) :size))
                      (%progn (explore (subseq rest-items 1 endrange) (append gids (list gid))))
                      :mark (getattr (car rest-items) :type))
                     (explore (subseq rest-items (1+ endrange)) gids))))
                 (:TMPEndRange (error "TMPEndRange should not occur here, malformed lowering result?"))
                 (:INDEX-COMPONENTS
                  (%progn (sendexpr (make-index-components (car rest-items) gids)) (explore (cdr rest-items) gids)))
                 (otherwise
                  (%progn (lower-item (car rest-items) gids) (explore (cdr rest-items) gids))))))
      (explore bp nil))))
;; Entry point for the lowerer is here :)  
(defun lower-schedule-item (schedule-item base-graph schedule-graph)
  "
```
(lower-schedule-item schedule-item)
```
Takes one node of type `Schedule-Item` and returns the blueprint.
"
  (declare (type node schedule-item))
  (assert (eql (node-type schedule-item) :Schedule-Item) () "lower-schedule-item expects a Schedule-Item, got ~a" schedule-item)
  ;; Only kernel are lowered
  (unless (eql :kernel (getattr schedule-item :type)) (return-from lower-schedule-item))
  (let* ((graph (apply #'make-graph (getattr schedule-item :items)))
         (_ (setf (graph-outputs graph) (node-writes schedule-item)))
         (iterspace (get-grouped-dims graph base-graph))
         (__ (fixup-graph-iteration-space graph iterspace base-graph))
         (order (initial-loop-permutation graph (length (car iterspace))))
         (gids (permute-list order (map 'list #'gid (range 0 (length (car iterspace))))))
         (group-size (permute-list order (car iterspace))))
    (declare (ignore _ __))
    (graph-swizzle-space graph order)
    ;; gids       = (gid0, gid1, gid2, ...)
    ;; group-size = (10, 20, 30, ...)
    ;; order      = (0 1 2 ...) (the order of gids, and group-size)
    (let ((ctx (make-ctx :graph graph :order order :gids gids :loop-size-list group-size :blueprint nil :band2node (make-hash-table) :schedule-graph schedule-graph)))
      (setf (ctx-blueprint ctx) (initial-bp ctx))
      #+nil(trace caten/codegen/blueprint::recursive-lower-into-bp)
      #+nil(untrace caten/codegen/blueprint::recursive-lower-into-bp)
      (mapc #'(lambda (x) (recursive-lower-into-bp ctx x)) (graph-outputs graph))
      (setf (ctx-blueprint ctx) (ctx-padding-loop ctx)
            (ctx-blueprint ctx) (bp-finalize-realize (ctx-blueprint ctx) schedule-item base-graph)
            (getattr schedule-item :blueprint) (caten/aasm::%simplify-ast (astify-blueprint schedule-item (ctx-blueprint ctx) (length (ctx-gids ctx)) base-graph schedule-graph))))))
;; [TODO] Move to caten/aasm
(defmethod print-blueprint (graph stream &aux (indent 0) (seen))
  ;; (caten/air:->dot graph :pathname "/tmp/graph.dot")
  (princ
   (with-output-to-string (out)
     (labels ((indent () (make-string indent :initial-element #\space))
              (fmt (desig &rest args) (apply #'format out (format nil "~a~a~%" (indent) desig) args))
              (r (s &aux (val (id->value graph s)))
                (when (and val (null (find (node-id val) seen)))
                  (f val) (push (node-id val) seen))
                s)
              (e (id)
                (let ((renderer (make-instance 'Default-Renderer :graph graph)))
                  (render-node renderer id)))
              (f (node)
                (case (node-type node)
                  (:PROGN
                    (fmt "{")
                    (incf indent 2) (mapc #'r (node-reads node)) (decf indent 2)
                    (fmt "}"))
                  (:EXPR
                   (if (eql :SETF (node-type (id->value graph (car (node-reads node)))))
                       (fmt "~a; // EXPR(STORE)" (e (car (node-reads node))))
                       (fmt "~(~a~) = ~a; // expr" (car (node-writes node)) (e (car (node-reads node))))))
                  (:DEFINE-GLOBAL); (fmt "defglobal ~a;" (car (node-writes node))))
                  (:RANGE)
                  (:FOR
                   (multiple-value-bind (range body) (apply #'values (node-reads node))
                     (setf range (id->value graph range))
                     (assert (and range (eql (node-type range) :RANGE)) () "The first argument of :FOR should be :RANGE, getting ~a" range)
                     (multiple-value-bind (bind size step) (values (getattr range :idx) (first (node-reads range)) (second (node-reads range)))
                       (when (symbolp size)
                         (let ((val (id->value graph size)))
                           (assert (and val (eql (node-type val) :EXPR)) () "Range: The size must be specified as EXPR or fixnum, getting ~a" val)
                           (setf size (car (node-reads val)))))
                       (when (symbolp step)
                         (let ((val (id->value graph step)))
                           (assert (and val (eql (node-type val) :EXPR)) () "Range: The step must be specified as EXPR or fixnum, getting ~a" val)
                           (setf step (car (node-reads val)))))
                       (fmt "@~(~a~) for (int ~(~a~)=0; ~(~a~)<~(~a~); ~(~a~)+=~a) ~a~a" (getattr node :mark)
                            bind bind (e size) bind (e step)
                            (if (getattr node :is-empty) "/* empty */" "")
                            (if (getattr node :band) (format nil " [~a]" (getattr node :band)) "")))
                     (unless (eql (node-type (id->value graph body)) :PROGN) (incf indent 2))
                     (r body)
                     (unless (eql (node-type (id->value graph body)) :PROGN) (decf indent 2))))
                  (:ALLOCATE (fmt "~(~a~) ~(~a~);" (getattr node :dtype) (car (node-writes node))))
                  (:LOAD (r (car (node-reads node))) (fmt "~(~a~) = ~(~a~);" (car (node-writes node)) (getattr node :value)))
                  (:Aref
                   (multiple-value-bind (name idx) (apply #'values (node-reads node))
                     (r name) (r idx)
                     (fmt "~(~a~) = ~(~a~)[~(~a~)];" (car (node-writes node)) name idx)))
                  (:IF
                   (multiple-value-bind (cond body) (apply #'values (node-reads node))
                     (setf cond (id->value graph cond))
                     (assert (and cond (eql (node-type cond) :EXPR)) () "IF: the conditon must be EXPR.")
                     (fmt "if (~(~a~)) {" (e (car (node-reads cond))))
                     (incf indent 2) (r body) (decf indent)
                     (fmt "}")))
                  (otherwise (mapc #'r (node-reads node)) (fmt "~(~a~) = ~(~a~)(~(~a~));" (car (node-writes node)) (node-type node) (render-list (node-reads node)))))))
       (f (id->value graph (car (graph-outputs graph))))))
   stream))
;; ^ Simplify how to describe write relay
;;; ~~~~ GFlops Measurements (Not Tested) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct GFlops-Measurer
  "A helper object to compute GFlops"
  (ops (error "flops must occur") :type (or null Expr))
  (succeed-p t :type boolean))
(defun cannot-compute-flop () (make-gflops-measurer :ops nil :succeed-p nil))
(defmethod compute-gflops ((gfm GFlops-Measurer) elapsed params)
  (when (null (gflops-measurer-succeed-p gfm)) (return-from compute-gflops nil))
  (assert (gflops-measurer-ops gfm))
  (when (zerop elapsed) (return-from compute-gflops nil)) ;; Elapsed Time = 0.0
  (let* ((ops (apply #'expr-realize (gflops-measurer-ops gfm) params))
         (_ (assert (numberp (tensor-relay-value ops)) () "measure-gflpos: the result is not a number."))
         (gflops (/ (tensor-relay-value ops) (* elapsed 1e9))))
    (declare (ignore _))
    gflops))
(defmethod schedule-item-flops (node &aux (total-flops))
  (declare (type node node))
  (assert (eql (node-type node) :Schedule-Item) () "schedule-item-flops: the node is not a Schedule-Item.")
  (assert (getattr node :blueprint-base) () ":blueprint-base must be provided!")
  (loop with bounds = nil
        for node in (getattr node :blueprint-base)
        if (eql (node-type node) :FOR)
          do (let ((size (expr-detach-loop-bound (getattr node :below) :allow-failed t)))
               ;; The loop must be affine
               (when (not (expr-equal-to (getattr node :upfrom) 0))
                 (return-from schedule-item-flops (cannot-compute-flop)))
               (when (not (expr-equal-to (getattr node :by) 1))
                 (return-from schedule-item-flops (cannot-compute-flop)))
               (when (null size) (return-from schedule-item-flops (cannot-compute-flop)))
               (push (cons node size) bounds))
        else if (eql (node-type node) :ENDFOR)
               do (setf bounds (remove (getattr node :idx) bounds :key #'(lambda (x) (getattr (car x) :idx)) :test #'equal))
        else if (eql (node-type node) :EXPR) do
          (let ((flop (expr-flops node))
                (volume (reduce #'expr-mul (map 'list #'cdr bounds))))
            (push (expr-mul (expr-const flop :int64) volume) total-flops))
        else
          do (return-from schedule-item-flops (cannot-compute-flop)))
  (let ((ops (reduce #'expr-add total-flops)))
    (setf (expr-graph ops) (->graph-with-tpsort (->fast-graph (expr-graph ops))))
    (make-gflops-measurer :ops ops :succeed-p t)))
