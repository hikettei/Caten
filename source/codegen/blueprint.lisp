(defpackage :caten/codegen/blueprint
  (:documentation "The package `caten/codegen/blueprint` is responsible for lowering the schedule-item into a blueprint. A blueprint is an IR that represents a computation graph with explicit loop bounds.
The `lower-schedule-item` method infers loop boundaries based on `Schedule-item` and performs lowering into a format that includes :FOR/:ENDFOR nodes.")
  (:use :cl :caten/air :caten/codegen/expr :alexandria :caten/codegen/expr-cache :caten/codegen/shape-inference :caten/codegen/helpers :caten/runtime/runtime :caten/runtime/buffer)
  (:import-from
   :caten/codegen/renderer
   #:render-expr
   #:Default-Renderer)
  (:import-from
   :caten/codegen/exprify
   #:blueprint-exprify
   #:blueprint-scalarify
   #:blueprint-set-iterations
   #:blueprint-propagate-reduction
   #:blueprint-realized-buffers)
  (:import-from :caten/codegen/rewriting-rules #:nodes-apply-static-gensym)
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
   #:schedule-item-gflops))

(in-package :caten/codegen/blueprint)
;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun %make-for (idx size)
  "Represents for an iteration in the range of [0, size)"
  (make-node :Render :FOR (list idx)  nil :idx idx :upfrom (expr-const 0 :int64) :below (expr-< (expr-const idx :int64) size) :by (expr-const 1 :int64)))

(defun %make-endfor (idx)
  "Represents the end of the iteration."
  (make-node :Render :ENDFOR nil nil :idx idx))

(defmethod print-blueprint (nodes stream &aux (gids))
  (labels ((print-aref (name b is &key (iterations (make-index-space)))
             (if (and is (not (= -1 (buffer-nrank b))) (> (length (iteration-space-shape is)) 0) (> (length iterations) 0))
                 (format nil "~a[~(~a~)]" name
                         (render-expr
                          'Default-Renderer
                          (apply
                           #'expr-add
                           (map
                            'list
                            #'(lambda (view stride i)
                                (if view
                                    (expr-mul stride (expr-add (expr-const (car view) :int64) (expr-mul (expr-const (third view) :int64) i)))
                                    (expr-mul stride i)))
                            (iteration-space-views is)
                            (iteration-space-strides is)
                            iterations))))
                 (format nil "~(~a~)" name)))
           (make-index-space ()
             (map 'list #'(lambda (x) (expr-const (intern (princ-to-string x)) :int64)) (reverse gids))))
    (format
     stream
     "{~%~a}~%"
     (with-output-to-string (out)
       (flet ((indent (n) (with-output-to-string (o) (dotimes (i n) (princ " " o)))))
         (loop with indent = 2
	       for node in nodes
	       if (eql (node-type node) :FOR)
	         do (format out "~a~afor (int ~(~a~)=~(~a~);~(~a~);~(~a~)+=~(~a~)) {~%"
                            (indent indent) (if (eql (getattr node :scope) :global) "parallel " "") (getattr node :idx)
                            (render-expr 'Default-Renderer (getattr node :upfrom)) (render-expr 'Default-Renderer (getattr node :below))
                            (getattr node :idx) (render-expr 'Default-Renderer (getattr node :by)))
                    (push (getattr node :idx) gids)
		    (incf indent 2)
	       else if (eql (node-type node) :ENDFOR)
	              do (decf indent 2) (format out "~a} // ~(~a~)~%" (indent indent) (getattr node :idx)) (setf gids (remove (getattr node :idx) gids))
               else if (eql (node-type node) :IF)
                      do (incf indent 2) (format out "~a if (~(~a~)) {~%" (indent indent) (render-expr 'Default-Renderer (getattr node :condition)))
               else if (eql (node-type node) :ENDIF)
                      do (decf indent 2) (format out "~a } // endif~%" (indent indent))
               else if (eql (node-type node) :EXPR) do
                 (let ((pre-iterations (getattr node :Iterations)))
                   (format out "~a~a = ~a;~a~a~%"
                           (indent indent)
                           (render-list
                            (map 'list #'(lambda (x y z) (print-aref x y z :iterations (or pre-iterations (make-index-space))))
                                 (node-writes node) (relay-writes (read-type-relay node)) (relay-write-iters (read-type-relay node))))
                           (render-expr 'Default-Renderer (getattr node :EXPR) :index-space (or pre-iterations (make-index-space)))
                           (if (getattr node :reduction :allow-undefined t)
                               " // :reduction=t"
                               "")
                           (if (typep (getattr node :meta :allow-undefined t) 'ExprMeta)
                               (format nil " // ~a" (exprmeta-comment (getattr node :meta)))
                               "")))
               else
	         do (format out "~a~a = ~(~a~)(~a);~a~%"
                            (indent indent)
                            (render-list (map 'list #'print-aref (node-writes node) (relay-writes (read-type-relay node)) (relay-write-iters (read-type-relay node))))
                            (node-type node)
                            (render-list (map 'list #'print-aref (node-reads node) (relay-reads (read-type-relay node)) (relay-read-iters (read-type-relay node))))
                            (if (getattr node :reduction :allow-undefined t)
                                " // :reduction=t"
                                ""))))))))

(defun node-reduced-axes (node)
  (let ((is (car (relay-write-iters (read-type-relay node)))))
    (when is
      (loop for s in (iteration-space-strides is)
            if (expr-equal-to s 0)
              collect t
            else
              collect nil))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod get-grouped-dims ((graph Graph) (base-graph Graph))
  "Infers the loop boundaries of the graph by finding the common iteration space."
  (let* ((kernel-rank
           (loop for node in (graph-nodes graph)
                 for type = (read-type-relay node)
                 maximize
                 (loop for r in (append (relay-reads type) (relay-writes type))
                       when r maximize (length (buffer-shape r)))))
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
                                  (buffer-iteration-space base-graph buffer)
                                  (buffer-merge-dims base-graph buffer))))
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
               (when (and original-buffer (> (length (buffer-shape original-buffer)) 0))
                 ;; Caten cannot inference where to insert one here.
                 (assert (= (length (buffer-shape original-buffer)) (1+ kernel-rank))
                         ()
                         "(id=~a) Cannot uprank ~a into the space ~a. A original buffer should be upranked by the scheduler in advance.~%~a" id original-buffer found-space graph)
                 (multiple-value-bind (new-shape new-stride new-view)
                     (values (merge-list procedure (buffer-shape original-buffer))
                             (merge-stride procedure (new-stride (buffer-stride original-buffer) (buffer-views original-buffer)))
                             (merge-view procedure (buffer-views original-buffer)))
                   (make-iteration-space
                    :shape new-shape
                    :strides new-stride
                    :views new-view
                    :procedure procedure)))))
      (dolist (n (graph-nodes graph))
        (setf (relay-read-iters (read-type-relay n)) (map 'list #'fixup-dims (node-reads n) (relay-reads (read-type-relay n)))
              (relay-write-iters (read-type-relay n)) (map 'list #'fixup-dims (node-writes n) (relay-writes (read-type-relay n))))))))

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
;; ~~~ Lowerer ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct ctx
  "an intermidate object used to debug `recursive-lower-into-bp`"
  graph order blueprint seen gids loop-size-list band2node schedule-graph)
;; ctx: print-object displays nothing not to collapse the trace macro.
(defmethod print-object ((ctx ctx) stream) (format stream "<CTX>"))

(defmethod initial-bp ((ctx ctx))
  (with-slots ((gids gids) (group-size loop-size-list)) ctx
    `(,@(map 'list #'%make-for gids group-size) ,@(reverse (map 'list #'%make-endfor gids)))))

(defmethod reduce-bp ((ctx ctx) (something node) gids key)
  (let* ((sizes (loop with size = (iteration-space-shape (car (relay-write-iters (read-type-relay something))))
                      for g in gids
                      for p = (position g (ctx-gids ctx))
                      collect (nth p size)))
         (loops (map 'list #'%make-for gids sizes)))
    (dolist (l loops)
      (setf (gethash (node-id l) (ctx-band2node ctx)) key))
    `(,@loops
      ,something
      ,@(reverse (map 'list #'%make-endfor gids)))))

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
            if (eql (node-type bp) :FOR) do
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
              if (eql (node-type bp) :ENDFOR)
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
        (let ((nrank (buffer-nrank (car (relay-writes (read-type-relay node))))))
          (when (and (> nrank 0) (getattr node :reduction :allow-undefined t))
            ;; The node is located out of the loop, scalarize it.
            ;; This will mutate constant as a pointer, or pointer as a constant.
            ;; Running `simplify-pointer-and-constant` and fix them.
            (setf (iteration-space-strides (car (relay-read-iters (read-type-relay node))))
                  (loop repeat nrank collect (expr-const 0 :int64))))
          (when (= nrank 0)
            ;; If only used in this blueprint
            (let ((users (id->users (ctx-schedule-graph ctx) (car (node-writes node)))))
              (if (some #'(lambda (x) (getattr x :jitable)) users) ;; If the scalar is used in another jitable kernel?
                  (let* ((dtype (buffer-dtype (car (relay-writes (read-type-relay node)))))
                         (buffer (make-buffer `(1) `(0) dtype `((0 1 1 t)) :device 'RelayBuffer))
                         (space (buffer-merge-dims (ctx-schedule-graph ctx) buffer)))
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
    (multiple-value-bind (new-bp changed-p)
        (try-insert-node ctx node)
      (when (null changed-p) ;; if failed -> add the outermost loops and retry. (todo: which is not preferable though...)
        (setf (ctx-blueprint ctx) (append (initial-bp ctx) (ctx-blueprint ctx)))
        (multiple-value-bind (new-bp1 changed-p1)
            (try-insert-node ctx node)
          (setf new-bp new-bp1 changed-p changed-p1)))
      (assert changed-p () "recursive-lower-into-bp: Cannot insert the node ~a into a single kernel.
Depends=~a Reduce=~a Users=~a
```
~a
```" node (node-depend-idx-list node (ctx-gids ctx)) (node-reduced-gids node (ctx-gids ctx))
     (map 'list #'node-id (id->users (ctx-graph ctx) (car (node-writes node)))) (ctx-blueprint ctx))

     (setf blueprint new-bp)
     (mapc
      #'(lambda (x)
          (when (and (null (find x seen)) (id->value (ctx-graph ctx) x))
            (recursive-lower-into-bp ctx x)))
      (node-reads node))
      nil)))

(defun simplify-pointer-and-constant (blueprints)
  (let ((constants))
    (loop for bp in blueprints
          if (and (not (eql (node-class bp) :Render)) (getattr bp :declare-type)) do
            (push (car (node-writes bp)) constants))
    (loop for bp in blueprints
          if (not (eql (node-class bp) :Render)) do
            (loop for read in (node-reads bp)
                  for type in (relay-reads (read-type-relay bp))
                  for is in (relay-read-iters (read-type-relay bp))
                  for nth upfrom 0
                  if (and type is (find read constants)) do
                    ;; The load to :declare-type is a scalar.
                    (setf (nth nth (relay-reads (read-type-relay bp)))
                          (let ((buffer (caten/runtime:copy-buffer type)))
                            (setf (buffer-nrank buffer) -1)
                            buffer))))
    blueprints))

(defun gid-n (gid &aux (gid-str (princ-to-string gid)))
  (assert (equalp (subseq gid-str 0 4) "_gid"))
  (parse-integer (subseq gid-str 4)))

(defun ctx-padding-loop (ctx)
  "Inserts padding loops to keep the rank of loops same. Polyhedral Compiler should responsible for transforming this, so, caten/codegen should keep the ir untouched."
  (let* ((appeared
           (loop for bp in (ctx-blueprint ctx)
                 if (eql (node-type bp) :FOR)
                   collect bp))
         (threshold (and appeared (apply #'min (map 'list #'(lambda (x) (gid-n (getattr x :idx))) appeared))))
         (padding-loops
           (loop for gid in (ctx-gids ctx)
                 for size in (ctx-loop-size-list ctx)
                 if (and (expr-equal-to size 1)
                         (null (find gid appeared :key #'(lambda (x) (getattr x :idx))))
                         threshold
                         (< (gid-n gid) threshold)) ;; only the outermost loops are subject.
                   collect (cons (%make-for gid size) (%make-endfor gid)))))
    (when (null appeared) (return-from ctx-padding-loop (ctx-blueprint ctx)))
    `(,@(map 'list #'car padding-loops)
      ,@(ctx-blueprint ctx)
      ,@(reverse (map 'list #'cdr padding-loops)))))

(defun lower-schedule-item (node base-graph scheduled-graph) ;; Entry point for lowerer is here :)
  "
```
(lower-schedule-item node base-graph scheduled-graph)
```

Lowers the Schedule-Item into blueprint.

- node is a schedule-item to lower which belongs to scheduled-graph.

- base-graph is the graph you passed to the graph-schedule.

- scheduled-graph is the scheduled graph obtained by graph-schedule.
"
  (declare (type node node) (type graph base-graph scheduled-graph))
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
  ;; won't lower the allocation, they are the vm instruction.
  (when (null (getattr node :jitable)) (return-from lower-schedule-item))
  (let* ((graph (apply #'make-graph (getattr node :items)))
         (_ (setf (graph-outputs graph) (node-writes node)))
         (iterspace (get-grouped-dims graph base-graph))
         (__ (fixup-graph-iteration-space graph iterspace base-graph)) ; Use the common iteration space in the group
         (order (initial-loop-permutation graph (length (car iterspace)))) ;; permute reduce axes deeper
         (gids (permute-list order (map 'list #'gid (range 0 (length (car iterspace))))))
         (group-size (permute-list order (car iterspace))))
    (declare (ignore _ __))
    (graph-swizzle-space graph order)
    ;; gids       = (gid0, gid1, gid2, ...)
    ;; group-size = (10, 20, 30, ...)
    ;; order      = (0 1 2 ...) (the order of gids, and group-size)
    (let ((ctx (make-ctx :graph graph :order order :gids gids :loop-size-list group-size :blueprint nil :band2node (make-hash-table)
                         :schedule-graph scheduled-graph)))
      ;; Initially the blueprint starts with plain loops
      (setf (ctx-blueprint ctx) (initial-bp ctx))
      #+nil(trace caten/codegen/blueprint::recursive-lower-into-bp)
      #+nil(untrace caten/codegen/blueprint::recursive-lower-into-bp)
      (mapc #'(lambda (x) (recursive-lower-into-bp ctx x)) (graph-outputs graph))
      ;; Peforming the OpFusion to the lowered blueprint.
      (setf (ctx-blueprint ctx) (simplify-blueprint (ctx-blueprint ctx)) ; remove empty loop
            (ctx-blueprint ctx) (simplify-pointer-and-constant (ctx-blueprint ctx)) ; early scalarify
            (ctx-blueprint ctx) (blueprint-scalarify (ctx-blueprint ctx) node base-graph) ; rewrite local buffers as a scalar
            (ctx-blueprint ctx) (blueprint-exprify (ctx-blueprint ctx) node) ; rewrite jitable nodes -> expr
            (ctx-blueprint ctx) (ctx-padding-loop ctx)) ;; keep the rank of loops same
      (multiple-value-bind (new-bp id-as-dag-map) (blueprint-propagate-reduction (ctx-blueprint ctx)) ;; A = B + C * D => B += C * D
        ;; id-as-dag-map: subsequent kernels will recognise the key as value
        (setf (ctx-blueprint ctx) new-bp)
        ;; Synchronize the realized buffers
        (multiple-value-bind (writes reads constants) (blueprint-realized-buffers (ctx-blueprint ctx) node)
          (let ((before-assigned-map) (cycle))
            (loop for w in writes
                  if (and (gethash (car w) id-as-dag-map)) do
                    (if (find (car w) (node-writes node)) (push w cycle) (push (car w) before-assigned-map)))
            (setf (getattr node :read-types) (map 'list #'cdr reads)
                  (getattr node :write-types) (append (map 'list #'cdr cycle) (map 'list #'cdr writes))
                  (getattr node :storage-id-src) (map 'list #'car reads)
                  (getattr node :storage-id-dst) (append (map 'list #'car cycle) (map 'list #'car writes))
                  (getattr node :dynamic-shapes) constants
                  (node-reads node) (append before-assigned-map (map 'list #'car reads))
                  ;; If A is rewritten as B by the propagate-reduction, other items still recognise A as A.
                  (node-writes node) (append (map 'list #'car cycle) (map 'list #'(lambda (x) (or (gethash (car x) id-as-dag-map) (car x))) writes))))))
      (blueprint-set-iterations (ctx-blueprint ctx)) ;; Finalize the iteration space
      (setf (getattr node :blueprint) (ctx-blueprint ctx)
            (getattr node :blueprint-base) (copy-list (ctx-blueprint ctx))))))
;; ~~~ Schedule Cache ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defmethod find-cache-base-schedule-item ((node Node) (schedule-graph Graph))
  (let ((node (find (getattr node :cache-name) (graph-nodes schedule-graph) :key #'(lambda (x) (getattr x :name)))))
    (assert node () "find-cache-base-schedule-item: No cache item for ~a" node)
    node))

(defun compare-and-make-namespace-map (cache-node tgt-node)
  (let ((namespace-cache (getattr cache-node :namespace))
        (namespace-tgt-node (getattr tgt-node :namespace))
        (result (make-hash-table)))
    (assert (= (length namespace-cache) (length namespace-tgt-node)) () "compare-and-make-namespace-map: two schedules should have the same-sized namespace. Make sure that they are really equivalent schedule?~%~a~%~a" cache-node tgt-node)
    (loop for id-old in namespace-cache
          for id-tgt in namespace-tgt-node
          if (or (numberp id-old) (numberp id-tgt))
            do (assert (eql id-old id-tgt)
                       ()
                       "compare-and-make-namespace-map: Found an distinct point in the namespace ~a vs ~a~%NameSpace:~%~a~%~a~%Schedule-Items:~%~a~%~a"
                       id-old id-tgt namespace-cache namespace-tgt-node cache-node tgt-node)
               (let ((v (gethash id-tgt result))) (assert (or (null v) (eql v id-old))))
               (setf (gethash id-tgt result) id-old)
          else
            do (assert (and (symbolp id-tgt) (symbolp id-old)))
               (let ((v (gethash id-tgt result))) (assert (or (null v) (eql v id-old))))
               (setf (gethash id-tgt result) id-old))
    result))

(defmethod lower-cached-schedule-item ((node Node) (schedule-graph Graph))
  "Lowers the (optimized) blueprint from cached schedule."
  (assert (getattr node :cache-name))
  (assert (getattr node :jitable))
  (let* ((base-item (find-cache-base-schedule-item node schedule-graph))
         (namemap (compare-and-make-namespace-map node base-item)))
    (assert (getattr base-item :blueprint) () "The base-item is not lowered!")
    ;; Copying the following nodes from the base-item. But node/reads are mapped from namemap
    ;; :items (to ensure the equivalence of two kernels?)
    ;; :blueprint (and :EXPR inside) (memory-planner will use this)
    ;; :dynamic-shapes
    ;; :storage-id-src / :read-types
    ;; :storage-id-dst / :write-types
    (labels ((map-from (x &key (allow-nil t))
               (let ((val (gethash x namemap)))
                 (if val
                     val
                     (if allow-nil
                         x
                         (error "map-from: the id ~a is not found." x)))))
             (update-buffer (buffer)
               (when buffer
                 (let ((buffer (copy-buffer buffer)))
                   (setf (buffer-shape buffer) (map 'list #'map-from (buffer-shape buffer))
                         (buffer-stride buffer) (map 'list #'map-from (buffer-stride buffer))
                         (buffer-views buffer)
                         (loop for v in (buffer-views buffer)
                               collect
                               (map 'list #'map-from v))
                         (buffer-orig-buffer-shape buffer) (map 'list #'map-from (buffer-orig-buffer-shape buffer)))
                   buffer))))
      ;; what attrs does the mp use?
      (setf (node-reads node) (map 'list #'map-from (node-reads base-item))
            (node-writes node) (map 'list #'map-from (node-writes base-item))
            (getattr node :storage-id-src) (map 'list #'map-from (getattr base-item :storage-id-src))
            (getattr node :storage-id-dst) (map 'list #'map-from (getattr base-item :storage-id-dst))
            (getattr node :read-types) (map 'list #'update-buffer (getattr base-item :read-types))
            (getattr node :write-types) (map 'list #'update-buffer (getattr base-item :write-types))
            (getattr node :dynamic-shapes) (getattr base-item :dynamic-shapes))
      ;; creates a copy of blueprint, expr is also refreshed. Memory Planner will use this.
      (flet ((make-copy-of-bp (bp)
               (loop for bp-base in bp
                     for bp = (copy-node bp-base)
                     if (eql (node-class bp) :Render)
                       do (setf (node-reads bp) (map 'list #'map-from (node-reads bp))
                                (node-writes bp) (map 'list #'map-from (node-writes bp)))
                          (assert (not (eql (node-type bp) :AREF)))
                       and
                         collect bp
                     else
                       if (eql (node-type bp) :EXPR)
                         collect
                         (let ((expr-out-node (copy-node (expr-out (getattr bp :EXPR)))))
                           (when (eql (node-type expr-out-node) :Aref)
                             (setf (getattr expr-out-node :storage-id) (map-from (getattr expr-out-node :storage-id))))
                           (setf (node-reads bp) (map 'list #'map-from (node-reads bp))
                                 (node-writes bp) (map 'list #'map-from (node-writes bp))
                                 (node-reads expr-out-node) (map 'list #'map-from (node-reads expr-out-node))
                                 (node-writes expr-out-node) (map 'list #'map-from (node-writes expr-out-node))
                                 (getattr bp :EXPR)
                                 (make-expr
                                  :graph
                                  (apply
                                   #'make-graph
                                   (loop for expr-node-base in (graph-nodes (expr-graph (getattr bp :EXPR)))
                                         for expr-node = (copy-node expr-node-base)
                                         do (setf (node-reads expr-node) (map 'list #'map-from (node-reads expr-node))
                                                  (node-writes expr-node) (map 'list #'map-from (node-writes expr-node)))
                                            (when (getattr expr-node :_type_relay :allow-undefined t)
                                              (setf
                                               (relay-reads (read-type-relay expr-node)) (map 'list #'update-buffer (relay-reads (read-type-relay expr-node)))
                                               (relay-writes (read-type-relay expr-node)) (map 'list #'update-buffer (relay-writes (read-type-relay expr-node)))))
                                            (when (eql (node-type expr-node) :Aref)
                                              (setf (getattr expr-node :storage-id) (map-from (getattr expr-node :storage-id))))
                                         collect expr-node))
                                  :out expr-out-node))
                           bp)
                     else
                       do (error "lower-cached-schedule-item: Don't know how to transform the node ~a from the cached blueprint.~%Try NO_SCHEDULE_CACHE=1" bp))))
        (setf (getattr node :blueprint) (make-copy-of-bp (getattr base-item :blueprint))
              (getattr node :blueprint-base) (make-copy-of-bp (getattr base-item :blueprint-base)))))))
;;; ~~~~ GFlops Measurements ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
         (_ (assert (numberp (buffer-value ops)) () "measure-gflpos: the result is not a number."))
         (gflops (/ (buffer-value ops) (* elapsed 1e9))))
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
;; ~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun blueprint-gather-grids (blueprint &key (max-dimension 3) (dtype :int64))
  (let ((grids
          (loop for bp in blueprint
                if (and (eql (node-type bp) :EXPR) (typep (getattr bp :meta :allow-undefined t) 'ExprGrid))
                  collect (getattr bp :meta :allow-undefined t)))
        (out
          (loop for i upfrom 0 below max-dimension
                collect (make-instance 'ExprGrid :rank i :global-size (expr-const 1 dtype) :local-size (expr-const 1 dtype)))))
    (assert (<= (length grids) max-dimension) () "blueprint-gather-grids: the number of grids is over the limit.~%~a" blueprint)
    (dolist (g grids)
      (setf (nth (exprgrid-rank g) out) g))
    out))
