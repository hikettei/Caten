(defpackage :caten/codegen/schedule-graph
  (:documentation "TensorGraph => ScheduleGraph Lowerer")
  (:use :cl :caten/air :caten/aasm :caten/aasm/expr :caten/codegen/helpers)
  (:export
   #:tensor-graph->schedule-graph))

(in-package :caten/codegen/schedule-graph)
;; ~~ Grids ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Grids
  (is-affine t :type boolean)
  (id 0 :type fixnum)
  ;; iterator
  (items nil :type list))

(defstruct LowerCtx
  (id->bind (make-hash-table) :type hash-table))
;; ~~ Early Coalesce ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Iteration-Space
  (shape nil :type list)
  (strides nil :type list)
  (views nil :type list)
  (procedure nil :type list))

(defun relay-write-iters (relay)
  (declare (type Relay relay))
  (map 'list #'(lambda (x) (when x (tensor-relay-iterspace x))) (relay-writes relay)))

(defun (setf relay-write-iters) (value relay)
  (declare (type Relay relay) (type list value))
  (assert (= (length value) (length (relay-writes relay))))
  (loop for w in (relay-writes relay)
        for v in value
        do (when w (setf (tensor-relay-iterspace w) v))))

(defun relay-read-iters (relay)
  (declare (type Relay relay))
  (map 'list #'(lambda (x) (when x (tensor-relay-iterspace x))) (relay-reads relay)))

(defun (setf relay-read-iters) (value relay)
  (declare (type Relay relay) (type list value))
  (assert (= (length value) (length (relay-reads relay))))
  (loop for w in (relay-reads relay)
        for v in value
        do (when w (setf (tensor-relay-iterspace w) v))))

(defun reveal-buffer (object)
  (if (typep object 'TensorRelay)
      (if (null (tensor-relay-shape object))
          (or (tensor-relay-value object) object)
          object)
      object))

(defun gather-only-scalars (nodes)
  (loop for n in nodes
        if (and (= 0 (tensor-relay-nrank (car (relay-writes (read-type-relay n))))))
          collect n))

(defun %expr-const (graph value dtype)
  (let* ((val (reveal-buffer value)))
    (if (or (numberp val) (null (id->value graph val)))
        (expr-const val dtype)
        ;; Merge only scalar path!
        (expr-from-graph val (apply #'caten/air:make-graph (gather-only-scalars (graph-nodes graph)))))))

(defun mergeable-view-p (g view shape &aux (shape (if (typep shape 'Expr) shape (expr-const (reveal-buffer shape) :int64))))
  "Mergeable axis = view is not created."
  (when (null view) (return-from mergeable-view-p t))
  (when (expr-equal-to shape 1) (return-from mergeable-view-p (fourth view))) ;; Always collapse one as long as they are broadcasted.
  (trivia:ematch view
    ((list (eql 0) (trivia:guard x (expr-scalar-equivalent-p (expr-const x :int64) shape)) (eql 1) _) t)
    ;; considering the case: X = |val_15|, shape=a*b (a little heavy, so separated)
    ((list (eql 0) (trivia:guard x (expr-scalar-equivalent-p (%expr-const g x :int64) shape)) (eql 1) _) t)
    (_ nil)))

(defmethod iteration-space-sync-broadcast ((is Iteration-Space))
  (setf (iteration-space-views is)
        (loop for stride in (iteration-space-strides is)
              for view in (iteration-space-views is)
              for size in (iteration-space-shape is)
              if (eql stride 0)
                collect (or view (list 0 size 1 t))
              else
                collect view))
  is)

(defun merge-dims (g shape strides views &key (no-collapse nil))
  (declare (type list shape strides views))
  (when (null shape) (return-from merge-dims))
  (when (every #'null views) (setf views (loop repeat (length shape) collect nil)))
  (assert (= (length shape) (length strides) (length views)))
  ;; ret = (list new-shapes new-strides new-views)
  (let ((ret (list
              (list
               (%expr-const g (nth 0 shape) :int64)
               (%expr-const g (nth 0 strides) :int64)
               (nth 0 views)
               (list 0)))))
    (loop for nth upfrom 1 below (length shape)
          for size = (nth nth shape)
          for stride = (nth nth strides)
          for view = (nth nth views) do
            (multiple-value-bind (last-size last-stride last-view last-pd) (apply #'values (car (last ret)))
              (if (and
                   (null no-collapse)
                   (mergeable-view-p g last-view last-size)
                   (mergeable-view-p g view size)
                   (or
                    (when (expr-equal-to last-stride 0) (eql stride 0))
                    (expr-scalar-equivalent-p
                     last-stride
                     (expr-mul (%expr-const g size :int64) (%expr-const g stride :int64)))))
                  (setf (nth (1- (length ret)) ret)
                        (list (expr-mul last-size (%expr-const g size :int64)) (%expr-const g stride :int64) nil (append last-pd (list nth))))
                  (setf ret
                        (append
                         ret
                         (list (list (%expr-const g size :int64) (%expr-const g stride :int64) (if (mergeable-view-p g view size) nil view) (list nth))))))))
    (iteration-space-sync-broadcast
     (make-iteration-space
      :shape
      (loop for s in ret collect (first s))
      :strides
      (loop for s in ret collect (second s))
      :views
      (loop for s in ret collect (third s))
      :procedure
      (loop for s in ret collect (fourth s))))))

(defmethod tensor-relay-merge-dims ((graph Graph) (buffer TensorRelay))
  (let ((viewed-shape (tensor-relay-shape buffer))
        (strides (tensor-relay-stride buffer))
        (views (tensor-relay-views buffer)))
    (merge-dims
     graph
     ;; base-shape is set to nil if views are not created.
     viewed-shape
     (loop for stride in strides
           for nth upfrom 0
           for view = (nth nth views)
           if (and (listp view) (fourth view))
             collect 0 ;; Broadcasted -> stride is zero
           else
             collect stride)
     (or
      (when (some #'identity views) views)
      (loop repeat (tensor-relay-nrank buffer) collect nil)))))

(defmethod tensor-relay-iteration-space ((graph Graph) (buffer TensorRelay))
  (let ((viewed-shape (tensor-relay-shape buffer))
        (strides      (tensor-relay-stride buffer))
        (views        (tensor-relay-views buffer)))
    (merge-dims
     graph
     ;; base-shape is set to nil if views are not created.
     viewed-shape
     (loop for stride in strides
           for nth upfrom 0
           for view = (nth nth views)
           if (and (listp view) (fourth view))
             collect 0 ;; Broadcasted -> stride is zero
           else
             collect stride)
     (or
      (when (some #'identity views) views)
      (loop repeat (tensor-relay-nrank buffer) collect nil))
     :no-collapse t)))

(defmethod get-grouped-dims (items (base-graph Graph))
  "Infers the loop boundaries of the graph by finding the common iteration space."
  (let* ((kernel-rank
           (loop for node in items
                 for type = (read-type-relay node)
                 maximize
                 (loop for r in (append (relay-reads type) (relay-writes type))
                       when r maximize (length (tensor-relay-shape r)))))
         (pid2space (make-hash-table :test #'equal))
         (candidates nil))
    ;; Assuming all buffers in the graph have reshaped to `kernel-rank` by the scheduler.
    (labels ((is-one (expr) (expr-equal-to expr 1))
             (check (buffer &key (noopt t))
               (when buffer
                 (let ((space
                         (if noopt
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
                                            (gethash p pid2space)))))))))
             (explore (node &key (noopt t))
               (mapc #'(lambda (x) (check x :noopt noopt)) (relay-reads (read-type-relay node)))
               (mapc #'(lambda (x) (check x :noopt noopt)) (relay-writes (read-type-relay node)))))
      (mapc #'(lambda (x) (explore x :noopt nil)) items)
      (setf candidates (alexandria:hash-table-keys pid2space))
      (mapc #'explore items)
      (let ((new-procedure))
        (dolist (c (sort (copy-list candidates) #'< :key #'length))
          (when (every #'(lambda (x) (null (find x (alexandria:flatten new-procedure)))) c)
            (push c new-procedure)))
        (loop for i upfrom 0 below kernel-rank
              if (null (find i (alexandria:flatten new-procedure)))
                do (push (list i) new-procedure))
        (setf new-procedure (sort new-procedure #'< :key #'car))
        (assert (equal (alexandria:flatten new-procedure) (caten/codegen/helpers:range 0 kernel-rank)))
        (cons
         (map
          'list
          #'(lambda (x)
              (assert (gethash x pid2space) () "the axis ~a is not found from ~a" x (alexandria:hash-table-keys pid2space))
              (gethash x pid2space))
          new-procedure)
         new-procedure)))))

(defmethod fixup-items-iteration-space ((items list) found-pair g &aux (kernel-rank (reduce #'max (alexandria:flatten (cdr found-pair)) :initial-value 0)))
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
                         "(id=~a) Cannot uprank ~a into the space ~a. A original buffer should be upranked by the scheduler in advance.~%~a" id original-buffer found-space items)
                 (multiple-value-bind (new-shape new-stride new-view)
                     (values (merge-list procedure (tensor-relay-shape original-buffer))
                             (merge-stride procedure (new-stride (tensor-relay-stride original-buffer) (tensor-relay-views original-buffer)))
                             (merge-view procedure (tensor-relay-views original-buffer)))
                   (make-iteration-space
                    :shape new-shape
                    :strides new-stride
                    :views new-view
                    :procedure procedure)))))
      (dolist (n items)
        (setf (relay-read-iters (read-type-relay n)) (map 'list #'fixup-dims (node-reads n) (relay-reads (read-type-relay n)))
              (relay-write-iters (read-type-relay n)) (map 'list #'fixup-dims (node-writes n) (relay-writes (read-type-relay n))))))))

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

(defmethod iteration-space-expr-aref ((is Iteration-Space) (type TensorRelay) gids)
  "Returns a list of EXPR which (reduce #'+ ...) represents for the index."
  (assert (not (= (tensor-relay-nrank type) -1)) () "buffer-nrank = -1 means the array was mutated to scalar!")
  (let ((size (iteration-space-shape is))
        (stride (iteration-space-strides is))
        (view (iteration-space-views is)))
    (assert (= (length gids) (length size)) () "The iteration space and the buffer should have the same rank, getting gids=~a~%~a" gids is)
    (loop for s in stride
          for nth upfrom 0
          for i in gids
          for v = (nth nth view)
          if v
            collect (expr-mul s (expr-add (expr-const (car v) :int64) (expr-mul (expr-const (third v) :int64) (expr-const i :int64))))
          else
            collect (expr-mul (if (numberp i) (expr-const i :int64) i) s))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-grids-from-node (graph node id->grids id->users)
  (declare (type node node))
  (flet ((node-is-singleton-p (id &aux (node (id->value graph id)))
           (and
            node
            (= 1 (length (gethash (car (node-writes node)) id->users))))))
    (let ((next-id (hash-table-count id->grids)))
      (case (node-type node)
        (:Allocate
         (if (node-reads node)
             (make-grids :id next-id :is-affine nil :items (list node))
             (make-grids :id next-id :is-affine t :items (list node))))
        (:View (make-grids :id next-id :is-affine t :items (list node)))
        (otherwise
         (if (typep (node-attr node) 'JITAble)
             (let* ((parent-grids
                      (loop for r in (node-reads node)
                            for g = (gethash r id->grids)
                            if (and g (grids-is-affine g) (node-is-singleton-p r)) collect g))
                    (items (append (reduce #'append (map 'list #'grids-items parent-grids)) (list node)))
                    (new-grids
                      (make-grids :id next-id :items items)))
               (dolist (n items)
                 (dolist (w (node-writes n))
                   (setf (gethash w id->grids) new-grids)))
               new-grids)
             (make-grids :id next-id :is-affine nil :id 0 :items (list node))))))))

(defun lower-into-blueprint (lctx gids iterspace items writes reads write-types read-types)
  (with-blueprint (:noopt t)
    ;; [TODO]
    ;; Insert %global
    (loop for w in writes for wt in write-types do
      (%global w (tensor-relay-dtype wt) (not (= 0 (tensor-relay-nrank wt)))))
    (loop for r in reads for rt in read-types do
      (%global r (tensor-relay-dtype rt) (not (= 0 (tensor-relay-nrank rt)))))
    (let ((binds)
          (caten/aasm/expr::*expr-no-simplify-mode* t)
          (id->bind (lowerctx-id->bind lctx))
          (id->load (make-hash-table))) ;; cache is created for each kernel
      (print items)
      ;; [TODO]
      ;; - Reduction
      ;; - %SETF Handling
      (labels ((sendexpr (expr)
                 (dolist (n (graph-nodes (expr-graph expr))) (emit n))
                 (expr-out expr))
               (iter->index (iter typ)
                 (sendexpr (reduce #'expr-add (iteration-space-expr-aref iter typ gids))))
               (%insert-aref (item &aux (item (copy-node item)))
                 (setf (node-id item) (gensym "NID"))
                 (map
                  'list
                  #'(lambda (x) (emit x))
                  (let ((*ctx* (make-graph)))
                    (append
                     (loop for r in (node-reads item)
                           for rt in (relay-reads (read-type-relay item))
                           for ri in (relay-read-iters (read-type-relay item))
                           if (and (find r reads) (null (gethash r id->load)))
                             collect
                             (let ((index (iter->index ri rt))
                                   (tmpid (gensym "AREF")))
                               (setf (gethash r id->load) tmpid)
                               (%aref r index :out tmpid)))
                     (loop for w in (node-writes item)
                           for wt in (relay-writes (read-type-relay item))
                           for wi in (relay-write-iters (read-type-relay item))
                           for nth upfrom 0
                           if (find w writes)
                             collect
                             (let ((waypoint (gensym "WP"))
                                   (new-w (gensym "T"))
                                   (tmpid (gensym "T"))
                                   (index (iter->index wi wt)))
                               (setf (nth nth (node-writes item)) waypoint
                                     (gethash w id->bind) new-w)
                               ;; [note] no waypoint user in this items right?
                               (push (make-node :JIT :BIND (list new-w) (list tmpid) :value w) binds) ;; schedule all item users after %setf
                               (%setf (%aref w index) waypoint :out tmpid)))
                     (progn
                       (setf (node-reads item) (map 'list #'(lambda (x) (gethash x id->load x)) (node-reads item)))
                       (case (node-type item)
                         (:VIEW
                          (setf (gethash (car (node-writes item)) id->load) (car (node-reads item)))
                          nil)
                         (:Allocate (push item binds))
                         (otherwise (list (emit item))))))
                    (graph-nodes *ctx*))))
               (lower-item (item)
                 (case (node-type item)
                   (:INDEX-COMPONENTS (list (sendexpr (make-index-components item gids))))
                   (otherwise (%insert-aref item)))))
        (let ((body (%progn (reduce #'append (map 'list #'lower-item items)))))
          (loop for gid in gids for space in iterspace
                do (setf body (%range gid (sendexpr space) body)))
          (dolist (b binds) (emit b))
          body)))))

(defun grids-init (lctx graph grids id->grids id->users graph-outputs)
  (declare (type Grids grids) (type hash-table id->grids id->users) (type list graph-outputs) (optimize (speed 3)))
  (when (and (= 1 (length (grids-items grids)))
             (eql :VIEW (node-type (car (grids-items grids)))))
    (setf (grids-is-affine grids) nil))
  (labels ((node-is-output-p (node)
             (or
              (some
               #'(lambda (usr)
                   (let ((usr (gethash (car (node-writes usr)) id->grids)))
                     (not (= (grids-id grids) (grids-id usr)))))
               (gethash (car (node-writes node)) id->users))
              (find (the symbol (car (node-writes node))) graph-outputs)))
           (node-reads-from-another-grids (node)
             (loop for r in (node-reads node)
                   for typ in (relay-reads (read-type-relay node))
                   for g = (gethash r id->grids)
                   if (and (symbolp r) g (not (= (grids-id grids) (grids-id g)))) ;; collect when definition is not self
                     collect (cons r typ))))
    (let* ((grid-writes*
             (loop for item in (grids-items grids)
                   if (node-is-output-p item)
                     collect (cons (car (node-writes item)) (car (relay-writes (read-type-relay item))))))
           (grid-reads*
             (loop for item in (grids-items grids)
                   append (node-reads-from-another-grids item)))
           (grid-writes (map 'list #'car grid-writes*))
           (grid-reads (map 'list #'car grid-reads*))
           (grid-write-types (map 'list #'cdr grid-writes*))
           (grid-read-types  (map 'list #'cdr grid-reads*)))
      (unless (grids-is-affine grids)
        (return-from grids-init ($nonaffine grid-writes grid-reads :items (grids-items grids))))
      ;; Early Loop Coalesce (Cannot judged in polyhedral model)
      (let* ((iterspace (get-grouped-dims (grids-items grids) graph))
             (_ (fixup-items-iteration-space (grids-items grids) iterspace graph))
             (gids (map 'list #'gid (range 0 (length (the list (car iterspace)))))) ;; todo: initial perm?
             (bp (lower-into-blueprint lctx gids (car iterspace) (grids-items grids) grid-writes grid-reads grid-write-types grid-read-types)))
        (declare (ignore _))
        (setf bp (caten/aasm::%simplify-ast bp))
        (fresh-line)
        (caten/codegen/blueprint:print-blueprint bp t)
        
        ;; 1. generate blueprint
        ;; - 1. Compute Common Iteration Space
        ;; - 2. Lowerblueprint considering SETF (add global ctx)
        ;; - index components
        ;; - 3. Lower %for first
        ;; - 4. Scalarify
        ($affine grid-writes grid-reads)
        ))))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO] Run benchmark!
(defun tensor-graph->schedule-graph (graph)
  "Constructs ScheduleGraph from the given tensorgraph."
  (declare (type Graph graph) (optimize (speed 3)))
  (graph-infer-type-relay graph)
  (assert (null (graph-seen graph)) () "tensor-graph->schedule-graph: Scheduling partial graph is not allowed! Set graph-seen = nil")
  (let ((id->grids (make-hash-table)) (id->users (make-hash-table)) (queue)
        (in-degrees (make-hash-table)) (out-degrees (make-hash-table)))
    (flet ((butseen (list) (loop for l in list for v = (id->value graph l) if (and v (symbolp l)) collect v)))
      (loop for node in (graph-nodes graph) do
        (assert (= 1 (length (the list (node-writes node)))))
        (setf (gethash (node-id node) in-degrees) (butseen (node-reads node)))
        (dolist (r (butseen (node-reads node)))
          (let ((node-id (car (node-writes r))))
            (when (null (find (node-id node) (the list (gethash node-id id->users)) :key #'node-id))
              (push node (gethash node-id id->users))))
          (when (null (find (the symbol (node-id node)) (the list (gethash (node-id r) out-degrees)) :key #'node-id))
            (push node (gethash (node-id r) out-degrees))))))
    ;; [TODO]
    ;; Insert (car (node-writes backward)) to node-reads of all backward nodes
    (loop for node in (graph-nodes graph) if (null (gethash (node-id node) in-degrees)) do (push node queue))
    (loop while queue
          for node = (pop queue)
          for new-grid = (make-grids-from-node graph node id->grids id->users) do
            (dolist (w (node-writes node)) (setf (gethash w id->grids) new-grid))
            (dolist (adj (gethash (node-id node) out-degrees))
              (setf (gethash (node-id adj) in-degrees) (remove (node-id node) (gethash (node-id adj) in-degrees) :key #'node-id))
              (when (null (gethash (node-id adj) in-degrees))
                (push adj queue)))
            (remhash (node-id node) out-degrees))
    (assert (= 0 (hash-table-count out-degrees)) ()
            "The following nodes are not scheduled. circular dependencies?~%~a" (alexandria:hash-table-values out-degrees))
    ;; Construct Graph
    (let ((all-grids (make-hash-table)) (n-scheduled 0))
      (declare (type fixnum n-scheduled))
      ;; circular dependency of schedule graph? will it happen?
      (maphash
       #'(lambda (id grids)
           (declare (ignore id))
           (setf (gethash (grids-id grids) all-grids) grids))
       id->grids)
      (let ((g
              (loop with lctx = (make-lowerctx)
                    for key in (sort (the list (alexandria:hash-table-keys all-grids)) #'<)
                    for grids = (gethash key all-grids)
                    do (incf n-scheduled (length (the list (grids-items grids))))
                    collect (grids-init lctx graph grids id->grids id->users (graph-outputs graph)))))
        (assert (= n-scheduled (length (the list (graph-nodes graph)))))
        (setf g (apply #'make-graph g)
              (graph-outputs g) (graph-outputs graph))
        (->fast-graph g)))))
;; Solve Graph Partition Problem
(defun schedule-graph-solve-ilp (schedule-graph)
  ;; Objective: Maximize the volume of Affine Nodes
  ;; Firstly, single Affine single reduce
  ;; Secondly, fuse reduction and reduction
  ;; Create this form first:
  ;; ACC      |
  ;;   REDUCE | <== Scalarify!
  ;; STORE    |
  ;; And then fuse Reduce+Reduce
  ;; [TODO] Rename schedule-graph ==> Lowerer
  )
