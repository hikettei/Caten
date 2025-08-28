(defpackage :caten/codegen/lowerer
  (:documentation "TensorGraph => ScheduleGraph Lowerer")
  (:use :cl :caten/air :caten/aasm :caten/aasm/expr :caten/codegen/helpers
   :caten/codegen/search/polyhedral :caten/codegen/search/autotune
   :caten/codegen/search/ast :caten/runtime)
  (:export
   #:make-schedule-graph
   #:schedule-graph-fuse))

(in-package :caten/codegen/lowerer)
;; ~~ Grids ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Grids
  (is-affine t :type boolean)
  (is-zero-cost nil :Type boolean)
  (id 0 :type fixnum)
  ;; iterator
  (items nil :type list)
  (writes nil :type list)
  (reads nil :type list))

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
;; ~~~ Utils ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
;; ~~ Loop Coalesce (Tensor Level) ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
      
      ;; [todo] remove loop collapse at tensor lvl for symbolic fusion
      (mapc #'explore items)
      (setf candidates (alexandria:hash-table-keys pid2space))
      ;;(mapc #'(lambda (x) (explore x :noopt nil)) items)
      ;;(setf candidates (alexandria:hash-table-keys pid2space))
      ;;(mapc #'explore items)
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
        (assert (= 1 (length (node-writes n))))
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
;; ~~ Permute ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

(defun items-reduced-axes (items rank-size)
  (let ((reduced-axes (make-list rank-size)))
    (dolist (node items)
      ;; Broadcasting information are always stored by the highest rank tensor.
      (when (and
             (getattr node :reduction :allow-undefined t)
             (car (relay-write-iters (read-type-relay node))))
        (when (= rank-size (length (iteration-space-shape (car (relay-write-iters (read-type-relay node))))))
          (loop for nth upfrom 0
                for r in (node-reduced-axes node)
                if r do (setf (nth nth reduced-axes) t)))))
    reduced-axes))

(defun initial-loop-permutation (items rank)
  (let ((reduced (items-reduced-axes items rank))
        (stashed))
    ;; reduced axes should be the last
    `(,@(loop for p in (range 0 rank)
              for r in reduced
              if r ;; (reduced)
                do (push p stashed)
              else
                collect p)
      ,@(nreverse stashed))))

(defun items-permute-all (items order)
  (flet ((swizzle (id space)
           (when space
             (assert (length (iteration-space-procedure space)) () "graph-swizzle-loop-order: Cannot swizzle the space ~a ~a with ~a" id space order)
             (setf (iteration-space-shape space) (permute-list order (iteration-space-shape space))
                   (iteration-space-strides space) (permute-list order (iteration-space-strides space))
                   (iteration-space-views space) (permute-list order (iteration-space-views space))
                   (iteration-space-procedure space) (permute-list order (iteration-space-procedure space))))))
    (dolist (n items)
      (mapc #'swizzle (node-reads n) (relay-read-iters (read-type-relay n)))
      (mapc #'swizzle (node-writes n) (relay-write-iters (read-type-relay n))))))

(defun iterspace-depend-idx-list (iterspace gids &aux
                                                   (shapes (make-list (length gids)))
                                                   (strides (make-list (length gids))))
  (flet ((no-dep-p (size stride) (or (expr-equal-to size 1) (expr-equal-to stride 0))))
    (loop for axis upfrom 0
          for shape in (iteration-space-shape iterspace)
          for stride in (iteration-space-strides iterspace)
          do (push shape (nth axis shapes)) (push stride (nth axis strides)))
    (loop for g in gids
          for size in shapes
          for stride in strides
          if (not (every #'no-dep-p size stride))
            collect g)))
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun make-grids-from-node (graph node id->grids id->users)
  (declare (type node node))
  (flet ((node-is-singleton-p (id &aux (node (id->value graph id)))
           (and
            node
            (null (getattr node :reduction :allow-undefined t)) ;; Note: Solve Reduction+Activation in Polyhedral Model
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
             (let* ((is-reduce-p (getattr node :reduction :allow-undefined t))
                    (parent-grids
                      (loop for r in (node-reads node)
                            for g = (gethash r id->grids)
                            for nth upfrom 0
                            if (and g (grids-is-affine g) (node-is-singleton-p r)
                                    (if is-reduce-p (not (= nth 0)) t)) ;; if reduction, force realize at the first argument.
                              collect g))
                    (items (append (reduce #'append (map 'list #'grids-items parent-grids)) (list node)))
                    (new-grids
                      (make-grids :id next-id :items items)))
               (dolist (n items)
                 (dolist (w (node-writes n))
                   (setf (gethash w id->grids) new-grids)))
               new-grids)
             (make-grids :id next-id :is-affine nil :id 0 :items (list node))))))))
;; TODO: Test (!exp (!add (!sin (make-tensor `(3 3))) (make-tensor `(3 3)) :reduce t))
;; - Symbolic Schedule Fix
;; - Symbolic SCoP
(defun lower-into-blueprint (storage-map id->bind gids iterspace items writes reads write-types read-types)
  (with-blueprint (:noopt t)
    (let ((binds)
          (loads)    ;; float acc_0 = 0.0f;
          (alus)     ;; for (int i=0; i<100; i++) acc_0 += ...;
          (stores)   ;; out[0] = acc_0;
          (caten/aasm/expr::*expr-no-simplify-mode* t)
          (id->load (make-hash-table))) ;; cache is created for each kernel

      (labels ((sendexpr (expr)
                 (dolist (n (graph-nodes (expr-graph expr))) (emit n))
                 (expr-out expr))
               (iter->index (iter typ)
                 (sendexpr (reduce #'expr-add (iteration-space-expr-aref iter typ gids))))
               (scope= (queue current-dim)
                 (find current-dim (car queue)))
               (%insert-aref (item)
                 ;; Memory Loads
                 (loop for r in (node-reads item)
                       for rt in (relay-reads (read-type-relay item))
                       for ri in (relay-read-iters (read-type-relay item))
                       if (and (find r reads) (null (gethash r id->load)))
                         collect
                         (let ((index (iter->index ri rt))
                               (tmpid (gensym "val_")))
                           (setf (gethash r id->load) tmpid)
                           (push (cons (iterspace-depend-idx-list ri gids) (%aref (gethash r id->bind r) index :out tmpid)) loads)))
                 ;; Reductions/Stores
                 (if (getattr item :reduction :allow-undefined t)
                     (let ((reduce-to (id->value *ctx* (gethash (car (node-reads item)) id->load))))
                       (assert (eql (node-type reduce-to) :AREF) () "lower-into-blueprint: In the node Binary(X, Y, reduce=T), X should be realized!")
                       (let* ((type (read-type-relay item))
                              (w (car (node-writes item)))
                              (wi (car (relay-write-iters type)))
                              (r (car (node-reads item)))
                              (waypoint (gensym "WP"))
                              (tmp (gensym "R"))
                              (tmp1 (gensym "TMP"))
                              (tmp2 (gensym "TMP")))
                         ;; Reduction is lowered as:
                         ;; A <- Binary(B, C, reduction=T)
                         ;; ==>
                         ;; TMP = SETF(AREF(B, idx1), Binary(AREF(B, idx2), AREF(C, idx3)))
                         ;; A   = BIND(TMP, B) // Schedule after TMP, but memory is stored as B
                         (assert (= 1 (length (node-writes item))))
                         (setf (car (node-writes item)) waypoint)
                         (push (make-node :JIT :BIND (list tmp1) (list tmp) :value (gethash r id->load r)) binds)
                         (assert (find w writes) () "Reduction+Activation should not fused in advance ...")
                         (push (%setf (gethash r id->load r) waypoint :out tmp) alus)
                         (let ((force-load (copy-item reduce-to)))
                           (setf (node-writes force-load) (list (gensym "arf4rd")))
                           (push (cons (iterspace-depend-idx-list wi gids) (%setf (emit force-load) tmp1 :out tmp2)) stores)
                           (setf (gethash w id->bind) (car (node-reads item))))))
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
                                (setf (nth nth (node-writes item)) waypoint)
                                ;; [note] no waypoint user in this items right?
                                (push (make-node :JIT :BIND (list new-w) (list tmpid) :value w) binds) ;; schedule all item users after %setf
                                (push (cons (iterspace-depend-idx-list wi gids) (%setf (%aref w index) waypoint :out tmpid)) stores))))
                 (progn
                   (setf (node-reads item) (map 'list #'(lambda (x) (gethash x id->load x)) (node-reads item)))
                   (case (node-type item)
                     (:VIEW (error "view should be purged from items first."))
                     (:Allocate (push item binds) nil)
                     (otherwise (push (emit item) alus)))))
               (lower-item (item)
                 (case (node-type item)
                   (:INDEX-COMPONENTS (push (sendexpr (make-index-components item gids)) alus))
                   (otherwise (%insert-aref item)))))
        (mapc #'lower-item items)
        (let ((body (apply #'%progn alus)))
          (loop for gid in (reverse gids) for space in (reverse iterspace)
                do (setf body
                         (%range
                          gid (sendexpr space)
                          ;; Loads
                          (apply
                           #'%progn
                           (loop for queue in loads for nth upfrom 0
                                 when (and queue (scope= queue gid))
                                   collect (progn (setf (nth nth loads) nil) (cdr queue)))
                           ;; ALUs
                           (list body)
                           ;; Stores
                           (loop for queue in stores for nth upfrom 0
                                 when (and queue (scope= queue gid))
                                   collect (progn (setf (nth nth stores) nil) (cdr queue)))))))
          (flet ((e (id)
                   (when (gethash id id->bind)
                     (setf (gethash id storage-map) (gethash id id->bind)))
                   (gethash id id->bind id)))
            (loop for w in writes for wt in write-types do
              (%global (e w) (tensor-relay-dtype wt) (not (= 0 (tensor-relay-nrank wt)))))
            (loop for r in reads for rt in read-types do
              (%global (e r) (tensor-relay-dtype rt) (not (= 0 (tensor-relay-nrank rt))))))
          (dolist (b binds) (emit b))
          (%progn (node->id body)))))))

(defun copy-item (item &aux (item (copy-node item)))
  (setf (node-id item) (gensym "NID"))
  item)

(defun items/fold-and-verify-toplevel-views (items reads writes)
  (declare (optimize (speed 3)) (type list items reads writes))
  (let ((w->r (make-hash-table)))
    (loop for i in items
          if (eql (node-type i) :VIEW) do
;;            (assert (find (the symbol (car (node-reads i))) reads)
;;                    ()
;;                    "Detected illegal scheduling group: Affine groups should not compose multiple views.")
            (assert (null (find (the symbol (car (node-writes i))) writes))
                    ()
                    "Detected illegal scheduling group: Affine groups should not return VIEW.")
            (setf (gethash (car (node-writes i)) w->r) (car (node-reads i))))
    (flet ((n (id) (gethash id w->r id)))
      (loop for i in items
            if (not (eql (node-type i) :VIEW))
              collect
              (progn
                (setf (node-reads i) (map 'list #'n (node-reads i)))
                i)))))

(defun grids-ensure-affine (grids)
  ;; Affine composed of a single VIEW = NonAffine
  (when (and (= 1 (length (grids-items grids)))
             (eql :VIEW (node-type (car (grids-items grids)))))
    (setf (grids-is-affine grids) nil
          (grids-is-zero-cost grids) t))
  ;; Scalar Graph = NonAffine
  (when (and
         (grids-is-affine grids)
         (every
          #'(lambda (node)
              (and
               (every #'(lambda (x) (or (null x) (= 0 (the fixnum (tensor-relay-nrank x))))) (relay-writes (read-type-relay node)))
               (every #'(lambda (x) (or (null x) (= 0 (the fixnum (tensor-relay-nrank x))))) (relay-reads (read-type-relay node)))))
          (grids-items grids)))
    (setf (grids-is-affine grids) nil
          (grids-is-zero-cost grids)
          (every #'(lambda (node) (find (node-type node) `(:ALLOCATE :LOAD))) (grids-items grids)))))

(defun grids-init-edges (grids id->grids id->users graph-outputs)
  (declare (type Grids grids) (type hash-table id->grids id->users) (type list graph-outputs) (optimize (speed 3)))
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
    (let ((grid-writes*
            (loop for item in (grids-items grids)
                  if (node-is-output-p item)
                    collect (cons (car (node-writes item)) (car (relay-writes (read-type-relay item))))))
          (grid-reads*
            (loop for item in (grids-items grids)
                  append (node-reads-from-another-grids item))))
      (setf (grids-writes grids) grid-writes*
            (grids-reads grids) grid-reads*))))

(defun grids->schedule-item (id->bind graph grids val->grids)
  (assert (grids-writes grids))
  (let ((grid-reads (map 'list #'car (grids-reads grids)))
        (grid-writes (map 'list #'car (grids-writes grids)))
        (grid-read-types (map 'list #'cdr (grids-reads grids)))
        (grid-write-types (map 'list #'cdr (grids-writes grids))))
    (when (null (grids-is-affine grids))
      (return-from grids->schedule-item ($nonaffine grid-writes grid-reads :items (grids-items grids))))
    ;; If grids is affine => prepare for scheduling ...
    (let ((extra-items
            (loop for r in grid-reads for nth upfrom 0
                  for g = (gethash r val->grids)
                  if (and g (grids-is-zero-cost g))
                    do (setf (nth nth grid-reads) (map 'list #'car (grids-reads g))
                             (nth nth grid-read-types) (map 'list #'cdr (grids-reads g)))
                    and append (grids-items g))))
      (setf grid-reads (alexandria:flatten grid-reads)
            grid-read-types (alexandria:flatten grid-read-types))
      (setf (grids-items grids)
            (items/fold-and-verify-toplevel-views (map 'list #'copy-item (append extra-items (grids-items grids))) grid-reads grid-writes)))
    ;; Early Loop Coalesce (Cannot judged in polyhedral model)
    (let* ((iterspace (get-grouped-dims (grids-items grids) graph))
           (_ (fixup-items-iteration-space (grids-items grids) iterspace graph))
           (order (initial-loop-permutation (grids-items grids) (length (the list (car iterspace)))))
           (gids (permute-list order (map 'list #'gid (range 0 (length (the list (car iterspace)))))))
           (group-size (permute-list order (car iterspace)))
           (__ (items-permute-all (grids-items grids) order))
           (storage-map (make-hash-table))
           (bp (lower-into-blueprint storage-map id->bind gids group-size (grids-items grids) grid-writes grid-reads grid-write-types grid-read-types))
           (has-reduce-p (some #'(lambda (x) (getattr x :reduction :allow-undefined t)) (grids-items grids))))
      (declare (ignore _ __))
      (setf bp (caten/aasm::%simplify-ast bp))
      (let ((args (loop for item in (graph-nodes bp) if (eql (node-type item) :DEFINE-GLOBAL) collect (car (node-writes item)))))
        ($affine grid-writes
                 (loop for r in grid-reads
                       if (find (gethash r storage-map r) args)
                         collect r)
                 :polyhedron (make-polyhedral-schedule-item bp :scal->array nil)
                 :blueprint bp
                 :reduction has-reduce-p
                 :storage-map storage-map)))))
;; ~~~ Entry Points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; [TODO]
;; - [x] Rename: make-schedule-graph
;; - [ ] Schedule involving scalars
;; - [ ] Schedule threefry (Reduce)
;; - [ ] KVCache Scheduling
;; - [x] SETF Bind failing case w/ Softmax CSE
(defun make-schedule-graph (graph)
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
    (let ((all-grids (make-hash-table)) (n-scheduled 0) (val->grids (make-hash-table)))
      (declare (type fixnum n-scheduled))
      ;; circular dependency of schedule graph? will it happen?
      (maphash
       #'(lambda (id grids)
           (declare (ignore id))
           (setf (gethash (grids-id grids) all-grids) grids))
       id->grids)
      ;; [todo] parallelize grids-init w/ lparallel!
      (mapc #'grids-ensure-affine (alexandria:hash-table-values id->grids))
      (mapc
       #'(lambda (x)
           (grids-init-edges x id->grids id->users (graph-outputs graph))
           (dolist (w (grids-writes x))
             (setf (gethash (car w) val->grids) x)))
       (alexandria:hash-table-values id->grids))
      (let ((g
              (loop with id->bind = (make-hash-table)
                    for key in (sort (the list (alexandria:hash-table-keys all-grids)) #'<)
                    for grids = (gethash key all-grids) do (incf n-scheduled (length (the list (grids-items grids))))
                    collect (grids->schedule-item id->bind graph grids val->grids))))
        (assert (= n-scheduled (length (the list (graph-nodes graph)))))
        (setf g (apply #'make-graph g)
              (graph-outputs g) (copy-list (graph-outputs graph)))
        (setf g (->schedule-graph g))
        (verify-graph g)
        g))))
;; ~~ TopLevel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun schedule-item-apply-schedule (graph item &key (allow-fission nil) (keep-scop t))
  (declare (type Node item))
  (assert (eql :Affine (node-type item)))
  (let ((kernels (apply-schedule (psi-theta (getattr item :polyhedron)) (getattr item :blueprint))))
    (assert (= 1 (length kernels)) () "schedule-graph-apply-schedule: Cannot schedule multiple kernels for a single affine object at this level.")
    (let* ((singletons
             (loop for r in (node-writes item)
                   if (and (= 0 (length (id->users graph r))) ;; todo:optimize id->users
                           (null (find r (graph-outputs graph))))
                     collect r))
           (kernel
             (if keep-scop
                 (caten/aasm::%simplify-ast
                  (ast-merge-expr-from-aref-subgraph
                   (ast-remove-extra-memloads (car kernels) singletons)))
                 (car kernels)))
           (args (loop for item in (graph-nodes kernel)
                       if (eql (node-type item) :DEFINE-GLOBAL)
                         collect (car (node-writes item)))))
      ;; (caten/codegen/blueprint:print-blueprint kernel t)
      (setf
       (node-writes item) (loop for w in (node-writes item) if (find w args) collect w)
       (node-reads item) (loop for r in (node-reads item) if (find r args) collect r)
       (getattr item :blueprint) kernel
       (getattr item :polyhedron)
       (make-polyhedral-schedule-item
        (getattr item :blueprint) :scal->array allow-fission
                                  :opt-history (psi-opt-history (getattr item :polyhedron))))))
  item)

(defun schedule-graph-apply-schedule (graph &key (allow-fission nil) (keep-scop t))
  "Recompute (out-of-date) blueprint w/ updated schedule."
  (declare (type ScheduleGraph graph))
  ;; [TODO] use lparallel:pdotimes, this can be parallelized.
  (dolist (item (graph-nodes graph))
    (when (eql (node-type item) :Affine)
      (schedule-item-apply-schedule graph item :allow-fission allow-fission :keep-scop t)))
  (verify-graph graph)
  graph)
;; ~~ Fusion Utilities ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun blueprint-sequence (x y)
  (with-blueprint (:noopt t)
    (dolist (e (graph-nodes x)) (emit e))
    (dolist (e (graph-nodes y)) (emit e))
    (%progn (graph-outputs x) (graph-outputs y))))

(defun merge-affine (a1 a2 new-poly)
  (let ((r (loop for r in (append (node-reads a1) (node-reads a2))
                 if (and (null (find r (node-writes a1))) (null (find r (node-writes a2))))
                   collect r)))
    ($affine (append (node-writes a1) (node-writes a2)) (remove-duplicates r)
             :polyhedron new-poly
             :blueprint (blueprint-sequence (getattr a1 :blueprint) (getattr a2 :blueprint))
             :reduction (or (getattr a1 :reduction) (getattr a2 :reduction)))))

(defun affine/solve-ilp-fusion-pair (parent child &key (mode :full)) ;; assuming parent.order < child.order
  (declare (type Node parent child))
  (assert (eql (node-type parent) :Affine)) (assert (eql (node-type child) :Affine))
  (flet ((m (x) (getattr x :polyhedron)))
    ;; [TODO] childに含まれている各Domainをどこに挿入するか，という問題に変える
    ;; もっと言えば順序付けしない
    ;; AffineにPrintされてる情報でCacheを作成できる
    ;; - [ ] 1. Cacheを実装する
    ;; - [ ] 2. 少しHeavyなILPベースでFusionを実施する
    ;; - [ ] 3. ある程度データが集まったら，検索ベースでFusionを実施するアルゴリズムを作る
    ;; Parent/Childは同一RankのTensor操作だと仮定する
    (let ((fused (ILP/SolveProximity (m parent) (m child) :mode mode)))
      (when fused
        (merge-affine parent child fused)))))

(defun affine/solve-ilp-fusion (unfused-ops parent children block &key (mode :full))
  (declare (type Node parent) (type list children))
  (when (not (= 1 (length children)))
    (flet ((p (i) (or (position (node-writes i) unfused-ops :key #'node-writes :test #'intersection) 0)))
      ;; [TODO] Is this sorting method valid? or needed?
      (setf children (sort children #'< :key #'p))))
  (dolist (c children)
    (when (null (find (node-id c) block :key #'node-id))
      (let ((fused (affine/solve-ilp-fusion-pair parent c :mode mode)))
        (when (null fused) (return-from affine/solve-ilp-fusion))
        (setf parent fused))))
  parent)

(defun generate-seed (ops)
  (declare (type list ops))
  (find-if #'(lambda (node) (and (eql (node-type node) :Affine) (getattr node :reduction))) ops))

(defun fuse-successor (unfused-ops graph sp sucs block &key (mode :full))
  (declare (type ScheduleGraph graph) (type list block sucs unfused-ops) (type node sp))
  ;; Only Affine is mergeable
  (when (or (= 0 (length sucs)) (some #'(lambda (x) (eql (node-type x) :NonAffine)) sucs))
    (return-from fuse-successor sp))
  (let ((fused (affine/solve-ilp-fusion unfused-ops sp sucs block :mode mode)))
    ;; Stop when no fusable pairs, or fusion is not beneficial.
    (when (null fused) (return-from fuse-successor sp))
    (dolist (w (node-writes fused)) (remnode graph w))
    (insert-nodes graph (list fused))
    (verify-graph graph)
    (setf fused (schedule-item-apply-schedule graph fused :allow-fission nil)) ;; [TODO] is it slow?
    (setf block (nconc block sucs))
    (dolist (w (node-writes fused))
      (setf fused (fuse-successor unfused-ops graph fused (id->users graph w) block :mode mode)))
    fused))

(defun schedule-graph-fuse (graph)
  "Solve ILP to minimize proximity while maximizing benefit."
  (declare (type ScheduleGraph graph))
  (let ((unfused-ops (tpsort-graph graph))) ;; explore from leaves to roots.
    ;; Step1. Explore top-down.
    ;; - Maximize Reduce+Reduce Fusion (may change the memory order signifcantly)
    (loop for sp = (generate-seed unfused-ops)
          while sp for block = (list sp) do
            ;; pop first reduction from unfused-ops, and pair them w/ another reduction who lives in descendants.
            ;; Head to successor
            (dolist (w (node-writes sp))
              ;; [todo] optimize id->users which is O(N)
              (fuse-successor unfused-ops graph sp (id->users graph w) block :mode :full))
            ;; Update unfused-ops
            (loop for b in block do
              (setf unfused-ops (remove (node-id b) unfused-ops :key #'node-id))))
    ;; Step2. Fuse predecessors.
    ;; - Fuse remained elwise ops if not required by its children.
    (loop for sp = (pop unfused-ops)
          while sp for block = (list sp)
          if (eql (node-type sp) :Affine) do
            (dolist (w (node-writes sp))
              (fuse-successor unfused-ops graph sp (id->users graph w) block :mode :partial))
            (loop for b in block do
              (setf unfused-ops (remove (node-id b) unfused-ops :key #'node-id))))
    (assert (null unfused-ops))
    graph))

;; [TODO] Runtime is a subclass of FastGraph
;; [TODO] Introduce LocalGensym
;; - CostModelを切り替えて，OfflineでBEAM Search, OnlineでBEAM Search, 両方可能にする
;; - LLM => BatchSizeをIterateしてBEAM Search...
;; - remove marks
;; [TODO] Reimplement api
;; - Node is always singleton (Cache)
;; - Faster compilation time
;; [TODO]
;; - BEAM Search: ScheduleGraphの状態のまま解く
;; - Symbolicも最適化できるようにする
;; - Nonaffineも普通に実行すればいい
;; - TensorID -> (cons speed kernel) mitaini cache sitai
;; - symbolic graph fusion?
;; - quasi affine?
;; - AccessMapさえ作れればいい。gidの係数ごとにlexiographical order?
;;   - w/ assuming each coefficient is constant.
;;   - 次元数も関係ない。
;;   | M*N | M | 1 |
;;   --------------|
;; S |  0  | 0 | 1 | = index
;; Affine/NonAffineのまま動かすために
;;
(defmethod realize-node ((node-type (eql :NonAffine)) runtime node args)
  (flet ((v (id) (if (symbolp id) (runtime-getvar runtime id) id)))
    (dolist (item (getattr node :items))
      (loop for w in (node-writes node)
            for v in (multiple-value-list (realize-node (node-type item) runtime item (map 'list #'v (node-reads item))))
            do (runtime-setvar runtime w v)))
    (apply #'values (map 'list #'v (node-writes node)))))

(defmethod realize-node ((node-type (eql :Affine)) runtime node args)
  (ILP/Search node) ;; :cost-model (make-instance runtime args)
  (error "STOP"))

(defun schedule-graph-search (graph)
  "BEAM Search for Affine Schedule Items"
  (declare (type ScheduleGraph graph))
  (let ((runtime
          (make-runtime
           (make-graph)
           :runtime (caten/codegen/byoc:get-runtime-type)
           :buffer-type (caten/codegen/byoc:get-buffer-type))))
    (flet ((v (id) (if (symbolp id) (runtime-getvar runtime id) id)))
      (dolist (item (tpsort-graph graph))
        (ecase (node-type item)
          ((:Affine :NonAffine)
           (loop for w in (node-writes item)
                 for v in (multiple-value-list (realize-node (node-type item) runtime item (map 'list #'v (node-reads item))))
                 do (runtime-setvar runtime w v)))))
      (free-runtime runtime))))

(defun schedule-graph-solve-memory-planner (graph)
  "Solve ILP to minimize the number of temporary buffer allocation."
  (declare (type ScheduleGraph graph))
  ;; Default:
  ;; - (*) write_id = get_from_memory_pool(id)
  ;; id is subject to optimize.
  )

(defun schedule-graph-finalize (graph)
  "Finalize ScheduleGraph+Construct a runtime graph."
  (declare (type ScheduleGraph graph))
  (dolist (item (tpsort-graph graph))
    (when (eql (node-type item) :Affine)
      (print (isl:schedule-get-root (psi-theta (getattr item :polyhedron))))
      (print (psi-read-union-map  (getattr item :polyhedron)))
      (print (psi-write-union-map  (getattr item :polyhedron)))
      ;;(caten/codegen/blueprint:print-blueprint (getattr item :blueprint) t)
      )))
;; Memo:
;; - [ ] ISL = Schedule and Memory Access Separation
;; - [ ] DataFlowGraph = MemoryAccessRelation+Schedule
;; - [ ] ScheduleGraph <==> DataFlowGraph Constructionを実装する
;;   - [ ] いや，BANDもしかしてBandがDEFINE-GLOBALなどに対応するのでは？
;;   - [ ] ScheduleNodeBand: LV(BAND_DEPTH), AREA=512x512
;; - [ ] DataFlowGraphから，FusionのStrategyを生成する方法の提案をしたい
;; - [ ] やることを一般化し範囲を絞って言語化すると，サイズが大きいが速度が遅いN段階のメモリがある。これがブラックボックスだとして，自動でマッピングするモデルの構築
;; - [ ] あと2週間あれば完成しそうな目処がたった！
;; - [ ] Schedule作り直し+差し替え+Refactor (2 or 3 days)
;;   - [ ] RuntimeGraph, TensorGraph, ...
;; - [ ] caten/api作り直す, reimpl autodiff (maybe 2 or 3 days)
;; - [ ] BEAM Search/BYOC作り直す           (2 days)
;; - [ ] テスト作り直す                     (2 days)

;; [TODO]
;; - [ ] Fusionをもう少し賢く実施したい。
;; - [ ] Memory Access Map => 依存違反で使うのではなく，最初からこれベースでもっと賢く実施
;; - [ ] ReSCoP, CSEを最初と最後の一回のみにしたい。
;; - [ ] UnionMapとScheduleからどこに融合するかって一発で作れそうなもんに見える
;; - [ ] DataFlowGraphを作成
;; - [ ] DEFINE-GLOBALを経由した数，VRAM <-> SRAMの読み込みなどを表現, これをCost Functionにする？
;; - [ ] 各変数について，iPadに記したみたいなDataFlowGraphを作成する
;;  - [ ] Visualize!
;; - [ ] うまくいけば途中でCSEせずにFusion続行？
;; - [ ] Read/WriteUnionMapのアクセス ==> Sort(UnionMap)ができる性質があればOK
;; - [ ] ScheduleTreeとDataFlowGraphだけで判定して軽量化...はできる
;; - [ ] ScheduleTree/Read/WriteUMap ==> DataFlowGraph
;; - [ ] Simplify(DataFlowGraph)
;;  - [ ] TILEしたらL1(DEFINE-GLOBAL) -> L2(more fater but small mem) -> L3(more ...) を明示的に作る？
;; (***) ManySchedule vs OneFilterScheduleで，FusionはOneFilterScheduleを適切な場所でInsertする操作だと考える。
;;  - [ ] i.e.: SCC同士でFusionをする。Oneの方のPlaceableな地点のリストを列挙, あるいはManyの方に何らかの操作をして
;;   - こうすればReSCOPifyが必要なくなる！！
;;  - [ ] TILEすると，AREF(GLOBAL_MEM_VAL, idx)が，AREF(GLOBAL_MEM_VAL, 0 <= idx <= TILE_SIZE)になる。
;;   - 0 <= idx-parent_loop_idx <= 0+TILE_SIZE
;;  - [ ] DataFlowGraphを実装する。これはSchedule, AST, どっちに対して実装するといい？
;;    - [ ] => 多分ScheduleTree, ただしScheduleTreeをもう少し理解しないといけない。。。
;; [DataFlowGraph]
;; - [ ] ScheduleNodeBand, 各Bandの深さ=メモリ階層のLVL
;; - [ ] ASTUserがどこに常に生成されるのか確認しないといけない。
;; - [ ] Fusion実装できたら，codegenの必要ないコード全部消す。
;; - [ ] caten aasm -> caten/ir
;; - [ ] ScheduleTree ==> DataFlowGraphを作成する。
;;   - [ ] これはTile探索のVislizeも兼ねる
;; ↓これがFusionできないといけない。
;; (let ((tg (tensor-lowered-graph (!sin (!t (!relu (!matmul (make-tensor `(1024 1024)) (make-tensor `(1024 1024)))))))))
;;              (time (caten/codegen/lowerer::codegen tg)))
(defun schedule-item-to-optrules ()

  )
;; Visualizeが大事？
(defun codegen (graph)
  (declare (type Graph graph))
  (let ((sched (make-schedule-graph graph)))
;    (schedule-graph-fuse sched) Minimize Proximity
    (schedule-graph-solve-memory-planner sched)
    (schedule-graph-finalize sched)
    sched))
