(defpackage :caten/codegen/blueprint
  (:use :cl :caten/air :caten/codegen/expr :alexandria)
  (:import-from
   :caten/codegen/shape-inference
   #:read-type-relay
   #:relay-read-iters
   #:relay-write-iters
   #:relay-reads
   #:relay-writes
   #:Iteration-Space
   #:iteration-space-shape
   #:iteration-space-strides
   #:iteration-space-views
   #:iteration-space-procedure
   #:%expr-const)
  (:import-from
   :caten/codegen/helpers
   :gid
   :range
   :permute-list)
  (:import-from
   :caten/avm
   #:buffer-shape
   #:buffer-stride
   #:buffer-views)
  (:import-from
   :caten/codegen/renderer
   #:render-expr
   #:Default-Renderer)
  (:export
   #:lower-schedule-item
   #:print-blueprint))

(in-package :caten/codegen/blueprint)

(defun %make-for (idx size)
  (make-node :Render :FOR nil nil :idx idx
             :upfrom (expr-const 0 :int64)
             :below (expr-< (expr-const idx :int64) size)
             :by (expr-const 1 :int64)))

(defun %make-endfor (idx)
  (make-node :Render :ENDFOR nil nil :idx idx))

(defmethod print-blueprint (nodes stream)
  (format
   stream
   "[Blueprint]{~%~a}"
   (with-output-to-string (out)
     (flet ((indent (n) (with-output-to-string (o) (dotimes (i n) (princ " " o)))))
       (loop with indent = 2
	     for node in nodes
	     if (eql (node-type node) :FOR)
	       do (format out "~afor (int ~(~a~)=~(~a~);~(~a~);~(~a~)+=~(~a~)) {~%" (indent indent) (getattr node :idx) (render-expr 'Default-Renderer (getattr node :upfrom)) (render-expr 'Default-Renderer (getattr node :below)) (getattr node :idx) (render-expr 'Default-Renderer (getattr node :by)))
		  (incf indent 2)
	     else if (eql (node-type node) :ENDFOR)
	            do (decf indent 2) (format out "~a} // ~(~a~)~%" (indent indent) (getattr node :idx))
	     else
	       do (format out "~aop[~a];~%" (indent indent) (node-type node)))))))

(defun remove-empty-loop (nodes &aux (removed nil))
  (loop for node in nodes
	for nth upfrom 0
	if (and (eql (node-type node) :FOR)
		(nth (1+ nth) nodes)
		(eql (node-type (nth (1+ nth) nodes)) :ENDFOR))
	  do (push (node-id (nth (1+ nth) nodes)) removed)
	else
          unless (find (node-id node) removed)
            collect node))

(defun simplify-blueprint (nodes)
  (let ((len (length nodes)))
    (setf nodes (remove-empty-loop nodes))
    (if (= (length nodes) len)
        nodes
        (simplify-blueprint nodes))))

(defmethod get-grouped-dims ((graph Graph) (global-iterator Iteration-Space))
  "Find out the iteration space that is common in the graph"
  (let* ((iterspace (iteration-space-shape global-iterator))
         (procedure (iteration-space-procedure global-iterator))
         (seen))
    ;; <Base_Loop>.len > <Collapsed Loops>.len が存在するとScheduleできない？
    (labels ((is-one (axis)
               (expr-scalar-equivalent-p axis (expr-const 1 :int64)))
             (merge-broadcast (space)
               (assert (= (length space) (length iterspace)))
               (setf
                iterspace
                (map
                 'list
                 #'(lambda (common-x new-y)
                     ;; Note: If Permuted?
                     (if (is-one common-x)
                         new-y
                         (progn
                           (unless (is-one new-y)
                             (assert (expr-scalar-equivalent-p common-x new-y) () "The size of adjacent loops should match (note: permuted?)~%~a~%~a" common-x new-y))
                           common-x)))
                 iterspace space)))
             (try-merge (space &aux (shape (iteration-space-shape space)))
               (when (null space) (return-from try-merge))
               (if (> (length shape) (length iterspace))
                   ;; Found higher rank? -> merge
                   (setf iterspace shape
                         procedure (iteration-space-procedure space))
                   (when (= (length shape) (length iterspace))
                     (merge-broadcast shape)))))
      (labels ((explore (id &aux (node (when (symbolp id) (id->value graph id))))
                 (when (and node (null (find id seen)))
                   (push id seen)
                   (flet ((f (x) (when x (try-merge x))))
                     (mapc #'f (relay-read-iters (read-type-relay node)))
                     (mapc #'f (relay-write-iters (read-type-relay node))))
                   (mapc #'explore (node-reads node)))))
        (mapc #'explore (graph-outputs graph))))
    (cons iterspace procedure)))

(defmethod fixup-graph-iteration-space ((graph Graph) found-pair g)
  "Rewrite the iteration space in the graph to match the common iteration space found in the get-grouped-dim function."
  (multiple-value-bind (found-space procedure) (values (car found-pair) (cdr found-pair))
    (labels ((merge-list (proc list)
               (loop for p in proc
                     collect
                     (apply #'expr-mul (map 'list #'(lambda (x) (%expr-const g (nth x list) :int64)) p))))
             (merge-stride (proc list)
               (loop for p in proc
                     collect
                     (%expr-const g (nth (car (last p)) list) :int64)))
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
                         (progn
                           ;; (assert (every #'null (map 'list #'(lambda (x) (nth x view)) p))
                           ;;         ()
                           ;;         "merge-view: collapsed axes are merged. ~a" view)
                           nil))))
             (fixup-dims (iterspace original-buffer)
               (when iterspace
                 (when (= (length (iteration-space-shape iterspace)) (length found-space))
                   (return-from fixup-dims))
                 (assert (<= (length (iteration-space-shape iterspace)) (length found-space))
                         ()
                         "The rank of the iteration space should be less than or equal to the found space~%~a~%~a" iterspace found-space)
                 ;; Caten cannot inference where to insert one here.
                 (assert (>= (length (alexandria:flatten (iteration-space-procedure iterspace))) (length found-space))
                         ()
                         "Cannot uprank ~a into the space ~a. The original buffer should be upranked in scheduler in advance." iterspace found-space)
                 (multiple-value-bind (new-shape new-stride new-view)
                     (values (merge-list procedure (buffer-shape original-buffer))
                             (merge-stride procedure (new-stride (buffer-stride original-buffer) (buffer-views original-buffer)))
                             (merge-view procedure (buffer-views original-buffer)))
                   (setf (iteration-space-shape iterspace) new-shape
                         (iteration-space-strides iterspace) new-stride
                         (iteration-space-views iterspace) new-view
                         (iteration-space-procedure iterspace) procedure)))))
      (dolist (n (graph-nodes graph))
        (mapc #'fixup-dims (relay-read-iters (read-type-relay n)) (relay-reads (read-type-relay n)))
        (mapc #'fixup-dims (relay-write-iters (read-type-relay n)) (relay-writes (read-type-relay n)))))))

(defmethod node-depend-idx-list ((node Node) gid
                                 &aux
                                   (type (read-type-relay node))
                                   (shapes (make-list (length gid))))
  "Enumerates a list of gid that the node depends on."
  (flet ((is-one (axis)
           (expr-scalar-equivalent-p axis (expr-const 1 :int64))))
    (dolist (space (append (relay-read-iters type) (relay-write-iters type)))
      (when space
        (loop for axis upfrom 0
              for shape in (iteration-space-shape space)
              do (push shape (nth axis shapes)))))
    (loop for g in gid
          for s in shapes
          if (not (every #'is-one s))
            collect g)))

(defun node-reduced-axes (node)
  (let ((is (car (relay-write-iters (read-type-relay node)))))
    (flet ((is-zero (axis)
             (expr-scalar-equivalent-p axis (expr-const 0 :int64))))
      (loop for s in (iteration-space-strides is)
            if (is-zero s)
              collect t
            else
              collect nil))))

(defun node-reduced-gids (node gids &aux (axes (node-reduced-axes node)))
  (assert (= (length gids) (length axes)) () "the reduction node ~a is not the highest rank tensor." node)
  (loop for nth upfrom 0
        for r in axes
        if r collect (nth nth gids)))

(defun graph-reduced-axes (graph rank-size)
  (let ((reduced-axes (make-list rank-size)))
    (dolist (node (graph-nodes graph))
      ;; Broadcasting information are always stored by the highest rank tensor.
      (when (car (relay-write-iters (read-type-relay node)))
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

(defstruct ctx
  "an intermidate object used to debug `recursive-lower-into-bp`"
  graph order blueprint seen gids loop-size-list)
;; ctx: print-object displays nothing not to collapse the trace macro.
(defmethod print-object ((ctx ctx) stream) (format stream "<CTX>"))

(defmethod initial-bp ((ctx ctx))
  (with-slots ((gids gids) (group-size loop-size-list) (order order)) ctx
    `(,@(permute-list order (map 'list #'%make-for gids group-size))
      ,@(reverse (permute-list order (map 'list #'%make-endfor gids))))))

(defun try-insert-node (ctx node
                        &key
                          (depend-idx) (depend-node) (path-reduced nil) (bp-limit nil)
                        &aux
                          (changed-p nil))
  (with-slots ((blueprint blueprint)) ctx
    (let ((satisfied)
          (idx-satisfied)
          (insertable-positions))
      (loop for bp in blueprint
            for nth upfrom 0
            if (eql (node-type bp) :FOR)
              do (push (getattr bp :idx) idx-satisfied)
            else
              if (eql (node-type bp) :ENDFOR)
                do (setf idx-satisfied (remove (getattr bp :idx) idx-satisfied))
            else
              do (dolist (r (node-reads bp))
                   (when (symbolp r)
                     (push r satisfied)))
            collect bp
            if (and
                (or (null bp-limit) (<= nth bp-limit))
                (every #'(lambda (x) (find x idx-satisfied)) depend-idx)
                ;; should not depends on reduced axes
                (every #'(lambda (x) (null (find x idx-satisfied))) path-reduced)
                (every #'(lambda (x) (null (find x satisfied))) depend-node))
              do (push nth insertable-positions))
      (when (null insertable-positions)
        (return-from try-insert-node (values blueprint nil)))
      (values
       (loop with insert-at = (apply #'max insertable-positions)
             for bp in blueprint
             for nth upfrom 0
             collect bp
             if (and (null changed-p) (= nth insert-at))
               do (setf changed-p t) and collect node)
       t))))

(defun recursive-lower-into-bp (ctx id &key (path-reduced nil) (parents nil) &aux (node (id->value (ctx-graph ctx) id)))
  ;; Try to insert the node ID into the above of parent
  (with-slots ((blueprint blueprint) (seen seen) (gids gids) (order order)) ctx
    (when (null node) (return-from recursive-lower-into-bp))
    (when (find id seen) (return-from recursive-lower-into-bp))
    (push id seen)
    (multiple-value-bind (new-bp changed-p)
        (try-insert-node ctx node :depend-idx (node-depend-idx-list node gids) :depend-node parents :path-reduced path-reduced)
      (if changed-p
          (setf blueprint new-bp)
          (let ((bp (initial-bp ctx)))
            ;; Cannot satify the dependency? create a new loops
            (setf blueprint (append bp blueprint))
            (multiple-value-bind (new-bp changed-p)
                (try-insert-node ctx node :depend-idx (node-depend-idx-list node gids) :depend-node parents :bp-limit (length bp) :path-reduced path-reduced)
              (assert changed-p () "Cannot insert the node ~a ~a~%[Ongoing blueprint]~%~a" (node-depend-idx-list node gids) node new-bp)
              (setf blueprint new-bp))))
      (mapc
       #'(lambda (x nth)
           (when path-reduced
             (assert (null (getattr node :reduction :allow-undefined t)) () "Detected double-reduce!"))
           (when (and (null (find x seen)) (id->value (ctx-graph ctx) x))
             (recursive-lower-into-bp
              ctx
              x
              :parents
              (loop for x in (remove-duplicates (append (node-reads node) parents))
                    if (symbolp x) collect x)
              :path-reduced
              (when (= nth 0)
                ;; If node is a reduce, the first argument should be separated from this loop.
                (if (getattr node :reduction :allow-undefined t)
                    (node-reduced-gids node gids) ;; Note that node-reduce-gids requires un-permuted gids
                    nil)))))
       (node-reads node) (range 0 (length (node-reads node)))))))

(defmethod lower-schedule-item ((node Node) (base-graph Graph))
  "Lowers the Schedule-Item into blueprint"
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
  ;; won't lower the allocation, they are the vm instruction.
  (when (getattr node :allocate-p) (return-from lower-schedule-item))
  (let* ((graph (apply #'make-graph (getattr node :items)))
         (_ (setf (graph-outputs graph) (node-writes node)))
         (iterspace (get-grouped-dims graph (getattr node :global-iterator)))
         (__ (fixup-graph-iteration-space graph iterspace base-graph)) ; Use the same iteration space in the group
         (group-size (car iterspace))
         (gids (map 'list #'gid (range 0 (length group-size))))
         (order (initial-loop-permutation graph (length group-size)))) ;; relocate reduction loop deeper
    (declare (ignore _ __))
    ;; gids       = (gid0, gid1, gid2, ...)
    ;; group-size = (10, 20, 30, ...)
    ;; order      = (0 1 2 ...) (the order of gids, and group-size)
    (print (permute-list order gids))
    (print gids)
    (print group-size)
    (fresh-line)
    (print graph)
    (let ((ctx (make-ctx :graph graph :order order :gids gids :loop-size-list group-size :blueprint nil)))
      ;; Initially the blueprint starts with plain loops
      (setf (ctx-blueprint ctx) (initial-bp ctx))
      (let ((p nil))
        #+nil(trace caten/codegen/blueprint::recursive-lower-into-bp)
        #+nil(untrace caten/codegen/blueprint::recursive-lower-into-bp)
        (mapc #'(lambda (x) (recursive-lower-into-bp ctx x :parents p) (setf p (node-reads (id->value graph x)))) (graph-outputs graph)))
      (setf (ctx-blueprint ctx) (simplify-blueprint (ctx-blueprint ctx)))
      (print-blueprint (ctx-blueprint ctx) t)
      ;; [TODO] 同じIteration内のOPたちを一つのExprにFuseする
      (setf (getattr node :blueprint) (ctx-blueprint ctx)))))

;; multi reduce (15:00 made)
;; - Load stand alone should only depends on _gid1
;; kyouha permute, graph partition made iketara ok
;; [!] Need process replay..
;; Involve the following things to the test
;; - [x] Initial Schedule
;;   - [x] Mean axis=0, axis=1,...
;;   - [x] Embedding
;;   - [ ] Double Reduce (add embedding embedding), add matmul matmul
;;   - [ ] Triple Reduce (add embedding embedding embedding)
;;   - [ ] WPE+WTE in Transformer is a single kernel.
;; - [ ] Permutation
;;   - [ ] Matmul, and ConvND
;; - [ ] Permute Fuse
;;   - [ ] Matmul+Transpose
;; - [ ] Graph Partition
;;   - [ ] Transfomer
;;   - [ ] Attention View will be properly scheduled?
;;   - [ ] Split the grpah as soon as :shrink was detected to schedule !randn
;; - [ ] Dynamic Shape
;; [TODO] (0 10 1 nil)はNILに書き換える
;; schedule.lispwokousin sinaito ugokanai rei:
;;(with-no-grad
;;            (time (caten/codegen:jit (caten (!add (make-tensor `(3 3)) (!sin (make-tensor `(3))))))))

;; (defparameter *model* (Transformer 64 4 2 1e-5 32))
;; (caten/codegen:jit (time (caten (call *model* (make-tensor `(1 10)) (iconst 'n)))))
;; [TODO] Loopの操作はPolyhedral Compilerに任せる。。。
;; Optimal Embeddingが無理だったら，GIDを，Reduceが一番最後に来るようにPermuteする。
;; - [ ] relu(gemm)
;; - [ ] conv
#|
for (int c0=0; c0<=2; c0+=)
  for (int c1=0; c1 <= 3; c1++)
     NID16190746(c0, c1);
   for (int c1=0; c1 <= 3; c1++)
     NID16190747(c0, c1);
   for (int c1=0; c1 <= 3; c1++)
     NID16190748(c0, c1);
  }
}
|#

#|
T=0
for (int c0=0; c0<=2; c0+=)
   for (int c1=0; c1 <= 3; c1++)
     NID16190748(c0, c1);
  }
}
T=1
for (int c0=0; c0<=2; c0+=)
   for (int c1=0; c1 <= 3; c1++)
     ...
   for (int c1=0; c1 <= 3; c1++)
     NID16190748(c0, c1);
  }
}
T=2

|#

;; Embedding: 5600, 90, 100
