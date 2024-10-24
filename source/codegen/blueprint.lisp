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
   :permute-list
   :render-list)
  (:import-from
   :caten/avm
   #:buffer-nrank
   #:buffer-shape
   #:buffer-stride
   #:buffer-views)
  (:import-from
   :caten/codegen/renderer
   #:render-expr
   #:Default-Renderer)
  (:import-from
   :caten/codegen/exprify
   #:graph-exprify
   #:graph-scalarify)
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
                                    (expr-mul (expr-const (third view) :int64) (expr-mul stride (expr-add (expr-const (car view) :int64) i)))
                                    (expr-mul stride i)))
                            (iteration-space-views is)
                            (iteration-space-strides is)
                            iterations))))
                 (format nil "~(~a~)" name)))
           (make-index-space ()
             (map 'list #'(lambda (x) (expr-const x :int64)) (reverse gids))))
    (format
     stream
     "{~%~a}~%"
     (with-output-to-string (out)
       (flet ((indent (n) (with-output-to-string (o) (dotimes (i n) (princ " " o)))))
         (loop with indent = 2
	       for node in nodes
	       if (eql (node-type node) :FOR)
	         do (format out "~afor (int ~(~a~)=~(~a~);~(~a~);~(~a~)+=~(~a~)) {~%" (indent indent) (getattr node :idx) (render-expr 'Default-Renderer (getattr node :upfrom)) (render-expr 'Default-Renderer (getattr node :below)) (getattr node :idx) (render-expr 'Default-Renderer (getattr node :by)))
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
                   (format out "~a~a = ~a;~a~%"
                           (indent indent)
                           (render-list
                            (map 'list #'(lambda (x y z) (print-aref x y z :iterations (or pre-iterations (make-index-space))))
                                 (node-writes node) (relay-writes (read-type-relay node)) (relay-write-iters (read-type-relay node))))
                           (render-expr 'Default-Renderer (getattr node :EXPR) :index-space (or pre-iterations (make-index-space)))
                           (if (getattr node :reduction :allow-undefined t)
                               " // :reduction=t"
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

(defmethod get-grouped-dims ((graph Graph))
  "Find out the iteration space that is common in the graph"
  (let* ((kernel-rank
           (loop for node in (graph-nodes graph)
                 for type = (read-type-relay node)
                 maximize
                 (loop for r in (append (relay-read-iters type) (relay-write-iters type))
                       when r maximize (apply #'max (flatten (iteration-space-procedure r))))))
         (pid2space (make-hash-table :test #'equal)))
    (labels ((is-one (expr)
               (expr-scalar-equivalent-p expr (expr-const 1 :int64)))
             (check (buffer space)
               (when (and buffer space)
                 (assert (= (apply #'max (flatten (iteration-space-procedure space))) kernel-rank) ()
                         "get-grouped-dims: Graph nodes should have the common rank. rank_size=~a but got ~a" kernel-rank space)
                 (loop for s in (iteration-space-shape space)
                       for p in (iteration-space-procedure space)
                       do (setf (gethash p pid2space)
                                (if (or (null (gethash p pid2space)) (is-one (gethash p pid2space)))
                                    s
                                    (gethash p pid2space))))))
             (explore (node)
               (mapc #'check (relay-reads (read-type-relay node)) (relay-read-iters (read-type-relay node)))
               (mapc #'check (relay-writes (read-type-relay node)) (relay-write-iters (read-type-relay node))))
             (related-keys (rank)
               (loop for key being the hash-keys of pid2space
                     if (find rank key) collect key)))
      (mapc #'explore (graph-nodes graph))
      (let* ((new-procedure
               (loop for dimension upfrom 0 below (1+ kernel-rank)
                     collect (car (sort (related-keys dimension) #'< :key #'length))))
             (new-procedure
               (loop with seen = nil
                     for p in new-procedure
                     if (null (find p seen :test #'equal))
                       collect (loop for x in p if (null (find x seen)) collect x and do (push x seen))
                       and do (push p seen)))
             (new-procedure (loop for p in new-procedure if p collect p)))
        (assert (equal (alexandria:flatten new-procedure) (range 0 (1+ kernel-rank))))
        (cons
         (map
          'list
          #'(lambda (x)
              (assert (gethash x pid2space) () "the axis ~a is not found" x)
              (gethash x pid2space))
          new-procedure)
         new-procedure)))))

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
                         nil)))
             (fixup-dims (iterspace original-buffer)
               (when iterspace
                 (when (= (length (iteration-space-shape iterspace)) (length found-space))
                   (return-from fixup-dims))
                 ;; Caten cannot inference where to insert one here.
                 (assert (>= (length (buffer-shape original-buffer)) (length found-space))
                         ()
                         "Cannot uprank ~a into the space ~a. The original buffer should be upranked in scheduler in advance." original-buffer found-space)
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

(defmethod reduce-bp ((ctx ctx) something gids)
  (let ((sizes (loop for g in gids
                     for p = (position g (ctx-gids ctx))
                     collect (nth p (ctx-loop-size-list ctx)))))
    `(,@(map 'list #'%make-for gids sizes)
      ,@something
      ,@(reverse (map 'list #'%make-endfor gids)))))

(defun try-insert-node (ctx node
                        &key
                          (depend-idx) (depend-node) (path-reduced nil) (bp-limit nil)
                          (reduce-p nil) (reduce-gids)
                        &aux
                          (changed-p nil)
                          (new-reduce-loop-p nil)
                          (node-reduce-p (getattr node :reduction :allow-undefined t)))
  (when reduce-p
    (assert (null path-reduced))
    (setf depend-idx (loop for d in depend-idx if (null (find d reduce-gids)) collect d)
          path-reduced reduce-gids))
  (with-slots ((blueprint blueprint)) ctx
    (let ((satisfied)
          (idx-satisfied)
          (insertable-positions))
      (when (and (null depend-idx) (null path-reduced))
        (push -1 insertable-positions))
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
                (or (null node-reduce-p) depend-idx (null idx-satisfied)) ;; If scalar computation is a reduction: write/read has a dependency.
                ;; should not depends on reduced axes
                (every #'(lambda (x) (null (find x idx-satisfied))) path-reduced)
                (every #'(lambda (x) (null (find x satisfied))) depend-node))
              do (push nth insertable-positions))
      (when (null insertable-positions)
        (return-from try-insert-node (values blueprint nil)))
      (when (= -1 (apply #'max insertable-positions))
        (return-from try-insert-node (values `(,node ,@blueprint) t)))
      (values
       (loop with insert-at = (apply #'max insertable-positions)
             for bp in blueprint
             for nth upfrom 0
             collect bp
             if (and (null changed-p) (= nth insert-at) (null reduce-gids))
               do (setf changed-p t) and collect node
             if (and (null changed-p) (= nth insert-at) reduce-gids)
               do (setf changed-p t new-reduce-loop-p t) and append (reduce-bp ctx (list node) path-reduced))
       t
       new-reduce-loop-p))))

(defun recursive-lower-into-bp (ctx id &key (path-reduced nil) (child-reduced nil) (parents nil) (new-reduce-loop-p nil)
                                &aux
                                  (node (id->value (ctx-graph ctx) id))
                                  (new-reduce-loop-p (or new-reduce-loop-p (and child-reduced (getattr node :reduction :allow-undefined t))))
                                  (reduce-p (and node new-reduce-loop-p (getattr node :reduction :allow-undefined t)))
                                  (reduced-axes (and node new-reduce-loop-p reduce-p (node-reduced-gids node (ctx-gids ctx)))))
  (with-slots ((blueprint blueprint) (seen seen) (gids gids) (order order)) ctx
    (when (null node) (return-from recursive-lower-into-bp))
    (when (find id seen) (return-from recursive-lower-into-bp))
    (push id seen)
    (multiple-value-bind (new-bp changed-p new-reduce-loop-p)
        (try-insert-node ctx node :depend-idx (node-depend-idx-list node gids) :depend-node parents :path-reduced path-reduced :reduce-p reduce-p :reduce-gids reduced-axes)
      (if changed-p
          (setf blueprint new-bp
                child-reduced (if new-reduce-loop-p nil child-reduced))
          (let ((bp (initial-bp ctx)))
            ;; Cannot satify the dependency? create a new loops
            (setf blueprint (append bp blueprint))
            (multiple-value-bind (new-bp changed-p new-reduce-loop-p)
                (try-insert-node ctx node :depend-idx (node-depend-idx-list node gids) :depend-node parents :bp-limit (length bp)
                                          :path-reduced path-reduced :reduce-p reduce-p :reduce-gids reduced-axes)
              (assert changed-p () "Cannot insert the node ~a
depending on ~a
depend-node=~a
path-reduced=~a
reduce-p=~a
reduce-gids=~a
[Ongoing blueprint]
~a"
                      node (node-depend-idx-list node gids)
                      parents path-reduced reduce-p reduced-axes
                      new-bp)
              (setf blueprint new-bp
                    child-reduced (if new-reduce-loop-p nil child-reduced)))))
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
                (if (getattr node :reduction :allow-undefined t)
                    (node-reduced-gids node gids)
                    nil))
              :child-reduced (or child-reduced (getattr node :reduction :allow-undefined t))
              :new-reduce-loop-p (= nth 0))))
       (node-reads node) (range 0 (length (node-reads node)))))
    nil))

(defmethod lower-schedule-item ((node Node) (base-graph Graph) (scheduled-graph Graph))
  "Lowers the Schedule-Item into blueprint"
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
  ;; won't lower the allocation, they are the vm instruction.
  (when (null (getattr node :jitable)) (return-from lower-schedule-item))
  (let* ((graph (apply #'make-graph (getattr node :items)))
         (_ (setf (graph-outputs graph) (node-writes node)))
         (iterspace (get-grouped-dims graph))
         (__ (fixup-graph-iteration-space graph iterspace base-graph)) ; Use the common iteration space in the group
         (group-size (car iterspace))
         (gids (map 'list #'gid (range 0 (length group-size))))
         (order (initial-loop-permutation graph (length group-size)))) ;; permute reduce axes deeper
    (declare (ignore _ __))
    ;; gids       = (gid0, gid1, gid2, ...)
    ;; group-size = (10, 20, 30, ...)
    ;; order      = (0 1 2 ...) (the order of gids, and group-size)
    (let ((ctx (make-ctx :graph graph :order order :gids gids :loop-size-list group-size :blueprint nil)))
      ;; Initially the blueprint starts with plain loops
      (setf (ctx-blueprint ctx) (initial-bp ctx))
      (let ((p nil))
        #+nil(trace caten/codegen/blueprint::recursive-lower-into-bp)
        #+nil(untrace caten/codegen/blueprint::recursive-lower-into-bp)
        (mapc #'(lambda (x) (recursive-lower-into-bp ctx x :parents p) (setf p (node-reads (id->value graph x)))) (graph-outputs graph)))
      (setf (ctx-blueprint ctx) (simplify-blueprint (ctx-blueprint ctx))
            (ctx-blueprint ctx) (graph-scalarify (ctx-blueprint ctx) node scheduled-graph)
            (ctx-blueprint ctx) (graph-exprify (ctx-blueprint ctx) node scheduled-graph))
      (when (>= (ctx:getenv :JIT_DEBUG) 2)
        (print-blueprint (ctx-blueprint ctx) t))
      (setf (getattr node :blueprint) (ctx-blueprint ctx)))))
