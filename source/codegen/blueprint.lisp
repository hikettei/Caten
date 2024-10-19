(defpackage :caten/codegen/blueprint
  (:use :cl :caten/air :caten/codegen/expr :alexandria)
  (:import-from
   :caten/codegen/shape-inference
   #:read-type-relay
   #:relay-read-iters
   #:relay-write-iters
   #:relay-reads
   #:relay-writes
   #:iteration-space-shape
   #:iteration-space-strides
   #:iteration-space-views
   #:iteration-space-procedure
   #:%expr-const)
  (:import-from
   :caten/codegen/helpers
   :gid
   :range)
  (:import-from
   :caten/avm
   #:buffer-shape
   #:buffer-stride
   #:buffer-views)
  (:export
   #:lower-schedule-item))

(in-package :caten/codegen/blueprint)

(defun %make-for (idx size)
  (make-node :Render :FOR nil nil :idx idx
             :upfrom (expr-const 0 :int64)
             :below (expr-< (expr-const idx :int64) size)
             :by (expr-const 1 :int64)))

(defun %make-endfor (idx)
  (make-node :Render :ENDFOR nil nil :idx idx))

(defmethod get-grouped-dims ((graph Graph))
  (let* ((iterspace (iteration-space-shape (car (relay-write-iters (read-type-relay (id->value graph (car (graph-outputs graph))))))))
         (procedure (iteration-space-procedure (car (relay-write-iters (read-type-relay (id->value graph (car (graph-outputs graph))))))))
         (seen))
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
                 ;; [Assert] All buffers have the same rank.
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


;; [TODO] (0 10 1 nil)はNILに書き換える
;; schedule.lispwokousin sinaito ugokanai rei:
;;(with-no-grad
;;            (time (caten/codegen:jit (caten (!add (make-tensor `(3 3)) (!sin (make-tensor `(3))))))))

;; [TODO] Loopの操作はPolyhedral Compilerに任せる。。。
;; Optimal Embeddingが無理だったら，GIDを，Reduceが一番最後に来るようにPermuteする。
(defmethod node-depend-idx-list ((node Node) gid
                                 &aux
                                   (type (read-type-relay node))
                                   (shapes (make-list (length (iteration-space-shape (car (relay-write-iters type)))))))
  (flet ((is-one (axis)
           (expr-scalar-equivalent-p axis (expr-const 1 :int64))))
    (dolist (space (append (relay-read-iters type) (relay-write-iters type)))
      (when space
        (loop for axis upfrom 0
              for shape in (iteration-space-shape space)
              do (push shape (nth axis shapes)))))
    (assert (= (length gid) (length shapes)))
    (loop for g in gid
          for s in shapes
          if (not (every #'is-one s))
            collect g)))

;; 必要な変更
;; Broadcasted Axes -> Depends-onから外す
;; size=1 -> depends-onではない
;; Paretnt-reducedを追加する
;; from bottom to up で追加していく
(defmethod lower-schedule-item ((node Node) (base-graph Graph))
  "Lowers the Schedule-Item into blueprint"
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
  ;; nothing to schedule
  (when (getattr node :allocate-p) (return-from lower-schedule-item))
  (let* ((graph (apply #'make-graph (getattr node :items)))
         (_ (setf (graph-outputs graph) (node-writes node)))
         (iterspace (get-grouped-dims graph))
         (group-size (car iterspace))
         (gids (map 'list #'gid (range 0 (length group-size))))
         (blueprint)
         (seen nil))
    (declare (ignore _))
    (fixup-graph-iteration-space graph iterspace base-graph)
    (labels ((initial-bp ()
               `(,@(map 'list #'%make-for gids group-size)
                 ,@(map 'list #'%make-endfor (reverse gids))))
             (try-insert-node (node &key (depend-idx) (depend-node) &aux (changed-p nil))
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
                       if (and (every #'(lambda (x) (find x idx-satisfied)) depend-idx)
                               (= (length idx-satisfied) (length depend-idx))
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
                  t)))
             (explore (id &key (path-reduced nil) (parents nil) &aux (node (id->value graph id)))
               ;; Try to insert the node ID into the above of parent
               (when (null node) (return-from explore))
               (when (find id seen) (return-from explore))
               (push id seen)
               (multiple-value-bind (new-bp changed-p)
                   (try-insert-node node :depend-idx (node-depend-idx-list node gids) :depend-node parents)
                 (if changed-p
                     (setf blueprint new-bp)
                     (progn
                       ;; Cannot satify the dependency, create a new loops
                       (setf blueprint (append (initial-bp) blueprint))
                       (multiple-value-bind (new-bp changed-p)
                           (try-insert-node node :depend-idx (node-depend-idx-list node gids) :depend-node parents)
                         (assert changed-p () "Cannot insert the node ~a" node)
                         (setf blueprint new-bp))))
                 (mapc
                  #'(lambda (x)
                      (explore
                       x
                       :path-reduced (or path-reduced (getattr node :reduction :allow-undefined t))
                       :parents (append (node-reads node) parents)))
                  (node-reads node)))))
      (setf blueprint (initial-bp))
      (let ((p nil))
        (mapc #'(lambda (x) (explore x :parents p) (setf p (node-reads (id->value graph x)))) (graph-outputs graph))))
    (print blueprint)
    (setf (getattr node :blueprint) blueprint)))

;; merge-dims
;; Stride=1 -> aref(array, Index-Component(*iters))
#|
LOAD: 10000
INDEX-COMPONENTS: 100 100 (0 1)

|#
