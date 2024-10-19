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

(defun get-kernel-rank (node)
  (declare (type node node))
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
  (flet ((rank-of (x)
           (if x
               (length (iteration-space-shape x))
               0)))
    (apply
     #'max
     (loop for item in (getattr node :items)
           append (map 'list #'rank-of (relay-read-iters (read-type-relay item)))
           append (map 'list #'rank-of (relay-write-iters (read-type-relay item)))))))

(defun get-iterators (node kernel-rank)
  (declare (type node node))
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
   (loop for item in (getattr node :items)
         append
         (loop for w in `(,@(relay-read-iters (read-type-relay item)) ,@(relay-write-iters (read-type-relay item)))
               if (and w (= (length (iteration-space-shape w)) kernel-rank))
                 append (iteration-space-shape w))))

(defun item->iteration-space (item)
  (declare (type node item))
  (flet ((rank-of (x)
           (if x
               (length (iteration-space-shape x))
               0)))
    ;; ここってcar rank-ofでも同じでは？
    (let* ((rank
             (apply
              #'max
              (append
               (map 'list #'rank-of (relay-read-iters (read-type-relay item)))
               (map 'list #'rank-of (relay-write-iters (read-type-relay item))))))
           (itrs (map 'list #'gid (range 0 rank)))
           )
      ;; (10 1) + (1 10)がLowerできるかわからない
      (print (relay-read-iters (read-type-relay item)))
      itrs)))

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

(defun lower-node (node)
  (declare (type node node))
  
  )

;; [TODO] (0 10 1 nil)はNILに書き換える
;; schedule.lispwokousin sinaito ugokanai rei:
;;(with-no-grad
;;            (time (caten/codegen:jit (caten (!add (make-tensor `(3 3)) (!sin (make-tensor `(3))))))))

(defmethod lower-schedule-item ((node Node) (base-graph Graph))
  "Lowers the Schedule-Item into blueprint"
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
  ;; nothing to schedule
  (when (getattr node :allocate-p) (return-from lower-schedule-item))
  (let* ((graph (apply #'make-graph (getattr node :items)))
         (_ (setf (graph-outputs graph) (node-writes node)))
         (iterspace (get-grouped-dims graph))
         (gids (map 'list #'gid (range 0 (length (car iterspace)))))
         (blueprint
           `(,@(map 'list #'%make-for gids (car iterspace))
             ,@(map 'list #'%make-endfor gids)))
         (seen nil))
    (declare (ignore _))
    (fixup-graph-iteration-space graph iterspace base-graph)

    (labels ((explore (id &key (path-reduced nil) (parent nil) &aux (node (id->value graph id)))
               ;; Try to insert the node ID into the above of parent
               (when (null node) (return-from explore))
               (when (find id seen) (return-from explore))
               (push id seen)
               (if (null parent)
                   (progn
                     ;; Initialize an initial blueprint
                     
                     )
                   (progn

                     ))))
      (let ((p nil))
        (mapc #'(lambda (x) (explore x :parent p) (setf p x)) (graph-outputs graph))))
    (print blueprint)
    ;; scheduleする時に一緒にLoop Boundの推論した方がいい？
    ;; Later, inserting
    ;; 必要なもの
    ;; 1. ノードごとにIterationSpaceを定義する
    ;; 2. merge iterspace and iterspace
    ;; Lowerer
    ;; [TODO] BigLoopFirst?
    ;;  (dolist (i (getattr node :items))
    ;;    (print (item->iteration-space i)))
    ;; stride=1
    ;; That is: AST.shape
    ;; Algorithm
    ;; 1. ループの初期状態を作る (TODO)
    ;; <Insertable Point>
    ;; for (gid0)
    ;;  <Insetable Point>
    ;;  for (gid1)
    ;;   <Insertable Point>
    ;; }
    ;; <Insertable Point>
    ;;}
    ;;<Insertable Point>
    ;; 2. (1.)の初期状態にノードを配置していく
    ;; 3. Complete
    ;; Each node is Node(aref(*iters))
    ))

;; merge-dims
;; Stride=1 -> aref(array, Index-Component(*iters))
#|
LOAD: 10000
INDEX-COMPONENTS: 100 100 (0 1)

|#
