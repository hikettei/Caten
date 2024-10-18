(defpackage :caten/codegen/blueprint
  (:use :cl :caten/air :caten/codegen/expr :alexandria)
  (:import-from
   :caten/codegen/shape-inference
   #:read-type-relay
   #:relay-read-iters
   #:relay-write-iters
   #:iteration-space-shape)
  (:import-from
   :caten/codegen/helpers
   :gid
   :range)
  (:export
   #:lower-schedule-item))

(in-package :caten/codegen/blueprint)

(defun %make-for (idx size)
  (make-node :Render :FOR nil nil :idx idx
             :upfrom (expr-const 0 :int64)
             :below (expr-< (expr-const idx :int64) (expr-const size :int64))
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

(defun lower-node (node)
  (declare (type node node))
  
  )
;; ittann FOR/ENDFOR/Printを作って考える
(defmethod lower-schedule-item ((node Node))
  "Lowers the Schedule-Item into blueprint"
  (assert (eql (node-type node) :Schedule-Item) () "node is not an Schedule-Item, getting ~a" node)
  ;; nothing to schedule
  (when (getattr node :allocate-p) (return-from lower-schedule-item))
  (let* ((graph (apply #'make-graph (getattr node :items)))
         ;; そもそもShapeの推定ができてない。。。(ConvNDはH/Cでそれ以上のループを作るはず)
         ;; might be ok?
         ;; TensorComprehensionのLowererのイメージ
         (iterators (map 'list #'gid (range 0 (get-kernel-rank node)))))
    (setf (graph-outputs graph) (node-writes node))
    (flet ((s ()
             
             ))
      ;; 必要なもの
      ;; 1. ノードごとにIterationSpaceを定義する
      ;; 2. merge iterspace and iterspace
      ;; Lowerer
      ;; 数値で比較するようなアルゴリズムは一切実装しない！(1==x)
      (print "+++++")
      (print iterators)
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
      )))

;; merge-dims
;; Stride=1 -> aref(array, Index-Component(*iters))
#|
LOAD: 10000
INDEX-COMPONENTS: 100 100 (0 1)

|#
