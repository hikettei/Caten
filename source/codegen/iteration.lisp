(defpackage #:caten/codegen/iteration
  (:use :cl :caten/aasm :caten/air :caten/aasm/expr :caten/runtime/buffer :caten/runtime/runtime)
  (:import-from :caten/codegen/helpers :permute-list)
  (:import-from :caten/common.dtype #:dtype-t #:dtype->lisp)
  ;; Relay Helpers
  (:export #:relay-write-iters #:relay-read-iters)
  ;; Viz
  ;; (:export #:inferred-type-vizualize-to-dot)
  (:export
   #:relay-reads #:relay-writes
   #:tensor-relay-merge-dims
   #:merge-dims
   #:Iteration-Space
   #:make-iteration-space
   #:Iteration-space-shape
   #:Iteration-space-strides
   #:Iteration-space-views
   #:Iteration-space-procedure
   #:%expr-const
   #:mergeable-view-p
   #:iteration-space-expr-aref
   #:tensor-relay-iteration-space
   #:ensure-iteration-space-length
   #:node-writes-broadcasted-p
   #:reveal-buffer))

(in-package :caten/codegen/iteration)
;; [TODO] Update caten/viz
;; ~~ Iteration Space Syntax Sugar ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun relay-write-iters (relay)
  (declare (type Relay relay))
  (map 'list #'tensor-relay-iterspace (relay-writes relay)))

(defun (setf relay-write-iters) (value relay)
  (declare (type Relay relay) (type list value))
  (assert (= (length value) (length (relay-writes relay))))
  (loop for w in (relay-writes relay)
        for v in value
        do (setf (tensor-relay-iterspace w) v)))

(defun relay-read-iters (relay)
  (declare (type Relay relay))
  (map 'list #'tensor-relay-iterspace (relay-reads relay)))

(defun (setf relay-read-iters) (value relay)
  (declare (type Relay relay) (type list value))
  (assert (= (length value) (length (relay-reads relay))))
  (loop for w in (relay-reads relay)
        for v in value
        do (setf (tensor-relay-iterspace w) v)))
;; ~~ Loop Collapse ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
;; ~~ Iteration Space ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct Iteration-Space
  "
Iteration-Space is a structure that contains the shape/stride/view information of the each buffer. It is used to render the kernel.
iteration-space-shape to get the shape, iteration-space-strides to get the stride, iteration-space-views to get the offsets and increments, and iteration-space-procedure to get how the iteraton-space is collapsed or permuted. Each elements for shape/stride are Expr.

`iteration-space-expr-aref` to render the aref.

```
(iteration-space-expr-aref iteration-space buffer gids)
```
gids corresponds for the loop idx in the kernel.
"
  (shape nil :type list)
  (strides nil :type list)
  (views nil :type list)
  (procedure nil :type list))

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

(defmethod ensure-iteration-space-length ((is Iteration-Space) gids)
  (let* ((rank (length (iteration-space-procedure is)))
         (pads (loop repeat (max 0 (- rank (length gids))) collect (expr-const 0 :int64))))
    (append gids pads)))

(defmethod ensure-iteration-space-length ((rank fixnum) gids)
  (let ((pads (loop repeat (max 0 (- rank (length gids))) collect (expr-const 0 :int64))))
    (append gids pads)))

(defun node-writes-broadcasted-p (node)
  (some #'(lambda (x) (and x (fourth x))) (tensor-relay-views (car (relay-writes (read-type-relay node))))))

