(defpackage :caten/codegen/type-relay
  (:documentation "Perform the following inference given the graph by direcly running it in :relay-checker VM.")
  (:use :cl :caten/ir/expr :caten/runtime/buffer :caten/runtime/runtime :caten/air)
  (:import-from :caten/codegen/helpers #:permute-list)
  (:import-from :caten/common.dtype #:dtype-t #:dtype->lisp)
  (:export
   #:RelayChecker #:RelayBuffer
   #:Inferred-Type #:make-inferred-type #:read-type-relay
   #:relay-reads #:relay-writes #:relay-read-iters #:relay-write-iters
   #:run-type-infer #:buffer-merge-dims #:merge-dims #:reveal-buffer
   #:Iteration-Space
   #:make-iteration-space
   #:Iteration-space-shape
   #:Iteration-space-strides
   #:Iteration-space-views
   #:Iteration-space-procedure
   #:mergeable-view-p
   #:iteration-space-expr-aref
   #:buffer-iteration-space #:ensure-iteration-space-length
   #:expr-infer-type
   #:inferred-type-vizualize-to-dot
   #:node-writes-broadcasted-p
   #:buffer-inferred-permute
   #:buffer-orig-buffer-shape
   #:buffer-depend-idx-list
   #:%expr-const))

(in-package :caten/codegen/type-relay)
;; ~~ TypeReporter ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defstruct (Type-Reporter
	    (:conc-name rp-)
	    (:constructor make-type-reporter ()))
  (id2buffer (make-hash-table :test #'eql))
  (seen nil :type list))

(defun map/type-of (type-reporter id)
  ;; Return: Buffer or number
  (declare (type type-reporter type-reporter)
	   (type (or number symbol) id))
  (if (numberp id)
      id
      (or (gethash id (rp-id2buffer type-reporter)) (error "map/type-of: ~a cannot be inferred from the graph" id))))
;; ~~ RelayChecker ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defclass RelayChecker (GraphRuntime) ((type-reporter :initarg :type-reporter :accessor runtime-type-reporter)))

(defclass RelayBuffer (AbstractBuffer)
  ((inferred-permute :accessor buffer-inferred-permute :initform nil)
   (orig-buffer-shape :accessor buffer-orig-buffer-shape :initform nil)
   (depend-idx-list :accessor buffer-depend-idx-list :initform nil))
  (:documentation "A buffer just used to report the shape/type/strides ... of buffer during jit compilation"))

(defmethod open-buffer ((runtime RelayChecker) buffer) buffer)
(defmethod close-buffer ((runtime RelayChecker) buffer) buffer)

(defun reveal-buffer (object)
  (if (buffer-p object)
      (if (null (buffer-shape object))
          (or (buffer-value object) object)
          object)
      object))

(defun merge-with-initial-value (node-reads realized-args)
  (assert (= (length node-reads) (length realized-args)))
  (loop for nr in node-reads
        for rr in realized-args
        if (and (buffer-p rr) (buffer-value rr) (= 0 (buffer-nrank rr)))
          collect (buffer-value rr)
        else
          collect nr))

(defun propagate-inference (args)
  ;; [TODO] The position of output should be :nth-out, not car.
  (assert (buffer-p (car args)) () "propagate-inference: Failed to infer the node-write from ~a.~%Hint: Do not propagate constants without providing :_type_relay." args)
  (let ((buff
          (make-buffer (buffer-shape (car args)) (buffer-stride (car args)) (buffer-dtype (car args)) (buffer-views (car args)) :device 'RelayBuffer)))
    (setf (buffer-inferred-permute buff) (buffer-inferred-permute (car args))
	  (buffer-orig-buffer-shape buff) (buffer-orig-buffer-shape (car args)))
    buff))
;; ~~ Realizes ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defparameter *special-nodes* `(:Allocate :View :Load :Where :AREF :SPACE))
(defmethod realize-node :around (node-id (runtime RelayChecker) node args)
  (when (getattr node :_type_relay :allow-undefined t)
    (loop for n in (node-writes node)
          for o in (relay-writes (read-type-relay node))
          do (setf (gethash n (rp-id2buffer (runtime-type-reporter runtime))) o))
    (return-from realize-node (apply #'values (relay-writes (read-type-relay node)))))
  (let ((out (multiple-value-list (if (find node-id *special-nodes*) (call-next-method) (propagate-inference args)))))
    (loop for n in (node-writes node)
          for o in out
          do (assert (buffer-p o) () "relay-checker: ~a should return a buffer" node)
             (setf (gethash n (rp-id2buffer (runtime-type-reporter runtime))) o))
    (apply #'values out)))

(defmethod realize-node ((node-id (eql :Allocate)) (runtime RelayChecker) node args)
  (multiple-value-bind (shape stride) (parse-allocate-node node (merge-with-initial-value (node-reads node) args))
    (make-buffer shape stride (getattr node :dtype) nil :device 'RelayBuffer)))

(defmethod realize-node ((node-id (eql :View)) (runtime RelayChecker) node args)
  (multiple-value-bind (shape v1 v2 v3 stride bc) (parse-view-node node (merge-with-initial-value (node-reads node) args))
    (let ((buffer (copy-buffer (car args))))
      (setf (buffer-shape buffer) shape
	    (buffer-stride buffer) stride
	    (buffer-views buffer)
	    (loop for i upfrom 0 below (length v1)
		  collect (list (nth i v1) (nth i v2) (nth i v3) (nth i bc)))
	    (buffer-nrank buffer) (length shape)
	    (buffer-inferred-permute buffer) (getattr node :permute)
	    (buffer-orig-buffer-shape buffer) (or (buffer-orig-buffer-shape (car args)) (buffer-shape (car args))))
      buffer)))

(defmethod realize-node ((node-id (eql :AREF)) (runtime RelayChecker) node args)
  (make-buffer nil nil (buffer-dtype (getattr node :buffer)) nil :device 'RelayBuffer))

(defmethod realize-node ((node-id (eql :SPACE)) (runtime RelayChecker) node args)
  (make-buffer nil nil (getattr node :dtype) nil :device 'RelayBuffer))

(defmethod realize-node ((node-id (eql :Load)) (runtime RelayChecker) node args)
  (let* ((tgt (car args)) (val (getattr node :value)) (out (copy-buffer tgt)))
    (when (or (numberp val) (symbolp val))
      (setf (buffer-value out) val))
    out))

(defmethod realize-node ((node-id (eql :Where)) (runtime RelayChecker) node args) (copy-buffer (second args)))
;; ~~ Entry Points ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(declaim (ftype (function (GraphRuntime &key (:allow-overwrite boolean)) Type-Reporter) run-type-infer))
(defun run-type-infer (runtime &key (allow-overwrite nil) &aux (graph (runtime-graph runtime)))
  "
```
(run-type-infer runtime)
```
Runs the shape inference to the given GraphRuntime, returning `Type-Reporter`."
  (ctx:with-contextvar (:PROFILE 0)
    (let ((*supress-allocate-mode* t)
          (type-reporter (make-type-reporter))
          (runtime (make-runtime graph :runtime 'RelayChecker :buffer-type 'RelayBuffer)))
      (setf (runtime-type-reporter runtime) type-reporter)
      (runtime-forward runtime)
      ;; Infer type for PAUSE/BACKWARD
      (let ((pause/bw (nth (runtime-pc runtime) (graph-nodes graph))))
        (when (and pause/bw (eql (node-type pause/bw) :PAUSE/BACKWARD))
	  (loop for r in (node-reads pause/bw)
	        for w in (node-writes pause/bw)
	        do (setf (gethash w (rp-id2buffer type-reporter)) (runtime-getvar runtime r)))))
      (runtime-backward runtime)
      (deploy-type-infer-results graph type-reporter :allow-overwrite allow-overwrite)
      type-reporter)))

(defstruct (Inferred-Type
	    (:conc-name relay-)
	    (:constructor make-inferred-type (reads writes)))
  "A structure `Inferred-Type` contains the shape/stride/view information of the node at each points. Also, it containts the iteration space information that is used to render the kernel.

If the shape inference is successfully done and properly deployed to the target graph by the `rewriting-rule`, the function `(read-type-relay node)` will return the `Inferred-Type` structure.

- relay-reads returns the list of the buffer corresponding to the node-reads. (If node-reads a number, nil is set)
- relay-writes returns the list of the buffer corresponding to the node-writes.
- relay-read-iters returns the list of the iteration space corresponding to the node-reads.
- relay-write-iters returns the list of the iteration space corresponding to the node-writes."
  (reads reads :type list)
  (read-iters nil :type list)
  (writes writes :type list)
  (write-iters nil :type list))

(defmethod print-object ((type Inferred-type) stream)
  (print-unreadable-object (type stream :type t)
    (let ((reads
            (map 'list #'buffer-shape
                 (loop for r in (relay-reads type)
                       if r collect r)))
          (writes
            (map 'list #'buffer-shape
                 (loop for w in (relay-writes type)
                       if w collect w))))
      (format stream "~a <- ~a" writes reads))))

(defmethod inferred-type-vizualize-to-dot ((type Inferred-type))
  (with-output-to-string (out)
    (format out "|[INFERRED_TYPE]|SHAPE: ")
    (loop for w in (relay-writes type)
          if w
            do (format out "~a" (buffer-shape w)))
    (format out "←")
    (loop for r in (relay-reads type)
          if r
            do (format out "~a " (buffer-shape r)))
    (format out "|STRIDE: ")
    (loop for w in (relay-writes type)
          if w
            do (format out "~a" (buffer-stride w)))
    (format out "←")
    (loop for r in (relay-reads type)
          if r
            do (format out "~a " (buffer-stride r)))
    (format out "|VIEW: ")
    (loop for w in (relay-writes type)
          if w
            do (format out "~a" (buffer-views w)))
    (format out "←")
    (loop for r in (relay-reads type)
          if r
            do (format out "~a " (buffer-views r)))))

(defun read-type-relay (node)
  (declare (type node node))
  (or (getattr node :_type_relay) (error "Failed to infer the type of ~a" node)))

(defun deploy-type-infer-results (graph type-map &key (allow-overwrite nil))
  "Writes the result of type-infer to :_type_relay"
  (flet ((->type (id) (when (symbolp id) (map/type-of type-map id))))
    (loop for n in (graph-nodes graph) do
      (let ((type (make-inferred-type
		   (map 'list #'->type (node-reads n))
		   (map 'list #'->type (node-writes n)))))
	(when (null allow-overwrite)
	  (assert (null (getattr n :_type_relay :allow-undefined t)) () ":_type_relay should be a nil!~%%safely-purge-views-from-graph was previously applied?~%- do not override the attr :_type_relay."))
	(when (null (getattr n :_type_relay :allow-undefined t))
          (when (subtypep (class-of (caten/air:node-attr n)) 'caten/ir:JITAble)
	    (setf (getattr n :_type_relay) type)))))))
;; ~~ Loop Collapse ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun mergeable-view-p (g view shape &aux (shape (if (typep shape 'Expr) shape (expr-const (reveal-buffer shape) :int64))))
  "Mergeable axis = view is not created."
  (when (null view) (return-from mergeable-view-p t))
  (when (expr-equal-to shape 1) (return-from mergeable-view-p (fourth view))) ;; Always collapse one as long as they are broadcasted.
  (trivia:ematch view
    ((list (eql 0) (trivia:guard x (expr-scalar-equivalent-p (expr-const x :int64) shape)) (eql 1) _) t)
    ;; considering the case: X = |val_15|, shape=a*b (a little heavy, so separated)
    ((list (eql 0) (trivia:guard x (expr-scalar-equivalent-p (%expr-const g x :int64) shape)) (eql 1) _) t)
    (_ nil)))

(defun gather-only-scalars (nodes)
  (loop for n in nodes
        if (and (= 0 (buffer-nrank (car (relay-writes (read-type-relay n))))))
          collect n))

(defun %expr-const (graph value dtype)
  (let* ((val (reveal-buffer value)))
    (if (or (numberp val) (null (id->value graph val)))
        (expr-const val dtype)
        ;; Merge only scalar path!
        (expr-from-graph val (apply #'caten/air:make-graph (gather-only-scalars (graph-nodes graph)))))))

(defun expr-infer-type (expr)
  "Running TypeRelay Inference for the expr graph."
  (declare (type expr expr))
  (assert (caten/air:graph-outputs (expr-graph expr)))
  ;; Sort the graph by execution order
  (setf (expr-graph expr) (caten/air:->fast-graph (expr-graph expr))
        (expr-graph expr) (caten/air:->graph-with-tpsort (expr-graph expr)))
  (run-type-infer (make-runtime (expr-graph expr) :fw-outputs (node-writes (expr-out expr)) :runtime 'RelayChecker) :allow-overwrite t)
  expr)

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

(defmethod iteration-space-expr-aref ((is Iteration-Space) (type RelayBuffer) gids)
  "Returns a list of EXPR which (reduce #'+ ...) represents for the index."
  (assert (not (= (buffer-nrank type) -1)) () "buffer-nrank = -1 means the array was mutated to scalar!")
  (let ((size (iteration-space-shape is))
        (stride (iteration-space-strides is))
        (view (iteration-space-views is)))
    (assert (= (length gids) (length size)) () "The iteration space and the buffer should have the same rank, getting gids=~a~%~a" gids is)
    (flet ((maybe-expr-const (x) (if (numberp x) (expr-const x :int64) x)))
      (loop for s in stride
            for nth upfrom 0
            for i in gids
            for v = (nth nth view)
            if v
              collect (expr-mul (maybe-expr-const s) (expr-add (maybe-expr-const (car v)) (expr-mul (maybe-expr-const (third v)) (maybe-expr-const i))))
            else
              collect (expr-mul (maybe-expr-const i) (maybe-expr-const s))))))

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

(defmethod buffer-merge-dims ((graph Graph) (buffer RelayBuffer))
  (let ((viewed-shape (buffer-shape buffer))
        (strides (buffer-stride buffer))
        (views (buffer-views buffer)))
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
      (loop repeat (buffer-nrank buffer) collect nil)))))

(defmethod buffer-iteration-space ((graph Graph) (buffer RelayBuffer))
  (let ((viewed-shape (buffer-shape buffer))
        (strides      (buffer-stride buffer))
        (views        (buffer-views buffer)))
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
      (loop repeat (buffer-nrank buffer) collect nil))
     :no-collapse t)))

(defun node-writes-broadcasted-p (node)
  (some #'(lambda (x) (and x (fourth x))) (buffer-views (car (relay-writes (read-type-relay node))))))
