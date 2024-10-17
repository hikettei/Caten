(defpackage #:caten/codegen/shape-inference
  (:documentation "Perform the following inference given the graph by direcly running it in :relay-checker VM.
- Shape/View/Stride Information
- (Plus) Permute information.
   - The idea of permute and permutation inference is an afterthought.
     - because there were no plans to implement Permute in the initial JIT.
     - The initial JIT implementation plan was to treat all internal Tensors as one-dimensional, but now they are multidimensional in order to express the accesing relations in an affine function for symbolic compilation.
     - This is a trade-off, Symbolic Compilation is possible instead of being able to hack the stride by rewriting it to whatever value you want. Functions that should be implemented with the former should be implemented by introducing another API.
   - Plus, we refer to permute as:
     - How the original shape/view/stride was shuffled to obtain the current shape/view/stride.
     - Therefore, when shuffling the shape/stride/view in the aIR level, you must to add :permute attribute in the VIEW node.
     - We have no plan for refactoring this, as permute inference is a still simple solution, and arrays are one-dimensional anyway when rendering.")
  (:use :cl :caten/codegen/expr)
  (:import-from
   :caten/common.dtype
   #:dtype-t
   #:dtype->lisp)
  (:import-from
   :caten/avm
   #:Buffer
   #:make-buffer
   #:buffer-shape
   #:buffer-stride
   #:buffer-dtype
   #:buffer-nrank
   #:buffer-views
   #:buffer-value
   #:buffer-inferred-permute
   #:buffer-orig-buffer-shape
   #:buffer-p
   #:parse-allocate-node
   #:parse-view-node
   #:realize-buffer
   #:copy-buffer

   #:AVM
   #:*device*
   #:vm/forward
   #:vm/backward
   #:avm-graph
   #:avm-pc
   #:vm/readvar
   #:avm-tape-length
   #:%impl
   #:%vm/allocate-buffer)
  (:import-from
   :caten/air
   #:Graph
   #:Node
   #:id->value
   #:node->id
   #:node-reads
   #:node-writes
   #:getattr
   #:graph-nodes
   #:node-type)
  (:export
   #:Inferred-Type
   #:make-inferred-type
   #:read-type-relay
   #:run-type-infer
   #:buffer-merge-dims
   #:merge-dims)
  (:export
   #:Iteration-Space
   #:Iteration-Space-shape
   #:Iteration-Shape-strides
   #:Iteration-shape-view
   ))

(in-package :caten/codegen/shape-inference)

(defparameter *type-reporter* nil)

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

(defstruct (FakeArray
	    (:constructor make-fakearray (shape dtype initial-element)))
  (shape shape :type list)
  (dtype dtype :type dtype-t)
  (initial-element initial-element))

(defun reveal-buffer (object)
  "Extracts the initial-value from the nested buffer/fake-array"
  (declare (type (or buffer fakearray integer symbol string) object))
  (if (stringp object)
      object
      (if (buffer-p object)
	  (if (fakearray-p (buffer-value object))
	      (fakearray-initial-element (buffer-value object))
	      (buffer-value object))
	  (if (fakearray-p object)
	      (fakearray-initial-element object)
	      object))))

(defmethod %vm/allocate-buffer ((device-id (eql :relay-checker)) buffer)
  (let ((initial-value (if (eql (buffer-dtype buffer) :bool)
			   nil
			   (coerce 0 (dtype->lisp (buffer-dtype buffer))))))
    (if (= (buffer-nrank buffer) 0)
	(setf (buffer-value buffer) (make-fakearray nil (buffer-dtype buffer) initial-value))
	(setf (buffer-value buffer) (make-fakearray (buffer-shape buffer) (buffer-dtype buffer) initial-value)))
    buffer))

(defmethod %impl :around ((device-id (eql :relay-checker)) op graph node args)
  (let ((out-buffer (multiple-value-list (if (next-method-p) (call-next-method) (car args)))))
    (when *type-reporter*
      (loop for n in (node-writes node)
	    for o in out-buffer
	    do (assert (and (buffer-p o) (fakearray-p (buffer-value o)))
		       ()
		       "relay-checker: ~a should return a buffer whose value is fake-array, but got ~a" op o)
	       (setf (gethash n (rp-id2buffer *type-reporter*)) o)))
    (apply #'values out-buffer)))

(defmethod %impl ((device-id (eql :relay-checker)) op graph node args)
  (if (next-method-p)
      (call-next-method)
      (let ((buff (make-buffer (buffer-nrank (car args)) (buffer-shape (car args)) (buffer-stride (car args)) (buffer-dtype (car args)) (buffer-views (car args)))))
	(setf (buffer-value buff) (make-fakearray (buffer-shape buff) (buffer-dtype buff) (car (node-writes node)))
	      (buffer-inferred-permute buff) (buffer-inferred-permute (car args)) ;; (buffer-inferred-permute buff)
	      (buffer-orig-buffer-shape buff) (buffer-orig-buffer-shape (car args)))
	buff)))

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :Allocate)) graph node args)
  (multiple-value-bind (shape stride) (parse-allocate-node node args)
    (realize-buffer graph (node->id node) :shape1 shape :stride1 stride)))

;; The same algorithm in function.lisp (class Permute)
(defun permute-list (order list)
  (assert (= (length order) (length list)) () "cannot shuffle ~a and ~a" order list)
  (loop for nth in order collect (nth nth list)))

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :view)) graph node args)
  (multiple-value-bind (shape v1 v2 v3 stride bc)
      (parse-view-node node args)
    (let ((buffer (copy-buffer (car args))))
      (setf (buffer-shape buffer) (map 'list #'reveal-buffer shape)
	    (buffer-stride buffer) (map 'list #'reveal-buffer stride)
	    (buffer-views buffer)
	    (loop for i upfrom 0 below (length v1)
		  collect (list (reveal-buffer (nth i v1)) (reveal-buffer (nth i v2)) (reveal-buffer (nth i v3)) (nth i bc)))
	    (buffer-nrank buffer) (length shape)
	    (buffer-inferred-permute buffer) (if (and (buffer-inferred-permute buffer) (getattr node :permute))
						 (permute-list (getattr node :permute) (buffer-inferred-permute buffer))
						 (or
						  (buffer-inferred-permute buffer)
						  (getattr node :permute)))
	    (buffer-orig-buffer-shape buffer) (map 'list #'reveal-buffer (or (buffer-orig-buffer-shape (car args)) (buffer-shape (car args)))))
      buffer)))

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :Load)) graph node args)
  (let* ((tgt (car args))
	 (val (getattr node :value)))
    (let ((out (copy-buffer tgt)))
      (setf (buffer-value out) (make-fakearray nil (buffer-dtype out) val))
      out)))

(defmethod %impl ((device-id (eql :relay-checker)) (op (eql :WHERE)) graph node args)
  (let ((buff (copy-buffer (second args))))
    (setf (buffer-value buff) (make-fakearray (buffer-shape buff) (buffer-dtype buff) (car (node-writes node))))
    buff))

(declaim (ftype (function (AVM) Type-Reporter) run-type-infer))
(defun run-type-infer (avm)
  "Run the shape-inference given AVM, returning Type-Reporter"
  (declare (type avm avm))
  (let ((*device* :relay-checker) (*type-reporter* (make-type-reporter)))
    (setf (avm-tape-length avm) (length (graph-nodes (avm-graph avm))))
    (vm/forward avm)
    ;; Infer type for PAUSE/BACKWARD
    (let ((pause/bw (nth (avm-pc avm) (graph-nodes (avm-graph avm)))))
      (when (and pause/bw (eql (node-type pause/bw) :PAUSE/BACKWARD))
	(loop for r in (node-reads pause/bw)
	      for w in (node-writes pause/bw)
	      do (setf (gethash w (rp-id2buffer *type-reporter*)) (vm/readvar avm r)))))
    (vm/backward avm)
    (deploy-type-infer-results avm *type-reporter*)
    *type-reporter*))

(defstruct (Inferred-Type
	    (:conc-name relay-)
	    (:constructor make-inferred-type (reads writes)))
  (reads reads :type list)
  (writes writes :type list))

(defmethod print-object ((type Inferred-type) stream)
  (print-unreadable-object (type stream :type t)
    (let ((ranks
            (append
             (map 'list #'buffer-shape
                  (loop for r in (relay-reads type)
                        if r collect r))
             (map 'list #'buffer-shape
                  (loop for w in (relay-writes type)
                        if w collect w)))))
      (format stream "~a" (map 'list #'reveal-buffer (car (sort ranks #'> :key #'length)))))))

(defun read-type-relay (node)
  (declare (type node node))
  (or (getattr node :_type_relay) (error "Failed to infer the type of ~a" node)))

(defun deploy-type-infer-results (avm type-map &key (allow-overwrite nil))
  "Writes the result of type-infer to :_type_relay"
  (flet ((->type (id) (when (symbolp id) (map/type-of type-map id))))
    (loop for n in (graph-nodes (avm-graph avm)) do
      (let ((type (make-inferred-type
		   (map 'list #'->type (node-reads n))
		   (map 'list #'->type (node-writes n)))))
	(when (null allow-overwrite)
	  (assert (null (getattr n :_type_relay :allow-undefined t)) () ":_type_relay should be a nil!~%%safely-purge-views-from-graph was previously applied?~%- do not override the attr :_type_relay."))
	(when (null (getattr n :_type_relay :allow-undefined t))
	  (setf (getattr n :_type_relay) type))))))
;; ~~ Loop Collase ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(defun mergeable-view-p (view shape &aux (shape (reveal-buffer shape)))
  "Mergeable axis = view is not created."
  (when (null view) (return-from mergeable-view-p t))
  (trivia:ematch view
    ;; antyhing for broadcast, because the strides of broadcasted axes are replaced w/ 0
    ((list (eql 0) (eql shape) (eql 1) _) t)
    (_ nil)))

(defun %expr-const (graph value dtype)
  (let ((val (reveal-buffer value)))
    (if (or (numberp val) (null (id->value graph val)))
        (expr-const val dtype)
        (expr-from-graph val graph))))

(defstruct Iteration-Space
  (shape nil :type list)
  (strides nil :type list)
  (views nil :type list))

;; [TODO] render-isl ((is Iteration-Space))

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

;; [TODO] もうちょっと真面目に考えた方がいい。
;; - Broadcastの扱い？
;; - is-view?
(defun merge-dims (g shape strides views)
  (when (null shape) (return-from merge-dims))
  (assert (= (length shape) (length strides) (length views)))
  ;; ret = (list new-shapes new-strides new-views)
  (let ((ret (list
              (list
               (%expr-const g (nth 0 shape) :int64)
               (%expr-const g (nth 0 strides) :int64)
               (nth 0 views)))))
    (loop for nth upfrom 1 below (length shape)
          for size = (nth nth shape)
          for stride = (nth nth strides)
          for view = (nth nth views) do
            (multiple-value-bind (last-size last-stride) (apply #'values (car (last ret)))
              (when (not (eql size 1)) ;; always merge 1
                (if (and
                     (mergeable-view-p view size)
                     (expr-scalar-equivalent-p
                      last-stride
                      (expr-mul (%expr-const g size :int64) (%expr-const g stride :int64))))
                    (setf (nth (1- (length ret)) ret)
                          (list (expr-mul last-size (%expr-const g size :int64)) (%expr-const g stride :int64) nil))
                    (setf ret
                          (append
                           ret
                           (list (list (%expr-const g size :int64) (%expr-const g stride :int64) view))))))))
    (iteration-space-sync-broadcast
     (make-iteration-space
      :shape
      (loop for s in ret collect (first s))
      :strides
      (loop for s in ret collect (second s))
      :views
      (loop for s in ret collect (third s))))))

(defmethod buffer-merge-dims ((graph Graph) (buffer Buffer))
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
      (when (every #'identity views) views)
      (loop repeat (buffer-nrank buffer) collect nil)))))
